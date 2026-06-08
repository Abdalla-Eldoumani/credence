{-# LANGUAGE BangPatterns #-}

{- | Trace-based single-site Metropolis-Hastings: the second interpreter of the
'Model' monad, and the first whose correctness is not evident from its structure.

Importance sampling draws from the prior and reweights; nothing about it can be
subtly wrong once the weights stay in log-space. MH is different. It proposes a
change to one random choice, recomputes the program, and accepts or rejects by a
ratio that is easy to get quietly wrong, and a wrong ratio yields a smooth,
plausible, incorrect posterior. So this module implements the acceptance ratio in
its general form from the primary source and leaves the proof to the oracles:
detailed balance against a known stationary distribution, and conjugate agreement
with the analytic Beta posterior (both in "test/Spec.hs").

== The method

This is the database-driven single-site MCMC of Wingate, Stuhlmuller, and Goodman,
"Lightweight Implementations of Probabilistic Programming Languages Via
Transformational Compilation," AISTATS 2011, Section 3. The current state is a
'Trace': the database of random choices addressed by execution position
("Credence.Trace"). One step:

1. Pick one address uniformly among the trace's choices.
2. Resample that single choice from its prior (its 'entryResample' action), the
   proposal kernel for the perturbed site.
3. Recompute the program with that one choice swapped and every other held, via
   'replayModel', producing the proposed trace and its return value.
4. Accept or reject by the general-form Metropolis-Hastings ratio (below), in
   log-space: draw @u@ uniform on (0,1) and accept iff @log u < logRatio@.

A run returns the chain of return values (so a projection such as @theta@ can be
summarized) and the measured acceptance rate (accepted steps over proposed steps),
which the tests assert lies strictly in (0,1): a chain that never moves or always
moves is a bug even if a moment happens to match.

== The acceptance ratio (general form, derived not remembered)

Wingate et al. give the Metropolis-Hastings acceptance probability for a move from
trace @x@ to proposed trace @x'@ as

    A(x -> x') = min(1, [ p(x') / p(x) ] * [ q(x | x') / q(x' | x) ])

where @p@ is the trace's unnormalized probability and @q(. | .)@ is the proposal
density. This module computes @log A@ from that expression directly, summing the
full prior and the full likelihood of each trace rather than asserting any reduced
formula; the single-site cancellations are left to happen numerically so detailed
balance can arbitrate.

The trace probability factors, in log-space, into the sum of every choice's prior
log-density plus the run's observe log-weight:

    log p(x) = (sum over addresses a of logPrior_a(x))  +  logLik(x)

Both summands come straight off the trace: 'traceTotalLogPrior' and
'traceLogWeight'. So

    log p(x') - log p(x)
      = [ traceTotalLogPrior x' - traceTotalLogPrior x ]
      + [ traceLogWeight x'      - traceLogWeight x      ]

The proposal is single-site resampling from the prior with a uniform choice of
which site to perturb. Writing @a*@ for the perturbed address, @|x|@ for the number
of choices in @x@, and @f_{a*}@ for the prior of the choice at @a*@:

    q(x' | x) = (1 / |x|)  * f_{a*}(value'_{a*})
    q(x  | x') = (1 / |x'|) * f_{a*}(value_{a*})

because the forward move chooses @a*@ with probability @1/|x|@ then draws the new
value from its prior, and the reverse move chooses @a*@ with probability @1/|x'|@
then would draw the old value from that same prior. Hence

    log q(x | x') - log q(x' | x)
      = [ -log |x'| + logPrior_{a*}(old value) ]
      - [ -log |x|  + logPrior_{a*}(new value) ]

This is the proposal-correction term. For a fixed-structure model @|x| = |x'|@ and
the structure-wide prior sums share every unperturbed address, so most of this
cancels against the prior difference and the move reduces to a likelihood ratio;
but that reduction is DERIVED here by computing the full expression, not assumed.
A model with @|x| /= |x'|@ would still be scored correctly by the same code,
though MH is scoped to fixed-structure models.

Everything is in log-space: the log-ratio is compared against @log u@, and no raw
log-weight is ever exponentiated.

The interpreter is generator-polymorphic over 'StatefulGen', so the tests drive it
in @ST@ under a fixed seed and the engine can drive it in @IO@.
-}
module Credence.Inference.MH (
    mhStep,
    mhChain,
) where

import Control.Monad (foldM)
import Credence.Model (Model)
import Credence.Trace (
    Entry (..),
    Trace (..),
    replayModel,
    traceAddresses,
    traceLogWeight,
    traceModel,
    traceSize,
    traceTotalLogPrior,
 )
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import System.Random.Stateful (StatefulGen, uniformRM)

{- | One Metropolis-Hastings step over a model and its current @(trace, value)@.

Picks one address uniformly, resamples that site from its prior, replays the model
with the swap to get the proposed @(value', trace')@, forms the general-form log
acceptance ratio, and accepts iff @log u < logRatio@ for @u@ uniform on (0,1).
Returns the (possibly unchanged) state and whether the proposal was accepted, so
the caller can tally the acceptance rate.

If the trace has no choices there is nothing to perturb; the step is a no-op
reported as a reject. (No model MH is run on is choice-free, but this keeps the
step total.)
-}
mhStep ::
    (StatefulGen g m) =>
    Model a ->
    (Trace, a) ->
    g ->
    m ((Trace, a), Bool)
mhStep model (current, currentVal) gen =
    case traceAddresses current of
        [] -> pure ((current, currentVal), False)
        addrs -> do
            -- Choose one site uniformly among the current trace's choices.
            i <- uniformRM (0, length addrs - 1) gen
            let target = addrs !! i
            -- Resample exactly that site from its prior; capture the new value's
            -- prior log-density (the forward proposal's density at the new value).
            (proposedEntry, newLogPrior) <- resampleSite (currentChoiceAt target) gen
            let
                -- The proposed trace is the current database with the one site
                -- swapped; replay recomputes the program around the swap, rescoring
                -- every reused choice and re-summing the likelihood for this run.
                proposedSeed = current{traceChoices = Map.insert target proposedEntry (traceChoices current)}
            (proposedVal, proposed) <- replayModel model proposedSeed gen
            let
                -- Trace-probability difference, in log-space: full prior sums and
                -- full observe weights off each trace (no hand-reduced formula).
                logProbDiff =
                    (traceTotalLogPrior proposed - traceTotalLogPrior current)
                        + (traceLogWeight proposed - traceLogWeight current)
                -- Proposal correction for single-site prior-proposal with a uniform
                -- choice of site: q(x'|x) = (1/|x|) f(new), q(x|x') = (1/|x'|) f(old).
                oldLogPrior = entryLogPrior (currentChoiceAt target)
                logProposalDiff =
                    (negate (log (fromIntegral (traceSize proposed))) + oldLogPrior)
                        - (negate (log (fromIntegral (traceSize current))) + newLogPrior)
                logRatio = logProbDiff + logProposalDiff
            -- Accept in log-space: log u < logRatio. min(1, .) is implicit, since
            -- log u <= 0 always accepts when logRatio >= 0.
            u <- uniformRM (0, 1) gen
            pure $
                if log u < logRatio
                    then ((proposed, proposedVal), True)
                    else ((current, currentVal), False)
  where
    -- The entry at an address known to be present (it came from traceAddresses).
    currentChoiceAt a = traceChoices current Map.! a

{- | Resample one site from its prior, returning the rebuilt entry and the new
value's prior log-density.

The entry's 'entryResample' redraws from the same 'Dist' the site was drawn from
and hands back @(value', logDensityDist d value')@. The new value is repacked into
an 'Entry' that carries that prior log-density and the same resampler, so the
proposed trace is a well-formed database; the returned 'Double' is the forward
proposal's density at the new value, needed for the acceptance ratio's correction
term.
-}
resampleSite :: (StatefulGen g m) => Entry -> g -> m (Entry, Double)
resampleSite (Entry _ _ resample) gen = do
    (value', logPrior') <- resample gen
    pure
        ( Entry{entryValue = value', entryLogPrior = logPrior', entryResample = resample}
        , logPrior'
        )

{- | Run @n@ Metropolis-Hastings steps and return the chain of return values and
the acceptance rate.

Initializes the chain by a forward run under 'traceModel' (drawing every choice
from its prior, the same start importance sampling would use), then folds 'mhStep'
@n@ times, threading the generator and tallying accepts. The result is the
@n@-element vector of return values visited (one per step, including repeats on
rejected steps, so the empirical distribution over the chain is the MH estimate)
paired with @accepts / n@, the measured acceptance rate. The tests assert that
rate is strictly in (0,1).

Generator-polymorphic, so a fixed @ST@ seed makes a run reproducible.
-}
mhChain ::
    (StatefulGen g m) =>
    Int ->
    Model a ->
    g ->
    m (V.Vector a, Double)
mhChain n model gen = do
    -- Initialize from a forward run: draw every choice from its prior. traceModel
    -- returns (value, trace); the step state is (trace, value), so swap.
    (initialVal, initialTrace) <- traceModel model gen
    -- Fold n steps, collecting visited values (reversed) and counting accepts.
    (_, visitedRev, accepts) <- foldM step ((initialTrace, initialVal), [], 0 :: Int) [1 .. n]
    let chain = V.fromList (reverse visitedRev)
        rate =
            if n > 0
                then fromIntegral accepts / fromIntegral n
                else 0
    pure (chain, rate)
  where
    step (state, visited, !acc) _ = do
        (state'@(_, val), accepted) <- mhStep model state gen
        let acc' = if accepted then acc + 1 else acc
        pure (state', val : visited, acc')
