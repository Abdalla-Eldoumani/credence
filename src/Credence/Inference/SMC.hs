{-# LANGUAGE BangPatterns #-}

{- | Sequential Monte Carlo: a particle filter, the third interpreter, this time
of a state-space model rather than of the 'Credence.Model.Model' monad directly.

Importance sampling draws one weighted sample of a whole-program posterior; MH
walks a single chain through it. A particle filter is for a different shape of
problem: a latent state that evolves in time, observed noisily at each step, where
the target is the filtering distribution @p(x_t | y_1..y_t)@ updated as each
observation arrives. It carries a population of weighted particles forward, and
the one operation that makes it more than parallel importance sampling is
resampling: when the weight concentrates on a few particles the rest are dead
weight, so the population is periodically resampled in proportion to weight and the
weights reset to uniform, refocusing computation on the live region of state space.

A state-space model is expressed entirely through the existing distribution
library, so the numerics stay in one place. A 'StateSpace' is three pieces: a prior
'Dist' for the initial state, a transition kernel @s -> Dist s@ for the propagation,
and a scalar observation log-likelihood @s -> Double -> Double@. No new sampling or
density machinery is introduced; the filter only sequences 'Credence.Distribution'
operations and reweights.

Correctness here, as everywhere in the engine, is log-space and is left to the
oracles rather than to the eye. Weights accumulate as log-likelihoods and are turned
into a probability vector only through 'normalizedLogWeights' (reused from the
importance interpreter, already tested), so no raw log-weight is ever exponentiated
before being summed. A particle filter that quietly biases the distribution is the
same failure mode as a biased sampler, so the
proof is three checks in "test/Spec.hs": effective sample size drops as weights
concentrate and resets after a resample, resampling does not shift the empirical
mean, and on a linear-Gaussian model the per-step filtering mean and variance match
the analytic Kalman filter.

The systematic resampling scheme follows the standard SMC treatment of Doucet and
Johansen, "A Tutorial on Particle Filtering and Smoothing: Fifteen Years Later"
(2009), and Arulampalam, Maskell, Gordon, and Clapp, "A Tutorial on Particle Filters
for Online Nonlinear/Non-Gaussian Bayesian Tracking" (IEEE Trans. Signal Processing,
2002): one uniform draw on @[0, 1/N)@ seeds @N@ equally spaced positions whose
inversion of the cumulative weight gives the surviving ancestors. It is low-variance
and the ancestor weights are read off the model, not fabricated.

The interpreter is generator-polymorphic over 'StatefulGen', so a fixed @ST@ seed
drives it deterministically in the tests and @IO@ drives the streaming engine, the
two-mode shape every interpreter shares.
-}
module Credence.Inference.SMC (
    StateSpace (..),
    FilterStep (..),
    CloudStep (..),
    particleFilter,
    particleCloud,
    effectiveSampleSize,
    systematicResample,
) where

import Control.Monad (replicateM)
import Credence.Distribution (Dist, sampleDist)
import Credence.Inference.Importance (normalizedLogWeights)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import System.Random.Stateful (StatefulGen, uniformRM)

{- | A state-space model, expressed through the distribution library.

Three pieces describe a latent state @s@ that evolves in time and is observed
noisily:

* @ssInit@ is the prior over the initial state @x_0@, a 'Dist' the filter draws the
  initial particle cloud from.
* @ssTransition@ is the propagation kernel: given the previous state it returns the
  'Dist' of the next state @x_t@. A Gaussian random walk is @\\x -> normalDist x
  (sqrt q)@.
* @ssLogLik@ is the observation model in log-space: @ssLogLik x y@ is the
  log-likelihood of the scalar observation @y@ given state @x@. A Gaussian
  observation is @\\x y -> logDensityDist (normalDist x (sqrt r)) y@.

Nothing here is particle-filter-specific; it is the same 'Dist' the other
interpreters use, which keeps the distribution code the single source of truth for
both sampling and density.
-}
data StateSpace s = StateSpace
    { ssInit :: Dist s
    -- ^ Prior over the initial state @x_0@.
    , ssTransition :: s -> Dist s
    -- ^ Propagation kernel @x_{t-1} -> Dist x_t@.
    , ssLogLik :: s -> Double -> Double
    -- ^ Observation log-likelihood @log p(y_t | x_t)@.
    }

{- | One step of the filtering output: the scalar filtering summary at time @t@.

After each observation is folded in, the filter records the weighted mean and
weighted variance of the (scalar) state under the post-update normalized weights,
which together are the filtering estimate @E[x_t | y_1..y_t]@ and its spread, plus
the effective sample size at that step (recorded BEFORE any resample, so it reflects
how concentrated the weights actually became). For a 'Double' state the projection
is the state itself, which is all the Kalman comparison needs.
-}
data FilterStep = FilterStep
    { fsMean :: !Double
    -- ^ Weighted filtering mean of the state at this step.
    , fsVariance :: !Double
    -- ^ Weighted filtering variance of the state at this step.
    , fsEss :: !Double
    -- ^ Effective sample size at this step, before any resample.
    }
    deriving (Eq, Show)

{- | Effective sample size of a log-weight vector: @1 / sum_i w_i^2@ over the
NORMALIZED weights.

The log-weights are turned into a probability vector by 'normalizedLogWeights' (the
same log-space normalization the importance interpreter uses, never exponentiating a
raw log-weight before summing), and the ESS is the reciprocal of the sum of their
squares. It ranges from @1@ when all mass sits on one particle to @N@ when the @N@
weights are equal, so it measures how many particles are effectively contributing.
This is the standard SMC degeneracy diagnostic (Doucet and Johansen 2009, Section
3.5). An empty weight vector has no particles; its normalized vector is empty and the
sum of squares is @0@, so this returns @+Infinity@, a case the filter never reaches
because it always runs at least one particle.
-}
effectiveSampleSize :: VU.Vector Double -> Double
effectiveSampleSize logWeights = 1 / VU.sum (VU.map (\w -> w * w) normalized)
  where
    -- Log-weights normalized to a probability vector in log-space.
    normalized = normalizedLogWeights logWeights

{- | Systematic resampling: draw @N@ ancestor indices in proportion to the
NORMALIZED weights of @N@ particles, with a single uniform draw.

Systematic resampling (Doucet and Johansen 2009; Arulampalam et al. 2002) draws one
@u0@ uniform on @[0, 1/N)@, forms the @N@ equally spaced positions @u0 + i/N@ for
@i@ in @0..N-1@, and for each position returns the first index where the cumulative
sum of normalized weights reaches it. A particle of weight @w@ thus spawns about
@N*w@ offspring, in expectation exactly @N*w@, which is what makes the scheme
unbiased; the single shared @u0@ is what makes it lower-variance than drawing @N@
independent uniforms (multinomial resampling). The returned vector is the surviving
ancestor index for each of the @N@ slots; the caller rebuilds the particle states
from these indices and resets every weight to uniform (log @0@), since after
resampling the offspring carry equal weight by construction.

The log-weights are normalized through 'normalizedLogWeights' first, so this stays
in the log-space discipline; the cumulative inversion then runs on the resulting
probability vector. The @particles@ argument fixes @N@ (its length) and is the
ancestor pool the indices point into.
-}
systematicResample ::
    (StatefulGen g m) =>
    -- | Log-weights of the @N@ particles.
    VU.Vector Double ->
    -- | The @N@ ancestor indices @0..N-1@ (the pool to resample from).
    VU.Vector Int ->
    g ->
    m (VU.Vector Int)
systematicResample logWeights particles gen = do
    -- One uniform on [0, 1/N): the shared offset for all N equally spaced positions.
    u0 <- uniformRM (0, 1 / n) gen
    pure (VU.generate count (pick u0))
  where
    count = VU.length particles
    n = fromIntegral count :: Double
    -- Normalized weights and their running cumulative sum, the CDF to invert.
    weights = normalizedLogWeights logWeights
    cumulative = VU.scanl1' (+) weights
    -- For position k, the comb point is u0 + k/N; find the first index whose
    -- cumulative weight reaches it, walking forward from index 0.
    pick u0 k = invert 0 (u0 + fromIntegral k / n)
    -- Invert the CDF: advance the ancestor index until the cumulative weight covers
    -- the position, clamping at the last index against floating-point round-off that
    -- could leave the final comb point a hair above the cumulative sum's last entry.
    invert !j position
        | j >= count - 1 = particles VU.! (count - 1)
        | position <= cumulative VU.! j = particles VU.! j
        | otherwise = invert (j + 1) position

{- | Run a particle filter over a state-space model and an observation sequence.

@particleFilter n threshold ss ys gen@ runs @n@ particles through the model @ss@
against the scalar observations @ys@, resampling whenever the effective sample size
falls below @threshold * n@, and returns one 'FilterStep' per observation.

The loop is the standard sequential-importance-resampling (SIR) filter:

1. Draw @n@ initial particles from @ssInit@; their log-weights start at @0@.
2. For each observation @y@:
     * propagate every particle through @ssTransition@ (one draw each), and
     * add @ssLogLik particle y@ to that particle's log-weight (LOG-SPACE; weights
       accumulate additively as log-likelihoods).
   Then compute the effective sample size and the weighted filtering mean and
   variance of the (scalar) state under the current NORMALIZED weights, and record
   them as the step's 'FilterStep'. The moments are taken BEFORE any resample, so
   they are the genuine filtering estimate; resampling only resets the population for
   the next step and must not be allowed to shift this step's estimate.
     * If the effective sample size is below @threshold * n@, systematically resample
       the particles and reset every log-weight to @0@ (the offspring are equally
       weighted by construction). Otherwise carry the weighted particles forward
       unchanged.

The state projection used for the moments is @realToFrac@ of the state, so the
filter summarizes a @Double@-valued state directly; a richer state would project to a
scalar of interest the same way. The single generator is threaded left to right
through every propagation and every resample draw, so one fixed @ST@ seed makes the
whole run deterministic, the property the oracles depend on.

The threshold is a fraction in @[0, 1]@ at the caller's discretion; @0.5@ (resample
when fewer than half the particles are effectively contributing) is the common
choice. A degenerate observation that drives every log-weight to @-Infinity@ would
make the normalized weights @NaN@; this is left to surface loudly rather than be
masked, per the numerics doctrine, and does not arise for the proper Gaussian
likelihoods the filter is run on.
-}
particleFilter ::
    (Real s, StatefulGen g m) =>
    -- | Number of particles @N@.
    Int ->
    -- | ESS-resample threshold as a fraction of @N@ (e.g. @0.5@).
    Double ->
    -- | The state-space model.
    StateSpace s ->
    -- | The scalar observation sequence.
    [Double] ->
    g ->
    m (V.Vector FilterStep)
particleFilter count threshold ss observations gen = do
    -- Draw the initial particle cloud from the prior; weights start at log 1 = 0.
    -- States are a boxed vector so the filter stays polymorphic in the state type
    -- (a Double state is all the oracles need, but nothing here forces Unbox); the
    -- weights are always Double, so they live in an unboxed vector.
    initialStates <- V.fromList <$> replicateM count (sampleDist (ssInit ss) gen)
    let initialLogWeights = VU.replicate count 0
    (_, _, stepsRev) <- foldStep (initialStates, initialLogWeights, []) observations
    pure (V.fromList (reverse stepsRev))
  where
    n = fromIntegral count :: Double
    -- The ESS level below which a resample is triggered.
    resampleBelow = threshold * n

    -- Fold the observation sequence, threading (states, logWeights, stepsReversed).
    foldStep state [] = pure state
    foldStep (states, logWeights, steps) (y : rest) = do
        -- Propagate: one transition draw per particle, generator threaded across all.
        propagated <- V.generateM count $ \i ->
            sampleDist (ssTransition ss (states V.! i)) gen
        let
            -- Reweight in log-space: add this observation's log-likelihood per
            -- particle to its running log-weight. No exponentiation here. imap over
            -- the (unboxed) weights indexes the matching (boxed) propagated state.
            updatedLogWeights =
                VU.imap (\i w -> w + ssLogLik ss (propagated V.! i) y) logWeights
            -- Normalized weights drive both the ESS and the filtering moments.
            normalized = normalizedLogWeights updatedLogWeights
            ess = effectiveSampleSize updatedLogWeights
            -- Scalar projection of each particle's state for the filtering summary.
            values = VU.generate count (\i -> realToFrac (propagated V.! i))
            -- Weighted filtering mean and variance under the post-update weights,
            -- variance by the E[x^2] - E[x]^2 identity, taken before any resample so
            -- the estimate is the true filtering estimate at this step.
            mean = VU.sum (VU.zipWith (*) normalized values)
            second = VU.sum (VU.zipWith (\w x -> w * x * x) normalized values)
            variance = second - mean * mean
            step = FilterStep{fsMean = mean, fsVariance = variance, fsEss = ess}
        if ess < resampleBelow
            then do
                -- Resample the population in proportion to weight, then reset the
                -- weights to uniform: the offspring are equally weighted.
                ancestors <- systematicResample updatedLogWeights (VU.enumFromN 0 count) gen
                let resampledStates = V.backpermute propagated (VU.convert ancestors)
                    resetLogWeights = VU.replicate count 0
                foldStep (resampledStates, resetLogWeights, step : steps) rest
            else
                -- Keep the weighted population; carry the log-weights forward.
                foldStep (propagated, updatedLogWeights, step : steps) rest

{- | One step of the filtering output WITH the weighted particle population, for the
live cloud the visualizer renders.

'FilterStep' summarizes a step in three numbers; a cloud needs the particles
themselves. 'CloudStep' adds the post-update particle population to the same summary:
the scalar projection of each particle's state and its NORMALIZED weight, both taken
at the same point the 'FilterStep' moments are (after the observation update, before
any resample), so the cloud the client paints is the genuine filtering population
that produced the step's mean. The two vectors are aligned (@csWeights ! i@ is the
weight of @csStates ! i@) and sum-to-one normalized, exactly the opacity-by-weight
input the existing cloud reads. No raw log-weight is exposed; the weights are the
same normalized probabilities 'normalizedLogWeights' produces.
-}
data CloudStep = CloudStep
    { csStep :: !FilterStep
    -- ^ The scalar filtering summary at this step (mean, variance, ESS).
    , csStates :: !(VU.Vector Double)
    -- ^ The scalar projection of each particle's state, post-update.
    , csWeights :: !(VU.Vector Double)
    -- ^ The normalized weight of each particle, aligned with 'csStates'.
    }
    deriving (Eq, Show)

{- | Run a particle filter and return the weighted particle population per step, the
cloud variant of 'particleFilter'.

Identical to 'particleFilter' in dynamics (the same propagate, reweight in log-space,
ESS, systematic-resample-below-threshold loop, with the moments taken before any
resample), but each step records a 'CloudStep': the 'FilterStep' summary plus the
post-update particle states and their normalized weights. This is what the live
tracking demo streams so the client can draw the particle cloud following the target,
with each particle's opacity scaled by its weight; 'particleFilter' returns only the
summaries and is kept as the lean path the oracles run against. The generator is
threaded left to right exactly as in 'particleFilter', so a fixed seed makes the run
deterministic.
-}
particleCloud ::
    (Real s, StatefulGen g m) =>
    -- | Number of particles @N@.
    Int ->
    -- | ESS-resample threshold as a fraction of @N@ (e.g. @0.5@).
    Double ->
    -- | The state-space model.
    StateSpace s ->
    -- | The scalar observation sequence.
    [Double] ->
    g ->
    m (V.Vector CloudStep)
particleCloud count threshold ss observations gen = do
    initialStates <- V.fromList <$> replicateM count (sampleDist (ssInit ss) gen)
    let initialLogWeights = VU.replicate count 0
    (_, _, stepsRev) <- foldStep (initialStates, initialLogWeights, []) observations
    pure (V.fromList (reverse stepsRev))
  where
    n = fromIntegral count :: Double
    resampleBelow = threshold * n

    foldStep state [] = pure state
    foldStep (states, logWeights, steps) (y : rest) = do
        propagated <- V.generateM count $ \i ->
            sampleDist (ssTransition ss (states V.! i)) gen
        let
            updatedLogWeights =
                VU.imap (\i w -> w + ssLogLik ss (propagated V.! i) y) logWeights
            normalized = normalizedLogWeights updatedLogWeights
            ess = effectiveSampleSize updatedLogWeights
            values = VU.generate count (\i -> realToFrac (propagated V.! i))
            mean = VU.sum (VU.zipWith (*) normalized values)
            second = VU.sum (VU.zipWith (\w x -> w * x * x) normalized values)
            variance = second - mean * mean
            filterStep = FilterStep{fsMean = mean, fsVariance = variance, fsEss = ess}
            -- The population the cloud renders: the scalar states and their normalized
            -- weights at this step, captured before any resample so they are the
            -- genuine filtering cloud behind the step's mean.
            cloudStep = CloudStep{csStep = filterStep, csStates = values, csWeights = normalized}
        if ess < resampleBelow
            then do
                ancestors <- systematicResample updatedLogWeights (VU.enumFromN 0 count) gen
                let resampledStates = V.backpermute propagated (VU.convert ancestors)
                    resetLogWeights = VU.replicate count 0
                foldStep (resampledStates, resetLogWeights, cloudStep : steps) rest
            else
                foldStep (propagated, updatedLogWeights, cloudStep : steps) rest
