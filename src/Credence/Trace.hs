{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- | The execution trace: a database of random choices addressed by execution
position, the substrate trace-based Metropolis-Hastings proposes on.

A trace records, for one run of a 'Model', every value its 'Sample' nodes drew,
keyed by the node's position in execution order, together with the run's total
'Observe' log-weight. This is the "database of values" of Wingate, Stuhlmuller,
and Goodman, "Lightweight Implementations of Probabilistic Programming Languages
Via Transformational Compilation," AISTATS 2011: each random choice is named by
its address and stored with enough information to rescore it and to redraw it, so
a proposal can perturb one address and recompute the program with every other
choice held fixed.

Addressing is by execution-order position (the first 'Sample' is address 0, the
next 1, and so on), assigned by a counter the interpreter increments at each
'Sample'. For fixed-structure models -- the only models MH is scoped to -- this
is stable across runs: the same control flow visits the
same 'Sample' nodes in the same order, so address n always names the same choice.
Models whose control flow branches on a random outcome are out of scope here;
importance sampling handles them and needs no addressing.

== The existential hazard

The 'Model' GADT's 'Sample' constructor hides the drawn value's type (it is
existential: an interpreter learns it only by supplying a generator). The trace
store therefore cannot hold a bare @value :: a@ with @a@ floating free. Each
'Entry' is an existential record that bundles, at one address, the drawn value,
its prior log-density already evaluated at that value, and an action to redraw
and rescore from the SAME 'Dist' -- packed under 'forall a' so the entry is
monomorphic and no static type leaks to the use site. MH reads 'entryLogPrior'
to form the trace score and calls 'entryResample' to propose a fresh value at one
site, neither of which needs to recover @a@.

Replay (reusing a stored value at its 'Sample' node, the deterministic
recomputation MH does after swapping one site) is the one place the hidden type
must meet the continuation @k :: a -> Model b@, which demands the concrete @a@.
That single coercion is contained in 'reuseEntryValue' and justified there: it is
sound exactly because fixed-structure addressing guarantees address n carries the
type the n-th 'Sample' expects.

Everything here stays in log-space. Prior log-densities and the observe weight are
summed as logarithms; nothing is exponentiated.
-}
module Credence.Trace (
    Address,
    Entry (..),
    Trace (..),
    emptyTrace,
    traceAddresses,
    traceLogWeight,
    traceTotalLogPrior,
    traceSize,
    traceModel,
    replayModel,
) where

import Credence.Distribution (logDensityDist, sampleDist)
import Credence.Model (Model (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Random.Stateful (StatefulGen)
import Unsafe.Coerce (unsafeCoerce)

{- | The address of a random choice: its position in execution order.

The first 'Sample' a run reaches is address 0, the next 1, and so on. An @Int@
counter the interpreter increments at each 'Sample' assigns these, following the
execution-position naming of Wingate et al. 2011. For a single-'Sample' model
like the coin model there is exactly one address, 0.
-}
type Address = Int

{- | One random choice in a trace: the value drawn at an address, its prior
log-density at that value, and an action to redraw it from the same distribution.

Existential over the drawn type @a@, because 'Sample' hides it (see the module
header). The three fields are exactly what MH needs at a site without recovering
@a@:

* 'entryValue' is the drawn value, kept so replay can feed it back into the
  model's continuation (the one use that must coerce it back to @a@; see
  'reuseEntryValue').
* 'entryLogPrior' is @logDensityDist d entryValue@, the choice's contribution to
  the trace's prior score, evaluated once when the entry is recorded so MH can sum
  per-site priors without touching the 'Dist'.
* 'entryResample' redraws from the SAME 'Dist' the value came from and returns the
  fresh value paired with its own prior log-density. MH calls this on the single
  perturbed site; the returned value is repacked into a new 'Entry'.
-}
data Entry = forall a. Entry
    { entryValue :: a
    -- ^ The drawn value, type hidden by the existential.
    , entryLogPrior :: Double
    -- ^ @logDensityDist d entryValue@: this choice's prior log-density.
    , entryResample :: forall g m. (StatefulGen g m) => g -> m (a, Double)
    -- ^ Redraw from the same 'Dist': returns @(value', logDensityDist d value')@.
    }

{- | A trace: the addressed database of choices plus the run's observe log-weight.

@traceChoices@ maps each 'Address' to its 'Entry'. @traceObserveWeight@ is the sum
of the 'Observe' increments along the run, in log-space, matching what importance
sampling accumulates for the same model. Together they give the trace's full
unnormalized log-probability: the sum of every entry's 'entryLogPrior' plus
@traceObserveWeight@ (see 'traceTotalLogPrior' and 'traceLogWeight').
-}
data Trace = Trace
    { traceChoices :: Map Address Entry
    -- ^ Address-keyed store of drawn choices.
    , traceObserveWeight :: Double
    -- ^ Sum of 'Observe' log-likelihood increments along the run.
    }

-- | The empty trace: no choices recorded, zero observe weight (log 1).
emptyTrace :: Trace
emptyTrace = Trace Map.empty 0

{- | The addresses in a trace, in ascending order.

Because addresses are execution-order positions starting at 0, this is
@[0 .. n-1]@ for an n-choice run. The address-stability test asserts two runs of
the coin model yield the same list (@[0]@).
-}
traceAddresses :: Trace -> [Address]
traceAddresses = Map.keys . traceChoices

{- | The run's observe log-weight: the sum of 'Observe' increments, in log-space.

This is the likelihood term of the trace's log-probability, the same quantity a
single importance particle carries for the model.
-}
traceLogWeight :: Trace -> Double
traceLogWeight = traceObserveWeight

{- | The sum of every choice's prior log-density: the trace's prior log-score.

Adding this to 'traceLogWeight' gives the trace's full unnormalized
log-probability @log p(x)@, the quantity the MH acceptance ratio differences
between the current and proposed traces.
-}
traceTotalLogPrior :: Trace -> Double
traceTotalLogPrior = Map.foldl' (\ !acc e -> acc + entryLogPrior e) 0 . traceChoices

{- | The number of random choices in a trace, @|x|@ in the Wingate et al.
acceptance ratio.

The proposal picks one of these uniformly, so its reciprocal is the
address-selection probability, and the count enters the proposal-correction term
of the general-form ratio.
-}
traceSize :: Trace -> Int
traceSize = Map.size . traceChoices

{- | Interpret a 'Model' forward, recording a trace.

Structural recursion in the shape of importance sampling's @runParticle@, but
threading an address counter alongside the generator and the running weight. At
each @Sample d k@ it draws @x@ from @d@, records an 'Entry' at the current address
carrying @x@, its prior log-density @logDensityDist d x@, and a resample closure
capturing @d@, then continues @k x@ at the next address. At each @Observe dw m@ it
adds @dw@ to the running observe weight. At @Pure x@ it finishes, returning the
value and the assembled trace.

Generator-polymorphic over 'StatefulGen', so tests run it in @ST@ under a fixed
seed and the engine runs it in @IO@.
-}
traceModel :: (StatefulGen g m) => Model a -> g -> m (a, Trace)
traceModel model gen = go 0 Map.empty 0 model
  where
    go !_ !choices !w (Pure x) = pure (x, Trace choices w)
    go !addr !choices !w (Sample d k) = do
        x <- sampleDist d gen
        let entry =
                Entry
                    { entryValue = x
                    , entryLogPrior = logDensityDist d x
                    , entryResample = \g -> do y <- sampleDist d g; pure (y, logDensityDist d y)
                    }
        go (addr + 1) (Map.insert addr entry choices) w (k x)
    go !addr !choices !w (Observe dw m) = go addr choices (w + dw) m

{- | Re-interpret a 'Model' against an existing trace, reusing stored choices.

This is the deterministic recomputation MH performs after swapping one site: at
each @Sample d k@, look up the current address in the given trace; if an 'Entry'
is present, reuse its stored value (feeding it back into @k@ and rescoring it
against the present @d@) instead of drawing; if the address is absent, draw fresh
exactly as 'traceModel' would. Either way it records a NEW trace from scratch,
rescoring every reused value with the current node's 'Dist' so the rebuilt entry's
'entryLogPrior' and 'entryResample' belong to this run, and re-summing the observe
weight. The generator is consulted only for genuinely fresh draws (a not-yet-stored
address), which for fixed-structure models is none after the initial run.

MH builds its proposed trace by inserting one resampled 'Entry' into the current
trace and calling 'replayModel'; every other address reuses its stored value, so
only the perturbed choice changes and the program recomputes consistently around
it.
-}
replayModel :: (StatefulGen g m) => Model a -> Trace -> g -> m (a, Trace)
replayModel model (Trace stored _) gen = go 0 Map.empty 0 model
  where
    go !_ !choices !w (Pure x) = pure (x, Trace choices w)
    go !addr !choices !w (Sample d k) =
        case Map.lookup addr stored of
            Just e -> do
                -- Reuse the stored value. It is existential in the entry but the
                -- continuation @k@ and @d@ fix its type to whatever this node's
                -- 'Sample' expects; 'reuseEntryValue' performs the one contained
                -- coercion that bridges them (sound under fixed-structure
                -- addressing; see its note). Rescore against the present @d@ so the
                -- rebuilt entry's prior log-density and resampler belong to this run.
                let x = reuseEntryValue e
                    entry =
                        Entry
                            { entryValue = x
                            , entryLogPrior = logDensityDist d x
                            , entryResample = \g -> do y <- sampleDist d g; pure (y, logDensityDist d y)
                            }
                go (addr + 1) (Map.insert addr entry choices) w (k x)
            Nothing -> do
                x <- sampleDist d gen
                let entry =
                        Entry
                            { entryValue = x
                            , entryLogPrior = logDensityDist d x
                            , entryResample = \g -> do y <- sampleDist d g; pure (y, logDensityDist d y)
                            }
                go (addr + 1) (Map.insert addr entry choices) w (k x)
    go !addr !choices !w (Observe dw m) = go addr choices (w + dw) m

{- | Recover a stored entry's value at the type its 'Sample' node demands.

This is the single, contained coercion the existential trace forces, and the only
@unsafeCoerce@ in the engine. The hazard and its resolution: 'Entry' hides the
drawn type @a@ behind an existential (because 'Model'\'s 'Sample' does), so the
stored 'entryValue' has no recoverable static type. Replay must feed that value
back into the continuation @k :: a -> Model b@ of the 'Sample' node at the same
address, which demands the concrete @a@. The two @a@s are the same type: addresses
are execution-order positions and MH is scoped to fixed-structure models, so the
node at address n is the same 'Sample' on every run and therefore draws and
consumes the same type. The type system cannot see this (the value was packed
under the existential), so the coercion is asserted here rather than proved.

It is exercised end to end by three tests that would all fail if the assumption
were violated: address stability (the address sets match across runs), detailed
balance (the chain converges to a known target), and conjugate agreement (the
chain matches the analytic Beta posterior). A coercion at a genuinely mismatched
type would corrupt the recomputed trace and break all three. The reuse is only
ever applied to a value this very interpreter stored at this address on a prior
run of the same fixed-structure model, never to caller-supplied data.
-}
reuseEntryValue :: Entry -> a
reuseEntryValue (Entry v _ _) = unsafeCoerce v
