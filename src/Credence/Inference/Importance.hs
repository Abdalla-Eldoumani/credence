{- | Likelihood-weighted importance sampling: the first inference interpreter.

Importance sampling is the simplest interpreter of the 'Model' monad and the one
that needs no addressing, so it runs any model, including those whose structure
depends on random outcomes. The idea: run the program forward, drawing each
'Sample' from its prior and proposing nothing cleverer, and let each 'Observe'
reweight the execution by its likelihood. A single forward run yields one
\"particle\": a sampled value paired with the total log-likelihood of the
observations along that run. Many runs give a weighted sample of the posterior;
the weights correct the mismatch between the prior we drew from and the posterior
we want.

Correctness here is entirely about staying in log-space. Each observation
contributes a log-likelihood, and a run's weight is their sum, which can be a
large negative number; exponentiating it on its own underflows to zero and the
posterior collapses or goes to NaN. So weights are combined through
'Credence.logSumExp', never by exponentiating individual log-weights and adding.
The normalization subtracts the log-sum-exp inside the exponent, which keeps the
largest weight at @exp 0 = 1@ and every other a well-scaled fraction. This is the
numerics the correctness discipline insists on, and the conjugate-agreement
gate is what proves the result is the true posterior and not a plausible wrong
one.

The interpreter is polymorphic over the generator ('StatefulGen'), so the same
code runs deterministically in @ST@ under a fixed seed for the tests and in @IO@
for the streaming engine, matching the two-mode shape of 'Credence.Distribution'.
-}
module Credence.Inference.Importance (
    importanceSamples,
    normalizedLogWeights,
    posteriorMean,
) where

import Control.Monad (replicateM)
import Credence (logSumExp)
import Credence.Distribution (sampleDist)
import Credence.Model (Model (..))
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import System.Random.Stateful (StatefulGen)

{- | Run one importance particle: interpret a 'Model' forward against a
generator, returning the sampled value and the run's total log-weight.

Each @Sample d k@ draws from the prior @d@ with 'sampleDist' and continues; each
@Observe w m@ adds the log-likelihood increment @w@ to the running weight and
continues; @Pure x@ ends the run. The weight starts at @0@ (log @1@, an
un-reweighted execution) and accumulates additively in log-space, so a run that
observes nothing has weight @0@ and a run with many observations has the sum of
their log-likelihoods. The generator is threaded left to right through every
draw, so one seeded generator makes the whole run deterministic.
-}
runParticle :: (StatefulGen g m) => Model a -> g -> m (a, Double)
runParticle model gen = go 0 model
  where
    go !w (Pure x) = pure (x, w)
    go !w (Sample d k) = do
        x <- sampleDist d gen
        go w (k x)
    go !w (Observe dw m) = go (w + dw) m

{- | Draw @n@ importance particles from a model under one generator.

This is the entry point: it runs 'runParticle' @n@ times, threading the same
generator through every run so the whole batch is deterministic under a fixed
seed. The result is a boxed vector of @(value, logWeight)@ pairs, the weighted
sample that 'posteriorMean' summarizes. It is generator-polymorphic, so the test
suite calls it in @ST@ with a fixed seed and the engine calls it in @IO@.
-}
importanceSamples :: (StatefulGen g m) => Int -> Model a -> g -> m (V.Vector (a, Double))
importanceSamples n model gen = V.fromList <$> replicateM n (runParticle model gen)

{- | Normalize a vector of log-weights into a probability vector in log-space.

Returns @exp (w_i - logSumExp ws)@ for each weight, which is the self-normalized
importance weight. The subtraction happens inside the exponent, so the maximum
weight maps to @exp 0 = 1@ and the rest to fractions of it; no individual
log-weight is exponentiated before being summed, which is the underflow trap the
correctness discipline calls out. The normalizer 'logSumExp' is the stable
log-sum-exp from "Credence". The result sums to @1@ up to floating-point
round-off. An empty input gives an empty result (there is nothing to normalize).
-}
normalizedLogWeights :: VU.Vector Double -> VU.Vector Double
normalizedLogWeights ws
    | VU.null ws = VU.empty
    | otherwise = VU.map (\w -> exp (w - logZ)) ws
  where
    -- The log normalizer, computed once in log-space and shared by every weight.
    logZ = logSumExp ws

{- | The weight-normalized posterior expectation of a projection.

Given a weighted sample @(value, logWeight)@ and a function @f@, returns
@sum_i (normalizedWeight_i * f value_i)@, the importance-sampling estimate of
@E[f(theta)]@ under the posterior. The weights are normalized with
'normalizedLogWeights', so they sum to @1@ and the result is a genuine weighted
average. Asking for @f = id@ gives the posterior mean of @theta@; asking for
@f = (^2)@ gives @E[theta^2]@, from which the conjugate gate derives the
posterior variance. An empty sample has no mass; its mean is @0@ by the empty-sum
convention, which the callers never hit because they always draw at least one
particle.
-}
posteriorMean :: V.Vector (a, Double) -> (a -> Double) -> Double
posteriorMean samples f =
    VU.sum (VU.zipWith (*) weights values)
  where
    -- Log-weights pulled into an unboxed vector for the log-space normalization.
    weights = normalizedLogWeights (VU.convert (V.map snd samples))
    -- The projection evaluated at each sampled value, aligned with the weights.
    values = VU.convert (V.map (f . fst) samples)
