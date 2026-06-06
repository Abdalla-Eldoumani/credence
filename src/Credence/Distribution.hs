{-# LANGUAGE RankNTypes #-}

{- | The distribution library: six distributions, each a sampler paired with a
hand-written log-density under one parameterization.

A 'Dist' reifies a distribution as the two operations inference needs of it: a
way to draw a value, and a way to score a value in log-space. Both halves share
one parameterization by construction, which is the whole point. The classic trap
is a sampler and a density that are each correct for a different parameterization
of the same family (Gamma shape/rate against shape/scale, for instance): every
isolated test passes and the combination is silently wrong. Pairing them here,
and proving they agree with a moment-matching test, is the defense.

The samplers come from @mwc-random@. The log-densities are written by hand over
@math-functions@ and deliberately do not route through the @statistics@ package,
which is reserved as the independent test oracle: an implementation checked
against itself checks nothing.
-}
module Credence.Distribution (
    Dist (..),
    bernoulliDist,
    betaDist,
    normalDist,
    gammaDist,
    categoricalDist,
    uniformDist,
) where

import Credence (logSumExp)
import Data.Vector.Unboxed qualified as VU
import Numeric.SpecFunctions (logBeta, logGamma)
import System.Random.MWC.Distributions qualified as MWC
import System.Random.Stateful (StatefulGen, uniformRM)

{- | A distribution over @a@: a sampler paired with a log-density.

The sampler is rank-2 over @mwc-random@'s 'MWC.StatefulGen' interface so one
'Dist' runs in both @ST s@ (a fixed seed, for deterministic tests) and @IO@ (the
streaming engine), without committing to a monad at the type. The log-density is
a pure function returning a log-probability; it is the same parameterization the
sampler draws from.
-}
data Dist a = Dist
    { sampleDist :: forall g m. (StatefulGen g m) => g -> m a
    -- ^ Draw one value from the distribution.
    , logDensityDist :: a -> Double
    -- ^ Log-density (continuous) or log-probability (discrete) of a value.
    }

{- | Bernoulli(p): 'True' with probability @p@, 'False' otherwise.

Log-density is @log p@ at 'True' and @log (1 - p)@ at 'False'. Finite for
@p@ strictly inside @(0, 1)@; at the endpoints one branch is @-Infinity@, which
is the honest log-probability of an impossible outcome and is left to propagate
rather than be clamped.
-}
bernoulliDist :: Double -> Dist Bool
bernoulliDist p =
    Dist
        { sampleDist = MWC.bernoulli p
        , logDensityDist = \b -> if b then log p else log (1 - p)
        }

{- | Beta(a, b) on @(0, 1)@ with shape parameters @a@ and @b@.

Log-density at @x@ is @(a - 1) * log x + (b - 1) * log (1 - x) - logBeta a b@,
where @logBeta@ is the log of the beta function from @math-functions@. Finite for
@a, b > 0@ and @x@ in the open interval; outside the open support it is
@-Infinity@.

The endpoints are guarded explicitly rather than left to @log@. At @x = 0@ with
@a = 1@ the term @(a - 1) * log x@ is @0 * -Infinity@, which is @NaN@, not the
@-Infinity@ the impossible outcome deserves; the same trap sits at @x = 1@ with
@b = 1@, and Beta(1, 1) is the uniform prior that the coin model leans on. So
anything at or beyond the open interval scores @-Infinity@ and only interior
points reach the formula.
-}
betaDist :: Double -> Double -> Dist Double
betaDist a b =
    Dist
        { sampleDist = MWC.beta a b
        , logDensityDist = \x ->
            if x > 0 && x < 1
                then (a - 1) * log x + (b - 1) * log (1 - x) - logBeta a b
                else negate (1 / 0)
        }

{- | Normal(mu, sigma) with mean @mu@ and standard deviation @sigma@ (not
variance).

Log-density at @x@ is @-0.5 * log (2 * pi * sigma^2) - (x - mu)^2 / (2 * sigma^2)@.
Finite for @sigma > 0@ and every real @x@.
-}
normalDist :: Double -> Double -> Dist Double
normalDist mu sigma =
    Dist
        { sampleDist = MWC.normal mu sigma
        , logDensityDist = \x ->
            -(0.5 * log (2 * pi * sigma * sigma)) - (x - mu) * (x - mu) / (2 * sigma * sigma)
        }

{- | Gamma with shape @k@ and SCALE @theta@ (not rate).

This parameterization is deliberate and load-bearing. @mwc-random@'s
'MWC.gamma' takes shape then scale, and the @statistics@ oracle's @gammaDistr k
theta@ also reads @theta@ as scale, so all three agree. The shape/rate
convention (rate = 1/scale) is the trap: swapping it leaves a sampler and a
density that are each correct for a different distribution, every separate test
green, and the result silently wrong. The moment test ties them together (mean
@k * theta@, variance @k * theta^2@).

Log-density at @x > 0@ is @(k - 1) * log x - x / theta - k * log theta - logGamma k@,
with @logGamma@ the log gamma function from @math-functions@. The support is
@x > 0@, guarded explicitly: at @x = 0@ with @k = 1@ the term @(k - 1) * log x@
is @0 * -Infinity = NaN@, and at @x < 0@ the @log x@ is @NaN@ too, so anything at
or below zero scores @-Infinity@ rather than poisoning the weight with a @NaN@.
-}
gammaDist :: Double -> Double -> Dist Double
gammaDist k theta =
    Dist
        { sampleDist = MWC.gamma k theta
        , logDensityDist = \x ->
            if x > 0
                then (k - 1) * log x - x / theta - k * log theta - logGamma k
                else negate (1 / 0)
        }

{- | Categorical over indices @0 .. n - 1@ with unnormalized non-negative
@weights@.

The weights need not sum to one; normalization happens in log-space.
'MWC.categorical' samples an index in proportion to the weights, and the
log-density of index @i@ is @log (weights ! i) - logSumExp (map log weights)@.
Doing the normalization through 'logSumExp' rather than dividing by a raw sum
keeps it stable and underflow-free, the failure mode the correctness
discipline flags for categoricals specifically.
-}
categoricalDist :: VU.Vector Double -> Dist Int
categoricalDist weights =
    Dist
        { sampleDist = MWC.categorical weights
        , logDensityDist = \i -> log (weights VU.! i) - logZ
        }
  where
    -- Normalizer computed once in log-space and shared by every query.
    logZ = logSumExp (VU.map log weights)

{- | Uniform(lo, hi) on @[lo, hi]@.

Log-density is the constant @-log (hi - lo)@ on the support and @-Infinity@
outside it. Finite for @hi > lo@. Sampling uses 'uniformRM' from
@System.Random.Stateful@: @System.Random.MWC.Distributions@ has no uniform, and
@mwc-random@'s own @uniformR@ is fixed to the concrete @Gen@ type, which would
break the rank-2 'StatefulGen' shape every other constructor uses.
-}
uniformDist :: Double -> Double -> Dist Double
uniformDist lo hi =
    Dist
        { sampleDist = uniformRM (lo, hi)
        , logDensityDist = \x ->
            if x >= lo && x <= hi then negate (log (hi - lo)) else negate (1 / 0)
        }
