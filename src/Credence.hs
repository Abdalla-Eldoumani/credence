{- | The public surface of the credence library. This module is intentionally
small at the start. As the library grows, re-export the model monad, the
distribution library, the observe primitive, and the importance sampler from
their respective modules under Credence.* so that callers import only this
one module. Keep the export list and credence.cabal exposed-modules in sync.
-}
module Credence (
    logSumExp,
) where

import Data.Vector.Unboxed qualified as VU

{- | Numerically stable @log (sum (map exp xs))@.

Inference works in log-space to avoid underflow: a product of thousands of
small probabilities rounds to zero in Double, but the sum of their logs does
not. The catch is that recovering a sum from logs needs @exp@, which
overflows or underflows for inputs far from zero. Factoring out the maximum
first keeps every @exp@ argument at or below zero, so the largest term is
exactly 1 and the rest are well-scaled fractions. This helper is the building
block for normalizing importance weights and for marginal likelihoods, so it
has to be correct before anything downstream can be trusted.

An empty input has no probability mass, so its log-sum is negative infinity.
-}
logSumExp :: VU.Vector Double -> Double
logSumExp xs
    | VU.null xs = negate (1 / 0)
    | otherwise = m + log (VU.sum (VU.map (\x -> exp (x - m)) xs))
  where
    m = VU.maximum xs
