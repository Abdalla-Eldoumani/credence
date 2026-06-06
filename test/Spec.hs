module Main (main) where

import Credence (logSumExp)
import Data.Vector.Unboxed qualified as VU
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

main :: IO ()
main = defaultMain tests

-- The real stage 1 test is conjugate-analytic agreement: importance
-- sampling on the Beta-Bernoulli coin model must match the closed-form Beta
-- posterior in mean and variance within a documented tolerance, run under a
-- fixed seed. That test is the one that proves the sampler is correct, since a
-- wrong posterior looks plausible. See the correctness checks for
-- the testing doctrine. This placeholder only checks the log-space helper so the
-- suite builds, runs, and fails loudly if the foundation breaks.
tests :: TestTree
tests = testGroup "credence" [logSumExpTests]

logSumExpTests :: TestTree
logSumExpTests =
    testGroup
        "logSumExp"
        [ testCase "sum of three zero-logs is log 3" $
            assertBool
                "result within tolerance of log 3"
                (abs (logSumExp (VU.fromList [0, 0, 0]) - log 3) < 1e-12)
        ]
