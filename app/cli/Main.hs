{- | Command-line entry point. stage 1 turns this into a tool that reads the
built-in coin-bias model and a sequence of observed flips, runs importance
sampling, and prints the posterior mean and a credible interval next to the
analytic Beta posterior for comparison. Argument parsing arrives in stage
2 via optparse-applicative (already noted as a pinned dependency in
credence.cabal). For now this is a placeholder that builds and runs.
-}
module Main (main) where

import Credence (logSumExp)
import Data.Vector.Unboxed qualified as VU

main :: IO ()
main = do
    putStrLn "credence: probabilistic programming with live inference"
    putStrLn "the cli is a stage 1 deliverable; see the task list"
    -- Touch the library so the wiring is exercised by the build, not just typed.
    putStrLn ("logSumExp [0,0,0] = " <> show (logSumExp (VU.fromList [0, 0, 0])))
