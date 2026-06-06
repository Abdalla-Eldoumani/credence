{- | Command-line entry point: the coin posterior, end to end.

This is the human-facing proof that the pipeline works before the web
visualizer exists. It reads a sequence of observed coin flips and a Beta prior,
runs the built-in 'coinModel' under likelihood-weighted importance sampling, and
prints the posterior mean with an equal-tailed credible interval next to the
closed-form Beta(a + heads, b + tails) answer, so the approximation sits beside
the exact value the conjugate posterior gives.

The inference lives in the library and is reused unchanged: this module builds
the model, runs 'importanceSamples', and summarizes the weighted sample. The
only arithmetic here is summarizing that output (a weighted equal-tailed
interval) and the closed-form Beta moments for the side-by-side comparison; no
sampling or weighting is re-implemented.

A coin run is a demonstration, not a statistical test, so it draws from an
IO-bound generator seeded from system entropy ('createSystemRandom') rather than
the fixed-seed ST generator the test suite uses. Successive runs therefore vary
slightly, which is the honest behavior of a Monte Carlo estimate; the printed
analytic column is the fixed target every run should land near.
-}
module Main (main) where

import Credence.Inference.Importance (importanceSamples, posteriorMean)
import Credence.Model (coinModel)
import Data.List (sortOn)
import Data.Vector qualified as V
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    strOption,
    value,
    (<**>),
 )
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Random.MWC (createSystemRandom)
import Text.Printf (printf)

-- | The parsed command line: a flip sequence, a Beta prior, and a particle count.
data Options = Options
    { optFlips :: String
    -- ^ Observed flips as a string of H/h (heads) and T/t (tails).
    , optPriorA :: Double
    -- ^ Beta prior shape @a@ (pseudo-count of heads).
    , optPriorB :: Double
    -- ^ Beta prior shape @b@ (pseudo-count of tails).
    , optParticles :: Int
    -- ^ Number of importance particles.
    }

{- | The argument parser. The flip sequence is required; the prior and particle
count default to the Beta(2, 2) prior and a particle budget large enough that the
estimate lands close to the analytic answer on a short flip sequence.
-}
options :: Parser Options
options =
    Options
        <$> strOption
            ( long "flips"
                <> metavar "HHTHT"
                <> help "Observed coin flips as a string of H (heads) and T (tails)"
            )
        <*> option
            auto
            ( long "prior-a"
                <> metavar "A"
                <> value 2.0
                <> showDefault
                <> help "Beta prior shape a (prior heads pseudo-count)"
            )
        <*> option
            auto
            ( long "prior-b"
                <> metavar "B"
                <> value 2.0
                <> showDefault
                <> help "Beta prior shape b (prior tails pseudo-count)"
            )
        <*> option
            auto
            ( long "particles"
                <> metavar "N"
                <> value 100000
                <> showDefault
                <> help "Number of importance-sampling particles"
            )

-- | The parser wrapped with usage text and a one-line description.
optionsInfo :: ParserInfo Options
optionsInfo =
    info
        (options <**> helper)
        ( fullDesc
            <> progDesc
                "Run the coin-bias model on a flip sequence under importance sampling \
                \and compare the posterior to the analytic Beta result"
            <> header "credence -- coin posterior by importance sampling"
        )

{- | Parse a flip string into a list of outcomes, 'True' for heads.

Accepts @H@/@h@ for heads and @T@/@t@ for tails; any other character is an
error, surfaced loudly rather than silently dropped, because a miscounted flip
quietly shifts the posterior.
-}
parseFlips :: String -> Either String [Bool]
parseFlips = traverse one
  where
    one 'H' = Right True
    one 'h' = Right True
    one 'T' = Right False
    one 't' = Right False
    one c = Left ("not a coin flip: " <> show c <> " (use H or T)")

{- | An equal-tailed credible interval from a weighted sample.

The sample is sorted by value and its normalized weights are accumulated into a
CDF; the interval endpoints are the values at which the running weight first
reaches @lo@ and @hi@ (here 2.5% and 97.5%). This is a summary of the
importance-sampling output, the standard weighted-quantile read of a posterior,
and it is the only quantity the CLI computes from the weighted sample beyond the
library's 'posteriorMean'. The total weight is recovered from the same
log-weights the library returns, so the CDF is genuinely normalized.
-}
weightedInterval :: Double -> Double -> V.Vector (Double, Double) -> (Double, Double)
weightedInterval lo hi samples
    -- An empty sample has no quantiles, and maximum [] and last [] both crash.
    -- The CLI validates particles > 0 so this is unreachable in normal use, but
    -- a quantile of nothing is NaN, not a panic, if it ever is reached.
    | V.null samples = (0 / 0, 0 / 0)
    | otherwise = (quantileAt lo, quantileAt hi)
  where
    -- Sort (value, weight) pairs by value, exponentiating each log-weight
    -- relative to the max so the running sum is stable and never underflows.
    sorted = sortOn fst (V.toList samples)
    wmax = maximum (map snd sorted)
    scaled = [(x, exp (w - wmax)) | (x, w) <- sorted]
    total = sum (map snd scaled)
    -- The value at which the normalized running weight first reaches p.
    quantileAt p = walk 0 scaled
      where
        target = p * total
        walk _ [] = fst (last scaled)
        walk acc ((x, w) : rest)
            | acc + w >= target = x
            | otherwise = walk (acc + w) rest

{- | Print @error: <msg>@ to stderr and exit 1.

A bad invocation has to be detectable by the shell or CI, so a usage error goes
to stderr with a non-zero status rather than to stdout where it would read as
normal output and exit 0.
-}
dieWith :: String -> IO a
dieWith msg = do
    hPutStrLn stderr ("error: " <> msg)
    exitWith (ExitFailure 1)

{- | Reject parameters outside their domain before any inference runs.

The Beta prior needs @a, b > 0@ to be a proper density, and the particle count
must be positive or the importance sample is empty and its summaries (the
weighted quantile's @maximum@ over no weights) are undefined. Catching this here
turns a downstream crash or a NaN into a clear usage error.
-}
validate :: Options -> IO ()
validate opts
    | optParticles opts <= 0 = dieWith "particles must be a positive integer"
    | optPriorA opts <= 0 = dieWith "prior-a must be positive"
    | optPriorB opts <= 0 = dieWith "prior-b must be positive"
    | otherwise = pure ()

main :: IO ()
main = do
    opts <- execParser optionsInfo
    validate opts
    case parseFlips (optFlips opts) of
        Left err -> putStrLn ("error: " <> err)
        Right flips -> do
            let a = optPriorA opts
                b = optPriorB opts
                n = optParticles opts
                heads = fromIntegral (length (filter id flips)) :: Double
                tails = fromIntegral (length (filter not flips)) :: Double
            -- Reuse the library: build the model and run importance sampling.
            -- No inference math lives in this module.
            gen <- createSystemRandom
            samples <- importanceSamples n (coinModel a b flips) gen
            let estMean = posteriorMean samples id
                estVar = posteriorMean samples (\x -> x * x) - estMean * estMean
                (loCI, hiCI) = weightedInterval 0.025 0.975 samples
                -- Closed-form Beta(a + heads, b + tails) posterior moments.
                aPost = a + heads
                bPost = b + tails
                anaMean = aPost / (aPost + bPost)
                anaVar =
                    (aPost * bPost)
                        / ((aPost + bPost) * (aPost + bPost) * (aPost + bPost + 1))
            putStrLn "credence: coin posterior by importance sampling"
            printf "  flips        : %d heads, %d tails\n" (round heads :: Int) (round tails :: Int)
            printf "  prior        : Beta(%.3f, %.3f)\n" a b
            printf "  particles    : %d\n" n
            putStrLn ""
            printf "  posterior    : Beta(%.3f, %.3f) (analytic)\n" aPost bPost
            putStrLn "                  estimated         analytic"
            printf "  mean         : %14.6f   %14.6f\n" estMean anaMean
            printf "  std dev      : %14.6f   %14.6f\n" (sqrt estVar) (sqrt anaVar)
            printf "  95%% interval : [%.6f, %.6f]   (equal-tailed, estimated)\n" loCI hiCI
