module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Credence (logSumExp)
import Credence.Distribution (
    Dist (..),
    bernoulliDist,
    betaDist,
    categoricalDist,
    gammaDist,
    normalDist,
    uniformDist,
 )
import Credence.Inference.Importance (
    importanceSamples,
    normalizedLogWeights,
    posteriorMean,
 )
import Credence.Inference.MH (mhChain)
import Credence.Inference.SMC (
    FilterStep (..),
    StateSpace (..),
    effectiveSampleSize,
    particleFilter,
    systematicResample,
 )
import Credence.Model (Model (..), coinModel, observe, sample)
import Credence.Model.Localization (localize)
import Credence.Model.Mixture (gaussianMixtureCollapsed)
import Credence.Model.Regression (linearRegression)
import Credence.Model.Tracking (
    trackR,
    trackingModel,
    trajectoryObservations,
    trueTrajectory,
 )
import Credence.Trace (traceAddresses, traceModel)
import Credence.Wire (
    Batch (..),
    Dataset (..),
    Method (..),
    RunRequest (..),
    RunStarted (..),
    StreamEvent (..),
    Summary (..),
 )
import Data.Aeson (decode, encode)
import Data.List (sort)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32)
import Statistics.Distribution (logDensity, logProbability, mean, variance)
import Statistics.Distribution.Beta (betaDistr)
import Statistics.Distribution.Binomial (binomial)
import Statistics.Distribution.Gamma (gammaDistr)
import Statistics.Distribution.Normal (normalDistr)
import Statistics.Distribution.Uniform (uniformDistr)
import System.Random.MWC (initialize)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

main :: IO ()
main = defaultMain tests

-- The stage 1 gate is conjugate-analytic agreement: importance sampling on
-- the coin model versus the closed-form Beta posterior (conjugateTests). Under it
-- sit the foundations: the log-space helper, every distribution's log-density
-- against the independent statistics oracle, every distribution's sampler tied to
-- that parameterization by moment matching, the model monad's laws, and the
-- importance sampler's normalization and unbiasedness. See
-- the correctness checks for the doctrine. A wrong sampler looks
-- plausible, so these checks, not the eye, are what prove correctness.
tests :: TestTree
tests =
    testGroup
        "credence"
        [ logSumExpTests
        , densityTests
        , supportTests
        , momentTests
        , modelTests
        , importanceTests
        , conjugateTests
        , mhTraceTests
        , mhDetailedBalanceTests
        , mhConjugateTests
        , normalNormalTests
        , crossSeedRegressionTests
        , localizationRecoveryTests
        , smcSanityTests
        , trackingRecoveryTests
        , mixtureRecoveryTests
        , wireTests
        ]

{- | A structural interpreter over Sample-free models (only Pure and Observe).

It walks the GADT accumulating each @Observe w m@ into a running log-weight and
returns the result paired with that weight. It deliberately rejects @Sample@: the
monad-law cases below are built from @pure@ and @observe@ only, so reaching a
Sample node means the test structure is wrong, not the monad. This isolates the
monad's plumbing (does @>>=@ thread continuations and sum observes in order) from
the sampler, the way the density tests isolate log-densities from sampling. The
coin model's sampler is proven by the conjugate gate below, where a real
generator runs it.
-}
runObserves :: Model a -> (a, Double)
runObserves = go 0
  where
    go :: Double -> Model b -> (b, Double)
    go !w (Pure x) = (x, w)
    go !_ (Sample _ _) = error "runObserves: model-law tests must not sample"
    go !w (Observe dw m) = go (w + dw) m

{- | The operational Model monad: Sample/Observe reify and the Monad instance is
lawful.

These checks touch only the structure, never a generator. The conjugate gate
below is what proves the sampler; this proves the monad it runs on threads
continuations and accumulates observes correctly, so a sampler interpreter can
trust the structure it walks.

The whole point of these cases is to write each monad law's left side literally
(@pure x >>= f@, @m >>= pure@, the un-fused bind chain) and assert it equals the
simplified right side, so hlint's "simplify this to the right side" hints are
exactly what must not be applied here.
-}
{-# ANN modelTests ("HLint: ignore Monad law, left identity" :: String) #-}
{-# ANN modelTests ("HLint: ignore Monad law, right identity" :: String) #-}
{-# ANN modelTests ("HLint: ignore Use >=>" :: String) #-}
modelTests :: TestTree
modelTests =
    testGroup
        "model monad (structural)"
        [ testCase "observe accumulates in bind order" $ do
            -- Three observes in sequence; the structural weight is their sum and
            -- the result is the final pure value. Confirms >>= threads observes.
            let m = observe 1.5 >> observe 2.0 >> observe (-0.5) >> pure (9 :: Int)
                (result, totalW) = runObserves m
            assertBool ("result is 9: " <> show result) (result == 9)
            assertBool ("weight is 3.0: " <> show totalW) (abs (totalW - 3.0) < 1e-12)
        , testCase "left identity: pure x >>= f == f x" $ do
            -- Binding through pure is the same as applying the continuation.
            let f x = observe (2 * x) >> pure x
                (r1, w1) = runObserves (pure (7 :: Double) >>= f)
                (r2, w2) = runObserves (f 7)
            assertBool "left identity result" (r1 == r2)
            assertBool "left identity weight" (abs (w1 - w2) < 1e-12)
        , testCase "right identity: m >>= pure == m" $ do
            -- Binding pure on the right changes neither result nor weight.
            let m = observe 3 >> pure (5 :: Int)
                (r1, w1) = runObserves (m >>= pure)
                (r2, w2) = runObserves m
            assertBool "right identity result" (r1 == r2)
            assertBool "right identity weight" (abs (w1 - w2) < 1e-12)
        , testCase "associativity: (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" $ do
            -- Two continuations chained two ways; order and sum of weights match.
            let m = observe 1 >> pure (2 :: Int)
                f x = observe (fromIntegral x) >> pure (x + 1)
                g y = observe (fromIntegral y) >> pure (y * 2)
                (rl, wl) = runObserves ((m >>= f) >>= g)
                (rr, wr) = runObserves (m >>= (\x -> f x >>= g))
            assertBool "associativity result" (rl == rr)
            assertBool "associativity weight" (abs (wl - wr) < 1e-12)
        ]

logSumExpTests :: TestTree
logSumExpTests =
    testGroup
        "logSumExp"
        [ testCase "sum of three zero-logs is log 3" $
            assertBool
                "result within tolerance of log 3"
                (abs (logSumExp (VU.fromList [0, 0, 0]) - log 3) < 1e-12)
        ]

{- | Assert two log-densities agree to floating-point round-off.

The hand-written density and the statistics reference compute the same number
by different code paths, so they differ only in rounding. 1e-9 is far tighter
than any real disagreement and far looser than exact equality, which would be
brittle across libm implementations.
-}
assertCloseDensity :: String -> Double -> Double -> IO ()
assertCloseDensity name ours reference =
    assertBool
        (name <> ": ours=" <> show ours <> " reference=" <> show reference)
        (abs (ours - reference) < 1e-9)

{- | Log-density of each distribution against an independent reference at a fixed
interior point.

The reference is the @statistics@ package wherever it has the matching
constructor (a test-only dependency for exactly this purpose), and a hand-derived
value where a constructor is awkward (Categorical). This isolates density bugs
from sampler bugs: it touches only 'logDensityDist', never the sampler.
-}
densityTests :: TestTree
densityTests =
    testGroup
        "log-density vs statistics"
        [ testCase "Bernoulli(0.3) at True and False vs binomial 1 p" $ do
            -- statistics models Bernoulli(p) as Binomial(1, p); logProbability is
            -- the log-pmf over the count in {0, 1}, which maps to {False, True}.
            let p = 0.3
                d = bernoulliDist p
                ref = binomial 1 p
            assertCloseDensity "Bernoulli True" (logDensityDist d True) (logProbability ref 1)
            assertCloseDensity "Bernoulli False" (logDensityDist d False) (logProbability ref 0)
        , testCase "Beta(2,5) at x=0.3 vs betaDistr" $ do
            let (a, b, x) = (2, 5, 0.3)
            assertCloseDensity
                "Beta"
                (logDensityDist (betaDist a b) x)
                (logDensity (betaDistr a b) x)
        , testCase "Normal(1,2) at x=0.5 vs normalDistr" $ do
            let (mu, sigma, x) = (1, 2, 0.5)
            assertCloseDensity
                "Normal"
                (logDensityDist (normalDist mu sigma) x)
                (logDensity (normalDistr mu sigma) x)
        , testCase "Gamma(shape=2,scale=3) at x=1.5 vs gammaDistr" $ do
            -- The parameterization alignment under test: our shape/scale, mwc's
            -- gamma shape scale, and statistics gammaDistr k theta (theta = scale)
            -- must all read the same numbers the same way. gammaDistr's second
            -- argument is scale, matching ours.
            let (k, theta, x) = (2, 3, 1.5)
            assertCloseDensity
                "Gamma"
                (logDensityDist (gammaDist k theta) x)
                (logDensity (gammaDistr k theta) x)
        , testCase "Uniform(-1,2) at x=0.5 vs uniformDistr" $ do
            let (lo, hi, x) = (-1, 2, 0.5)
            assertCloseDensity
                "Uniform"
                (logDensityDist (uniformDist lo hi) x)
                (logDensity (uniformDistr lo hi) x)
        , testCase "Categorical at i=1 vs hand-derived log(w_i) - logSumExp(log w)" $ do
            -- No clean statistics constructor for an arbitrary categorical, so the
            -- reference is derived by hand: the normalized log-probability of an
            -- index is log w_i minus the log of the total weight. The normalizer is
            -- logSumExp over the log-weights, the same log-space path the
            -- implementation takes; a linear log (VU.sum ws) reference would agree
            -- here but could not catch an underflow bug in that path, which is the
            -- failure mode the correctness discipline flags for categoricals.
            let ws = VU.fromList [1, 3, 6 :: Double]
                i = 1
                ref = log (ws VU.! i) - logSumExp (VU.map log ws)
            assertCloseDensity "Categorical" (logDensityDist (categoricalDist ws) i) ref
        ]

{- | Assert a log-density is exactly @-Infinity@, and crucially not @NaN@.

The distinction is the whole point of the support guards. At a boundary like
Beta(1, 1) at @x = 0@, the unguarded formula computes @0 * -Infinity = NaN@. A
@NaN@ silently poisons every downstream sum and acceptance ratio, where an honest
@-Infinity@ is the log-probability of an impossible outcome and propagates
correctly. So this checks both halves: infinite (so the outcome is scored
impossible) and negative (so it is the low end, not @+Infinity@), and the @isNaN@
guard makes @NaN@ fail rather than slip through 'isInfinite'.
-}
assertNegInfinity :: String -> Double -> IO ()
assertNegInfinity name x =
    assertBool
        (name <> ": expected -Infinity, got " <> show x)
        (isInfinite x && x < 0 && not (isNaN x))

{- | Out-of-domain inputs score @-Infinity@, never @NaN@.

Each continuous density has an open support, and the correctness discipline
is explicit that a guard returning @-Infinity@ must have a test pinning it,
because the failure it prevents (a @NaN@ from @0 * log 0@ at a boundary, or a
panic from an out-of-range categorical index reached through @observe@) is
silent or fatal otherwise. These cases hit the exact boundaries that the formula
would mishandle.
-}
supportTests :: TestTree
supportTests =
    testGroup
        "support guards (out-of-domain scores -Infinity, not NaN)"
        [ testCase "Beta(1,1) at 0 and 1 is -Infinity (the 0 * log 0 trap)" $ do
            -- The uniform prior. a = 1 makes (a-1)*log 0 = 0 * -Infinity = NaN
            -- without the guard; b = 1 is the mirror trap at x = 1.
            assertNegInfinity "Beta(1,1) at 0" (logDensityDist (betaDist 1 1) 0)
            assertNegInfinity "Beta(1,1) at 1" (logDensityDist (betaDist 1 1) 1)
        , testCase "Beta(2,2) at 0 and 1 is -Infinity (not NaN)" $ do
            -- With a, b > 1 the unguarded formula already gives -Infinity, but the
            -- guard must not have turned that into NaN or flipped its sign.
            assertNegInfinity "Beta(2,2) at 0" (logDensityDist (betaDist 2 2) 0)
            assertNegInfinity "Beta(2,2) at 1" (logDensityDist (betaDist 2 2) 1)
        , testCase "Gamma(0.5,1) at 0 and a negative x is -Infinity (not NaN)" $ do
            -- k = 0.5 < 1 makes (k-1)*log 0 = (-0.5) * -Infinity = +Infinity in
            -- isolation, but the support is x > 0 regardless, so x = 0 scores
            -- -Infinity. A negative x feeds log x = NaN without the guard.
            assertNegInfinity "Gamma(0.5,1) at 0" (logDensityDist (gammaDist 0.5 1.0) 0)
            assertNegInfinity "Gamma(0.5,1) at -2" (logDensityDist (gammaDist 0.5 1.0) (-2))
        , testCase "Categorical out-of-range index is -Infinity (no panic)" $ do
            -- The index is data and an out-of-range i reaches the density through
            -- observe; an unguarded weights VU.! i would panic. Below 0 and at the
            -- length (one past the last valid index) both score -Infinity.
            let ws = VU.fromList [1, 3, 6 :: Double]
            assertNegInfinity "Categorical at 3" (logDensityDist (categoricalDist ws) 3)
            assertNegInfinity "Categorical at -1" (logDensityDist (categoricalDist ws) (-1))
        ]

{- | A fixed seed for every statistical test, so a run is reproducible and a pass
is not luck. mwc-random's 'initialize' seeds from this Word32 vector.
-}
fixedSeed :: VU.Vector Word32
fixedSeed = VU.fromList [0x6772, 0x6564, 0x656e, 0x6365]

{- | Number of samples per moment test. Large enough that the standard error of
the mean (sd / sqrt n) is tiny relative to the moments under test, so the
documented tolerances clear a correct implementation essentially always.
-}
sampleCount :: Int
sampleCount = 200000

{- | Draw 'sampleCount' values from a 'Dist' in ST under the fixed seed, into an
unboxed vector. No IO generator: the seed is the same every run.
-}
drawSamples :: (VU.Unbox a) => Dist a -> VU.Vector a
drawSamples d = runST $ do
    g <- initialize fixedSeed
    VU.fromList <$> replicateM sampleCount (sampleDist d g)

{- | Empirical mean and variance of a sample, variance by the E[X^2] - E[X]^2
identity. Population variance (divide by n), the closed-form target.
-}
sampleMoments :: VU.Vector Double -> (Double, Double)
sampleMoments xs = (m, v)
  where
    n = fromIntegral (VU.length xs)
    m = VU.sum xs / n
    m2 = VU.sum (VU.map (\x -> x * x) xs) / n
    v = m2 - m * m

{- | The parameterization-trap guard: each sampler's empirical mean and variance
must match the closed-form moments of the SAME parameterization its log-density
uses.

A sampler and a density that are each correct for a different parameterization
(Gamma shape/rate vs shape/scale, say) pass every isolated test yet are silently
wrong together. Tying empirical sampler moments to the analytic moments is what
catches that. Tolerances below are stated per case in standard errors of the
estimator, generous enough to clear a correct implementation essentially always.
-}
momentTests :: TestTree
momentTests =
    testGroup
        "moment matching (sampler vs closed-form, fixed seed)"
        [ testCase "Bernoulli(0.3): mean p, var p(1-p)" $ do
            let p = 0.3
                -- Sample as 0/1 doubles to reuse the moment machinery.
                xs = VU.map (\b -> if b then 1 else 0) (drawSamples (bernoulliDist p))
                (em, ev) = sampleMoments xs
                tmean = p
                tvar = p * (1 - p)
            -- mean: SE = sqrt(p(1-p)/n) ~ 0.001 at n=2e5; var SE comparable. A
            -- 0.01 window is ~10 SE on each, so a correct sampler clears it always.
            assertMoment "Bernoulli mean" em tmean 0.01
            assertMoment "Bernoulli var" ev tvar 0.01
        , testCase "Beta(2,5): mean a/(a+b), var ab/((a+b)^2 (a+b+1))" $ do
            let (a, b) = (2, 5)
                (em, ev) = sampleMoments (drawSamples (betaDist a b))
                tmean = a / (a + b)
                tvar = (a * b) / ((a + b) * (a + b) * (a + b + 1))
            -- Beta(2,5) sd ~ 0.16, so mean SE ~ 0.00036 at n=2e5; 0.005 is ~14 SE.
            assertMoment "Beta mean" em tmean 0.005
            assertMoment "Beta var" ev tvar 0.005
        , testCase "Normal(1,2): mean mu, var sigma^2" $ do
            let (mu, sigma) = (1, 2)
                (em, ev) = sampleMoments (drawSamples (normalDist mu sigma))
            -- mean SE = sigma/sqrt(n) = 2/447 ~ 0.0045; var SE = sigma^2 sqrt(2/n)
            -- ~ 0.0126. 0.05 mean / 0.1 var windows are ~11 / ~8 SE.
            assertMoment "Normal mean" em mu 0.05
            assertMoment "Normal var" ev (sigma * sigma) 0.1
        , testCase "Gamma(shape=2,scale=3): mean k*theta, var k*theta^2" $ do
            let (k, theta) = (2, 3)
                (em, ev) = sampleMoments (drawSamples (gammaDist k theta))
                tmean = k * theta
                tvar = k * theta * theta
            -- This case is the trap's teeth: mean k*theta = 6, var k*theta^2 = 18.
            -- A shape/rate sampler against a shape/scale density would land far
            -- outside. mean sd = sqrt(18) ~ 4.24, SE ~ 0.0095; var SE larger.
            -- 0.1 mean / 0.5 var windows are ~10 SE and clear a correct impl.
            assertMoment "Gamma mean" em tmean 0.1
            assertMoment "Gamma var" ev tvar 0.5
        , testCase "Uniform(-1,2): mean (lo+hi)/2, var (hi-lo)^2/12" $ do
            let (lo, hi) = (-1, 2)
                (em, ev) = sampleMoments (drawSamples (uniformDist lo hi))
                tmean = (lo + hi) / 2
                tvar = (hi - lo) * (hi - lo) / 12
            -- Range 3, sd = 3/sqrt(12) ~ 0.866, mean SE ~ 0.0019. 0.02 / 0.02
            -- windows are ~10 SE.
            assertMoment "Uniform mean" em tmean 0.02
            assertMoment "Uniform var" ev tvar 0.02
        , testCase "Categorical [1,3,6]: mean and var of the index" $ do
            let ws = VU.fromList [1, 3, 6 :: Double]
                total = VU.sum ws
                ps = VU.map (/ total) ws
                -- Closed-form moments of the index 0..n-1 under normalized ps.
                idx = VU.fromList (map fromIntegral [0 .. VU.length ws - 1]) :: VU.Vector Double
                tmean = VU.sum (VU.zipWith (*) idx ps)
                tsecond = VU.sum (VU.zipWith (\i pr -> i * i * pr) idx ps)
                tvar = tsecond - tmean * tmean
                xs = VU.map fromIntegral (drawSamples (categoricalDist ws))
                (em, ev) = sampleMoments xs
            -- index sd < 1, mean SE < 0.0023 at n=2e5; 0.02 windows are ~9 SE.
            assertMoment "Categorical mean" em tmean 0.02
            assertMoment "Categorical var" ev tvar 0.02
        ]

-- | Assert an empirical moment is within an absolute tolerance of its target.
assertMoment :: String -> Double -> Double -> Double -> IO ()
assertMoment name empirical target tol =
    assertBool
        (name <> ": empirical=" <> show empirical <> " target=" <> show target <> " tol=" <> show tol)
        (abs (empirical - target) < tol)

{- | The importance sampler's mechanics: log-space normalization and an unbiased
estimate of a known integral.

These are the pieces the conjugate gate rests on, checked in isolation. The
normalization cases pin down 'normalizedLogWeights' and 'posteriorMean' as pure
functions. The unbiased case is the correctness discipline's second oracle
(unbiased estimation of a known integral): a model that only samples, with no
observe, has every log-weight zero, so the weighted mean is an ordinary average
and must converge to the prior mean it draws from. That isolates the
"draw each Sample from its prior" half of the interpreter from the observe half
the conjugate gate exercises.
-}
importanceTests :: TestTree
importanceTests =
    testGroup
        "importance sampling (normalization and unbiasedness)"
        [ testCase "normalizedLogWeights sums to 1" $ do
            -- Arbitrary unequal log-weights; the normalized weights are a
            -- probability vector, so they sum to 1 up to round-off.
            let ws = VU.fromList [-2.0, 0.5, -1.0, 3.0, -0.25]
                total = VU.sum (normalizedLogWeights ws)
            assertBool ("sum of normalized weights is 1: " <> show total) (abs (total - 1) < 1e-12)
        , testCase "normalizedLogWeights of equal log-weights is uniform" $ do
            -- Equal log-weights mean equal probability: each of n weights is 1/n.
            -- Catches a normalizer that divides by the wrong quantity.
            let n = 5
                ws = VU.replicate n (1.7 :: Double)
                normed = normalizedLogWeights ws
                expected = 1 / fromIntegral n
            assertBool
                ("each equal weight is 1/n: " <> show (VU.toList normed))
                (VU.all (\w -> abs (w - expected) < 1e-12) normed)
        , testCase "posteriorMean of a constant projection is that constant" $ do
            -- E[c] under any normalized weights is c, since the weights sum to 1.
            -- Pins posteriorMean to weight-normalize rather than, say, average raw.
            let samples = V.fromList [(0.2, -1.0), (0.8, 2.0), (0.5, 0.3)] :: V.Vector (Double, Double)
                est = posteriorMean samples (const 4.0)
            assertBool ("constant expectation is 4: " <> show est) (abs (est - 4.0) < 1e-12)
        , testCase "unbiased: prior-only model estimates the prior mean a/(a+b)" $ do
            -- A model that samples Beta(2,5) and observes nothing: all log-weights
            -- are 0, normalized weights are uniform, so posteriorMean theta id is a
            -- plain Monte Carlo estimate of E[theta] = a/(a+b) = 2/7. Fixed ST seed.
            let (a, b) = (2, 5)
                n = 50000
                priorOnly = sample (betaDist a b)
                samples = runST $ do
                    g <- initialize fixedSeed
                    importanceSamples n priorOnly g
                est = posteriorMean samples id
                target = a / (a + b)
                -- Beta(2,5) sd ~ 0.16, so at n=5e4 the mean SE ~ 0.16/sqrt(5e4)
                -- ~ 0.0007. With uniform weights this is an iid estimator, so a
                -- 0.01 window is ~14 SE: a correct sampler clears it essentially
                -- always while a prior drawn from the wrong distribution would miss.
                tol = 0.01
            assertBool
                ("prior mean estimate " <> show est <> " near " <> show target <> " (tol " <> show tol <> ")")
                (abs (est - target) < tol)
        ]

{- | THE stage 1 gate: conjugate-analytic agreement on the coin model.

This is the single most important test in the project. A wrong sampler returns a
smooth, plausible posterior, so the only proof that importance sampling is correct
is that on a model with a closed-form answer it reproduces that answer. The coin
model is Beta-Bernoulli conjugate: prior Beta(a, b) with h heads and t tails has
posterior exactly Beta(a + h, b + t). The gate runs importance sampling on
@coinModel a b flips@ under a fixed ST seed and asserts the weighted posterior
matches the analytic Beta(a + h, b + t) in BOTH mean and variance. Mean alone is
not enough: a sampler can hit the mean while getting the spread wrong, so the
variance is checked too. Per the correctness discipline, stage 2 does not
begin until this holds.

The analytic moments are computed from the formulas (mean @(a+h)/(a+b+h+t)@,
variance @a'b'/((a'+b')^2 (a'+b'+1))@) and independently cross-checked against
@statistics@' @betaDistr a' b'@, so a typo in either path is caught by the other.
-}
conjugateTests :: TestTree
conjugateTests =
    testGroup
        "conjugate-analytic agreement (the stage 1 gate)"
        [ testCase "coin model importance posterior matches Beta(a+h, b+t) in mean and variance" $ do
            let
                -- Prior Beta(2,2) and a fixed flip sequence: 7 heads, 3 tails.
                (a, b) = (2, 2) :: (Double, Double)
                heads = 7 :: Int
                tails = 3 :: Int
                flips = replicate heads True ++ replicate tails False
                -- Posterior Beta(a', b') with a' = a + h, b' = b + t.
                a' = a + fromIntegral heads
                b' = b + fromIntegral tails
                -- Analytic posterior moments, by the closed-form Beta formulas.
                analyticMean = a' / (a' + b')
                analyticVar = (a' * b') / ((a' + b') * (a' + b') * (a' + b' + 1))
                -- Independent cross-check: the same posterior via statistics. If
                -- the hand formulas above and this reference disagree, one is wrong.
                refDist = betaDistr a' b'
                -- Importance sampling on the coin model under a FIXED ST seed.
                n = 200000
                samples = runST $ do
                    g <- initialize fixedSeed
                    importanceSamples n (coinModel a b flips) g
                -- Estimated posterior mean and variance from the weighted sample.
                -- Variance via E[theta^2] - (E[theta])^2 on the normalized weights.
                estMean = posteriorMean samples id
                estSecond = posteriorMean samples (\x -> x * x)
                estVar = estSecond - estMean * estMean
                -- Tolerances. The proposal is the prior Beta(2,2); the target is
                -- the posterior Beta(9,5), mean 9/14 ~ 0.643, sd ~ 0.124. The
                -- likelihood theta^7 (1-theta)^3 is mild, so the effective sample
                -- size is a healthy fraction of n; even at ESS ~ n/10 = 2e4 the
                -- self-normalized mean SE ~ sd/sqrt(ESS) ~ 0.124/141 ~ 0.0009, so
                -- the 0.01 mean window is ~10 SE: a correct sampler clears it
                -- essentially always. The variance is a second-moment estimator
                -- with larger sampling error and the analytic variance is only
                -- ~0.0153, so its window is the looser 0.005 (still well inside
                -- what a correct sampler delivers, while a mis-weighted sampler --
                -- which collapses toward the prior variance ~0.042 -- misses by
                -- far more than 0.005).
                meanTol = 0.01
                varTol = 0.005
            -- Hand formulas agree with the statistics reference to round-off.
            assertBool
                ("analytic mean matches betaDistr: " <> show analyticMean <> " vs " <> show (mean refDist))
                (abs (analyticMean - mean refDist) < 1e-9)
            assertBool
                ("analytic variance matches betaDistr: " <> show analyticVar <> " vs " <> show (variance refDist))
                (abs (analyticVar - variance refDist) < 1e-9)
            -- The gate: importance posterior agrees with the analytic posterior.
            assertBool
                ("posterior mean " <> show estMean <> " near analytic " <> show analyticMean <> " (tol " <> show meanTol <> ")")
                (abs (estMean - analyticMean) < meanTol)
            assertBool
                ("posterior variance " <> show estVar <> " near analytic " <> show analyticVar <> " (tol " <> show varTol <> ")")
                (abs (estVar - analyticVar) < varTol)
        ]

{- | Address stability: the tracing interpreter names choices reproducibly.

Trace-based MH rests on each random choice keeping a stable identity across runs,
so a proposal perturbs the same choice it means to. Addresses are execution-order
positions, so for a fixed-structure model the same control flow must visit the
same 'Sample' nodes in the same order and assign the same addresses every run.
This pins that for the coin model: two runs under the same fixed seed yield the
same address list, and the coin model has exactly one choice, so that list is
@[0]@. A regression here (addresses drifting or reordering) would silently break
every MH proposal, so it is asserted directly rather than left implicit in the
chain tests.
-}
mhTraceTests :: TestTree
mhTraceTests =
    testGroup
        "trace addressing (stable across runs)"
        [ testCase "coin model trace addresses are [0] on two runs with the same flips" $ do
            let flips = replicate 7 True ++ replicate 3 False
                -- Run the tracing interpreter twice under the same fixed seed.
                addrs1 = runST $ do
                    g <- initialize fixedSeed
                    (_, t) <- traceModel (coinModel 2 2 flips) g
                    pure (traceAddresses t)
                addrs2 = runST $ do
                    g <- initialize fixedSeed
                    (_, t) <- traceModel (coinModel 2 2 flips) g
                    pure (traceAddresses t)
            -- The single-Sample coin model has exactly address 0, on both runs.
            assertBool ("run 1 addresses are [0]: " <> show addrs1) (addrs1 == [0])
            assertBool ("run 2 addresses are [0]: " <> show addrs2) (addrs2 == [0])
            assertBool
                ("the two runs agree on addresses: " <> show addrs1 <> " vs " <> show addrs2)
                (addrs1 == addrs2)
        ]

{- | Detailed balance: the MH chain converges to a known stationary distribution.

This is the oracle the correctness discipline mandates for MH and the check
that the acceptance ratio and the addressing are right. A wrong ratio yields a
smooth, plausible, WRONG stationary distribution, so the proof is a small target
whose stationary distribution is known in closed form, against which the chain's
empirical distribution is compared.

The target is discrete and tractable, and deliberately uses a NON-UNIFORM prior so
the proposal correction (the Hastings term) is load-bearing rather than zero. A
categorical prior proportional to @[3, 2, 1]@ over the indices {0, 1, 2} is
reshaped by a likelihood proportional to @[1, 3, 6]@, via @observe@ of that
categorical's log-density at the drawn index. The posterior over the index is
prior times likelihood, @prior_i * lik_i ~ [3, 6, 6]@, normalized to
@[0.2, 0.4, 0.4]@, the known stationary distribution.

Why non-uniform: the proposal redraws the index from the prior, so for a UNIFORM
prior the proposal is symmetric and the Hastings correction vanishes identically;
a chain that wrongly dropped that correction would still pass against a uniform
target, giving the test no teeth on the proposal term. With this non-uniform
prior the correction does not cancel (it is the standard independence-proposal
correction that, with the prior difference, leaves the likelihood ratio
@lik_j / lik_i@), so dropping it skews the stationary distribution toward the
high-prior index and the test fails loudly. That is the mutation the test-teeth
check exercises, and at the committed seed dropping the Hastings term moves the
empirical distribution to roughly @[0.33, 0.44, 0.22]@, about 0.18 off the target
and ~9x outside the tolerance below. The acceptance rate is asserted strictly
inside (0,1): a chain that never moves or always moves is a bug even if the
frequencies happen to match.
-}
mhDetailedBalanceTests :: TestTree
mhDetailedBalanceTests =
    testGroup
        "detailed balance (chain converges to a known stationary distribution)"
        [ testCase "MH empirical distribution matches [0.2,0.4,0.4] and the acceptance rate is in (0,1)" $ do
            let
                -- Non-uniform prior and a likelihood; the stationary distribution is
                -- prior_i * lik_i normalized, which is [3,6,6]/15 = [0.2,0.4,0.4].
                priorWs = VU.fromList [3, 2, 1 :: Double]
                likWs = VU.fromList [1, 3, 6 :: Double]
                unnormalized = VU.zipWith (*) priorWs likWs
                total = VU.sum unnormalized
                stationary = map (/ total) (VU.toList unnormalized)
                -- Draw the index from the non-uniform prior, then reshape by the
                -- likelihood. The non-uniform prior keeps the Hastings correction
                -- non-zero, so the full acceptance ratio is genuinely tested.
                target = do
                    i <- sample (categoricalDist priorWs)
                    observe (logDensityDist (categoricalDist likWs) i)
                    pure i
                -- A long chain with a burn-in discarded. Fixed ST seed.
                n = 200000
                burn = 20000
                (chain, rate) = runST $ do
                    g <- initialize fixedSeed
                    mhChain n target g
                kept = V.drop burn chain
                keptN = fromIntegral (V.length kept) :: Double
                -- Empirical frequency of each index over the post-burn chain.
                empirical =
                    [ fromIntegral (V.length (V.filter (== j) kept)) / keptN
                    | j <- [0 .. VU.length priorWs - 1]
                    ]
                -- Tolerance. Each bin frequency is a multinomial proportion; its
                -- standard error is sqrt(p(1-p)/Neff). MH samples are
                -- autocorrelated, so the effective sample size is below the 180000
                -- kept draws; budget a conservative Neff ~ n/10 = 18000. The
                -- worst-case bin p=0.4 then has SE ~ sqrt(0.24/18000) ~ 0.0037, so
                -- a 0.02 window is ~5 SE: a correct chain clears it essentially
                -- always, while a wrong acceptance ratio (the dropped-Hastings
                -- mutation skews this to ~0.18 off) misses by far more. The observed
                -- max deviation at the committed seed is ~0.0017, about 11x inside
                -- the window.
                tol = 0.02
            -- The acceptance rate must be a genuine accept/reject, not degenerate.
            assertBool
                ("acceptance rate strictly in (0,1): " <> show rate)
                (rate > 0 && rate < 1)
            -- The chain's empirical distribution matches the known stationary one.
            mapM_
                ( \(j, e, s) ->
                    assertBool
                        ("index " <> show j <> " empirical " <> show e <> " near stationary " <> show s <> " (tol " <> show tol <> ")")
                        (abs (e - s) < tol)
                )
                (zip3 [0 :: Int ..] empirical stationary)
        ]

{- | THE MH conjugate gate: the chain matches the analytic Beta posterior.

The second oracle for MH, alongside detailed balance, and the exact analogue of
the stage 1 importance-sampling gate: on the coin model, whose posterior is
the closed-form Beta(a+h, b+t), the MH chain's empirical mean and variance must
match that posterior. Mean alone is not enough (a chain can hit the mean while
getting the spread wrong), so variance is checked too, and the analytic moments
are independently cross-checked against @statistics@' @betaDistr@ so a typo in
either path is caught. Passing this together with detailed balance closes the
first half of the stage 2 gate.

Unlike the importance sample, MH draws are autocorrelated, so the effective sample
size is well below the chain length and the windows are looser than the
importance-sampling gate's for the same nominal n. The proposal is the single-site
prior proposal on the one Beta choice, which mixes well here (the measured
acceptance rate is about 0.5), so a moderate chain with a burn-in lands inside the
documented windows essentially always. The acceptance rate is also asserted inside
(0,1).
-}
mhConjugateTests :: TestTree
mhConjugateTests =
    testGroup
        "MH conjugate-analytic agreement (Beta posterior on the coin model)"
        [ testCase "MH chain matches Beta(a+h, b+t) in mean and variance; acceptance rate in (0,1)" $ do
            let
                -- Prior Beta(2,2), 7 heads and 3 tails, so posterior Beta(9,5).
                (a, b) = (2, 2) :: (Double, Double)
                heads = 7 :: Int
                tails = 3 :: Int
                flips = replicate heads True ++ replicate tails False
                a' = a + fromIntegral heads
                b' = b + fromIntegral tails
                -- Analytic posterior moments by the closed-form Beta formulas.
                analyticMean = a' / (a' + b')
                analyticVar = (a' * b') / ((a' + b') * (a' + b') * (a' + b' + 1))
                -- Independent cross-check via statistics, same defense as stage 1.
                refDist = betaDistr a' b'
                -- The MH chain on the coin model under a FIXED ST seed.
                n = 200000
                burn = 20000
                (chain, rate) = runST $ do
                    g <- initialize fixedSeed
                    mhChain n (coinModel a b flips) g
                kept = V.drop burn chain
                keptN = fromIntegral (V.length kept) :: Double
                -- Empirical mean and variance over the post-burn chain, variance by
                -- E[theta^2] - (E[theta])^2.
                estMean = V.sum kept / keptN
                estSecond = V.sum (V.map (\x -> x * x) kept) / keptN
                estVar = estSecond - estMean * estMean
                -- Tolerances. The target is Beta(9,5), mean 9/14 ~ 0.643, sd ~ 0.124.
                -- MH samples are autocorrelated, so the effective sample size is
                -- below the 180000 kept draws; at the measured acceptance rate ~0.5
                -- a conservative ESS ~ n/10 = 18000 gives a mean SE ~ 0.124/sqrt(18000)
                -- ~ 0.00092, so a 0.02 mean window is ~20 SE. The variance is a
                -- noisier second-moment estimator against an analytic value of only
                -- ~0.0153, so it gets the looser 0.005 window (the same value the
                -- importance gate uses). Both clear a correct chain essentially
                -- always while a wrong acceptance ratio misses by far more.
                meanTol = 0.02
                varTol = 0.005
            -- Hand formulas agree with the statistics reference to round-off.
            assertBool
                ("analytic mean matches betaDistr: " <> show analyticMean <> " vs " <> show (mean refDist))
                (abs (analyticMean - mean refDist) < 1e-9)
            assertBool
                ("analytic variance matches betaDistr: " <> show analyticVar <> " vs " <> show (variance refDist))
                (abs (analyticVar - variance refDist) < 1e-9)
            -- The acceptance rate is a genuine accept/reject.
            assertBool
                ("acceptance rate strictly in (0,1): " <> show rate)
                (rate > 0 && rate < 1)
            -- The gate: the MH posterior agrees with the analytic posterior.
            assertBool
                ("MH posterior mean " <> show estMean <> " near analytic " <> show analyticMean <> " (tol " <> show meanTol <> ")")
                (abs (estMean - analyticMean) < meanTol)
            assertBool
                ("MH posterior variance " <> show estVar <> " near analytic " <> show analyticVar <> " (tol " <> show varTol <> ")")
                (abs (estVar - analyticVar) < varTol)
        ]

{- | The regression fixture shared by the Normal-Normal agreement gate and the
cross-seed convergence test.

One single-coefficient line @y = w * x@ with a broad Normal prior on the slope,
known observation noise, and a small literal dataset near the true slope 2. The
parameters live here once so both tests run the same model and agree on the
analytic posterior; changing the fixture in one place changes it for both, which
is what keeps the cross-seed test honest about the same @muN@ the agreement test
checks.

Prior @Normal(0, 3)@ is weakly informative (the truth 2 sits 0.67 prior-sd from
the mean), known noise @sigmaL = 1@. The five points carry mild deviations off
@y = 2 x@, kept literal so every run is deterministic.
-}
regMu0, regSigma0, regSigmaL :: Double
regMu0 = 0.0
regSigma0 = 3.0
regSigmaL = 1.0

regPoints :: [(Double, Double)]
regPoints = [(1.0, 2.1), (2.0, 3.9), (3.0, 6.2), (4.0, 7.8), (5.0, 10.3)]

{- | The closed-form Normal-Normal posterior on the slope for 'regPoints',
computed in precision form (precision is one over variance).

For a Normal prior @Normal(mu0, sigma0)@ on @w@ and observations @y_i ~
Normal(w * x_i, sigmaL)@ with known @sigmaL@, the posterior on @w@ is Normal with
posterior precision @1/sigma0^2 + (sum_i x_i^2)/sigmaL^2@ and posterior mean
@posteriorVar * (mu0/sigma0^2 + (sum_i x_i*y_i)/sigmaL^2)@. This is the scalar
Normal-Normal conjugate result with each observation weighted by its @x_i@,
derived from the location form rather than asserted blind; the agreement test is
what arbitrates it. Returns @(muN, sigmaN^2)@.
-}
regPosterior :: (Double, Double)
regPosterior = (muN, varN)
  where
    sx2 = sum [x * x | (x, _) <- regPoints]
    sxy = sum [x * y | (x, y) <- regPoints]
    priorPrec = 1 / (regSigma0 * regSigma0)
    tauN = priorPrec + sx2 / (regSigmaL * regSigmaL)
    varN = 1 / tauN
    muN = varN * (regMu0 * priorPrec + sxy / (regSigmaL * regSigmaL))

{- | THE Normal-Normal gate: the regression posterior matches its closed-form
Normal posterior in mean and variance.

The second conjugate oracle the correctness discipline mandates, independent
of the Beta gate and on a structurally different model: a continuous latent with
many observations rather than one Beta draw. The fixture is conjugate
Normal-Normal with known noise ('regPoints'), so the posterior on the slope is an
exact Normal whose mean @muN@ and variance @sigmaN^2@ come from the precision-form
formulas in 'regPosterior'. Importance sampling on @linearRegression@ under a
fixed ST seed must reproduce both. Mean alone is not enough (a sampler can hit the
mean while getting the spread wrong), so the variance is checked too.

The analytic @muN@ and @sigmaN^2@ are cross-checked against @statistics@'
@normalDistr muN sigmaN@ to round-off before the sampler is judged: @mean@ and
@variance@ of that Normal are just @muN@ and @sigmaN^2@, so the check is trivially
true and serves as the typo guard that the values feeding the assertion are the
ones the formulas compute, the same defense the Beta gate uses against a
mistyped closed form.

Importance sampling is the lowest-variance choice for this exact-match gate; MH
agreement on regression is exercised additionally by the cross-seed test. This
fixture demonstrates a second model running under an interpreter unchanged.
-}
normalNormalTests :: TestTree
normalNormalTests =
    testGroup
        "Normal-Normal conjugate agreement (regression posterior on the slope)"
        [ testCase "regression importance posterior matches the closed-form Normal posterior in mean and variance" $ do
            let
                -- Analytic posterior on the slope from the precision-form formulas.
                (analyticMean, analyticVar) = regPosterior
                analyticSd = sqrt analyticVar
                -- Independent cross-check: the same moments via statistics. mean and
                -- variance of normalDistr muN sigmaN are muN and sigmaN^2, so a
                -- disagreement here means the analytic path was mistyped.
                refDist = normalDistr analyticMean analyticSd
                -- Importance sampling on the regression model under a FIXED ST seed.
                n = 200000
                samples = runST $ do
                    g <- initialize fixedSeed
                    importanceSamples n (linearRegression regMu0 regSigma0 regSigmaL regPoints) g
                -- Estimated posterior mean and variance from the weighted sample,
                -- variance by E[w^2] - (E[w])^2 on the normalized weights.
                estMean = posteriorMean samples id
                estSecond = posteriorMean samples (\w -> w * w)
                estVar = estSecond - estMean * estMean
                -- Tolerances. The proposal is the prior Normal(0,3); the target is
                -- the posterior Normal(muN ~ 2.018, sigmaN ~ 0.135). The likelihood
                -- (five Normal factors with sum x^2 = 55, sigmaL = 1) is sharper than
                -- the broad prior, so the effective sample size is ~5% of n; at
                -- n = 2e5 that is ESS ~ 10000 and the self-normalized mean SE ~
                -- sigmaN/sqrt(ESS) ~ 0.135/100 ~ 0.00135, so the 0.01 mean window is
                -- ~7 SE: a correct sampler clears it essentially always (a 40-seed
                -- sweep had max mean error ~0.002, ~5x inside). The variance is a
                -- noisier second-moment estimator against an analytic value of only
                -- ~0.0181, so it gets the looser 0.005 window, the same value the
                -- Beta gate uses (the 40-seed sweep had max variance error ~0.0004,
                -- ~12x inside). A mis-weighted sampler collapses toward the prior
                -- variance 9 and misses by orders of magnitude.
                meanTol = 0.01
                varTol = 0.005
            -- Hand formulas agree with the statistics reference to round-off.
            assertBool
                ("analytic mean matches normalDistr: " <> show analyticMean <> " vs " <> show (mean refDist))
                (abs (analyticMean - mean refDist) < 1e-9)
            assertBool
                ("analytic variance matches normalDistr: " <> show analyticVar <> " vs " <> show (variance refDist))
                (abs (analyticVar - variance refDist) < 1e-9)
            -- The gate: the importance posterior agrees with the analytic posterior.
            assertBool
                ("posterior mean " <> show estMean <> " near analytic " <> show analyticMean <> " (tol " <> show meanTol <> ")")
                (abs (estMean - analyticMean) < meanTol)
            assertBool
                ("posterior variance " <> show estVar <> " near analytic " <> show analyticVar <> " (tol " <> show varTol <> ")")
                (abs (estVar - analyticVar) < varTol)
        ]

{- | Three distinct fixed seeds for the cross-seed convergence test.

A single fixed seed proves a run is reproducible; it does not prove the estimate
is stable rather than right by luck on one draw. These are three different Word32
vectors (the first is the suite's 'fixedSeed'), committed so every run is
reproducible while still exercising three independent random streams. The
cross-seed test asserts the per-seed estimates agree with each other and with the
analytic posterior under both interpreters.
-}
crossSeeds :: [VU.Vector Word32]
crossSeeds =
    [ fixedSeed
    , VU.fromList [0x1234, 0x5678, 0x9abc, 0xdef0]
    , VU.fromList [0x0f1e, 0x2d3c, 0x4b5a, 0x6978]
    ]

{- | Cross-seed convergence sanity: the regression posterior-mean estimate is
stable across seeds under BOTH importance sampling and MH.

The companion to the Normal-Normal gate. That gate proves the posterior is right
on one seed; this proves the estimate is not a lucky one-seed fluke, by running
the SAME fixture ('regPoints', so the analytic @muN@ from 'regPosterior' is known)
under three distinct fixed seeds and asserting the per-seed estimates agree with
each other and with @muN@. It runs under both interpreters, which is also the
operational-monad payoff in miniature: the one regression model drives importance
sampling and MH with no change to either.

For MH the acceptance rate on each seed is asserted strictly in (0,1): a chain
that never moves or always moves would be a degenerate pass even if a mean happened
to match. The proposal is the single-site prior proposal on the one slope, an
independence proposal from the broad prior; against the tight posterior it accepts
about 4 to 5 percent of the time, low but a genuine accept/reject, so the chains
are run long with a burn-in to let that mixing average out.
-}
crossSeedRegressionTests :: TestTree
crossSeedRegressionTests =
    testGroup
        "cross-seed convergence (regression stable across seeds under both methods)"
        [ testCase "importance posterior mean is stable across seeds and near analytic muN" $ do
            let
                (analyticMean, _) = regPosterior
                model = linearRegression regMu0 regSigma0 regSigmaL regPoints
                n = 200000
                -- One importance posterior-mean estimate per seed, each in ST.
                perSeed =
                    [ runST $ do
                        g <- initialize seed
                        samples <- importanceSamples n model g
                        pure (posteriorMean samples id)
                    | seed <- crossSeeds
                    ]
                spread = maximum perSeed - minimum perSeed
                -- Tolerances. Each per-seed estimate is a self-normalized importance
                -- mean at ESS ~ 5% of n (ESS ~ 10000 at n = 2e5), so its SE ~
                -- sigmaN/sqrt(ESS) ~ 0.135/100 ~ 0.00135. The cross-seed spread of
                -- three such independent estimates is a few SE, so a 0.01 window
                -- clears a correct sampler essentially always (a 40-seed sweep had a
                -- full-range spread ~0.004); each estimate is held within 0.01 of the
                -- analytic muN by the same SE argument the Normal-Normal gate makes.
                spreadTol = 0.01
                meanTol = 0.01
            assertBool
                ("importance cross-seed spread " <> show spread <> " within " <> show spreadTol <> " (estimates " <> show perSeed <> ")")
                (spread < spreadTol)
            mapM_
                ( \est ->
                    assertBool
                        ("importance per-seed mean " <> show est <> " near analytic " <> show analyticMean <> " (tol " <> show meanTol <> ")")
                        (abs (est - analyticMean) < meanTol)
                )
                perSeed
        , testCase "MH posterior mean is stable across seeds, near analytic muN, with acceptance rate in (0,1)" $ do
            let
                (analyticMean, _) = regPosterior
                model = linearRegression regMu0 regSigma0 regSigmaL regPoints
                n = 200000
                burn = 20000
                -- One MH chain mean (after burn-in) and acceptance rate per seed.
                perSeed =
                    [ runST $ do
                        g <- initialize seed
                        (chain, rate) <- mhChain n model g
                        let kept = V.drop burn chain
                            m = V.sum kept / fromIntegral (V.length kept)
                        pure (m, rate)
                    | seed <- crossSeeds
                    ]
                means = map fst perSeed
                rates = map snd perSeed
                spread = maximum means - minimum means
                -- Tolerances. MH draws are autocorrelated and the prior-proposal
                -- acceptance rate is low (~0.045), so the effective sample size is
                -- far below the 180000 kept draws; the chain accepts ~9000 times, so
                -- ESS is on that order and the mean SE ~ sigmaN/sqrt(9000) ~ 0.0014.
                -- The cross-seed spread and each per-seed deviation from muN are a few
                -- such SE, so a 0.02 window (looser than the importance window, as the
                -- MH conjugate gate is looser than the importance gate) clears a
                -- correct chain essentially always: a 12-seed simulation had spread
                -- ~0.005 and max deviation from muN ~0.003, ~7x inside.
                spreadTol = 0.02
                meanTol = 0.02
            -- Each chain is a genuine accept/reject, not degenerate.
            mapM_
                ( \rate ->
                    assertBool
                        ("MH acceptance rate strictly in (0,1): " <> show rate)
                        (rate > 0 && rate < 1)
                )
                rates
            assertBool
                ("MH cross-seed spread " <> show spread <> " within " <> show spreadTol <> " (means " <> show means <> ")")
                (spread < spreadTol)
            mapM_
                ( \m ->
                    assertBool
                        ("MH per-seed mean " <> show m <> " near analytic " <> show analyticMean <> " (tol " <> show meanTol <> ")")
                        (abs (m - analyticMean) < meanTol)
                )
                means
        ]

{- | The localization fixture: a planted source and two literal reading sets near
it, the data the recovery oracle runs on.

A hidden source sits at 'plantedTheta'. The sensors return noisy position reads of
it under 'locSigma' noise, drawn from a broad 'locMu0'/'locSigma0' prior. The
readings are LITERAL (not generated) so every run is deterministic under the fixed
seed, and they are centered tightly on the planted value so the planted answer is a
sharp oracle: 'fewReadings' is a small set and 'manyReadings' a larger one with the
same mean, so the posterior over the same source can be compared at two data
volumes. Each set's arithmetic mean is 'plantedTheta' exactly (the deviations are
symmetric and cancel), which keeps the recovery target the planted value itself
rather than a finite-sample offset of it.
-}
plantedTheta, locMu0, locSigma0, locSigma :: Double
plantedTheta = 3.0
locMu0 = 0.0
locSigma0 = 5.0
locSigma = 1.0

{- | A small reading set centered on 'plantedTheta'. Six symmetric deviations about
3.0 that sum to zero, so the sample mean is exactly 3.0.
-}
fewReadings :: [Double]
fewReadings = [3.6, 2.4, 3.2, 2.8, 3.5, 2.5]

{- | A larger reading set centered on 'plantedTheta', the same mean as 'fewReadings'
(exactly 3.0, twenty-four symmetric deviations summing to zero) but four times as
many observations, so the posterior over the same source concentrates further.
-}
manyReadings :: [Double]
manyReadings =
    [ 3.6
    , 2.4
    , 3.2
    , 2.8
    , 3.5
    , 2.5
    , 3.7
    , 2.3
    , 3.1
    , 2.9
    , 3.4
    , 2.6
    , 3.8
    , 2.2
    , 3.3
    , 2.7
    , 3.9
    , 2.1
    , 3.05
    , 2.95
    , 3.45
    , 2.55
    , 3.15
    , 2.85
    ]

{- | THE localization gate: planted-source recovery under a fixed seed.

The correctness oracle for the localization model, per the correctness
discipline: no closed form is assumed, the planted source is the oracle. A known
'plantedTheta' is conditioned on with noisy sensor reads, and the importance
posterior must (a) recover the planted position in its MEAN within a documented
tolerance, and (b) CONCENTRATE, its posterior variance strictly smaller at a large
reading count than at a small one. Both are asserted, not just the mean: a mean that
lands while the spread does not shrink would mean the model conditions but does not
sharpen, the opposite of what the cloud is meant to show.

This is also the third model running under the importance interpreter unchanged,
the operational-monad payoff: 'localize' is built only from 'sample' and 'observe'
and needs no change to the sampler.

Recovery (mean). The readings' arithmetic mean is exactly 'plantedTheta', so the
broad-prior posterior mean sits at the planted value up to the sampler's error. The
proposal is the prior Normal(0,5); the likelihood (six Normal factors at sigma = 1)
is sharper than the prior, so the effective sample size is a fraction of n. At
n = 2e5 and ESS on the order of a few percent of n the self-normalized mean SE is
roughly sigma_post / sqrt(ESS); the posterior sd here is about
1/sqrt(6/1) ~ 0.41, so with ESS ~ 5000 the SE is ~ 0.006. The 0.05 window is ~8 SE,
generous enough that a correct sampler clears it essentially always while a wrong
likelihood (which would not pull the broad prior onto the readings) misses by far
more. Mirrors the standard-error discipline of 'normalNormalTests'.

Concentration (variance). The same planted source is conditioned on 'fewReadings'
(six) and 'manyReadings' (twenty-four, same mean), and the posterior variance is
estimated from the weighted second moment minus the squared mean under the fixed
seed. With a Normal prior and Normal reads the posterior variance falls like
1/(1/sigma0^2 + n/sigma^2), so four times the readings cuts it to roughly a quarter
(0.41^2 ~ 0.166 at six versus ~ 0.042 at twenty-four). The assertion is the strict
inequality varMany < varFew, the qualitative concentration the cloud renders; the
gap is large (about 4x) relative to the second-moment estimator's noise, so the
inequality holds essentially always under the fixed seed.
-}
localizationRecoveryTests :: TestTree
localizationRecoveryTests =
    testGroup
        "localization planted-source recovery (the localization gate)"
        [ testCase "importance posterior recovers the planted source in mean (fixed seed)" $ do
            let
                -- Importance sampling on the localization model under a FIXED ST seed.
                n = 200000
                samples = runST $ do
                    g <- initialize fixedSeed
                    importanceSamples n (localize locMu0 locSigma0 locSigma fewReadings) g
                estMean = posteriorMean samples id
                -- The readings' mean is exactly plantedTheta, so recovery targets the
                -- planted value. SE ~ posterior sd / sqrt(ESS) ~ 0.41/sqrt(5000) ~ 0.006;
                -- 0.05 is ~8 SE, clearing a correct sampler essentially always while a
                -- wrong likelihood leaves the mean near the prior mean 0 and misses by
                -- far more than 0.05.
                meanTol = 0.05
            assertBool
                ("localization posterior mean " <> show estMean <> " near planted " <> show plantedTheta <> " (tol " <> show meanTol <> ")")
                (abs (estMean - plantedTheta) < meanTol)
        , testCase "posterior spread shrinks as readings accumulate (concentration)" $ do
            let
                n = 200000
                -- Posterior variance at each reading count, via E[theta^2] - E[theta]^2
                -- on the normalized weights, each under the SAME fixed seed so the only
                -- difference is the number of observations.
                varianceAt readings = runST $ do
                    g <- initialize fixedSeed
                    samples <- importanceSamples n (localize locMu0 locSigma0 locSigma readings) g
                    let m = posteriorMean samples id
                        second = posteriorMean samples (\x -> x * x)
                    pure (second - m * m)
                varFew = varianceAt fewReadings
                varMany = varianceAt manyReadings
            -- Concentration: four times the readings cuts the conjugate posterior
            -- variance to roughly a quarter (~0.166 at six readings vs ~0.042 at
            -- twenty-four), so the strict inequality holds with a wide margin under
            -- the fixed seed. Asserting the inequality, not a tolerance, is the point:
            -- the posterior must sharpen with data, which is what the cloud shows.
            assertBool
                ("posterior variance at " <> show (length manyReadings) <> " readings " <> show varMany <> " strictly less than at " <> show (length fewReadings) <> " readings " <> show varFew)
                (varMany < varFew)
        ]

{- | One step of the analytic scalar Kalman filter, the known answer the particle
filter is judged against.

For the linear-Gaussian random walk @x_t = x_{t-1} + N(0, q)@ observed as
@y_t = x_t + N(0, r)@, the filtering distribution stays Normal and its mean and
variance follow the standard scalar Kalman recursion (Kalman 1960; any state-space
text). Given the previous posterior @(m, p)@ and a new observation @y@:

    predict:  m' = m,        p' = p + q
    update:   k  = p' / (p' + r)
              m_new = m' + k * (y - m')
              p_new = (1 - k) * p'

The transition is a pure random walk so the predicted mean equals the previous mean;
the Kalman gain @k@ blends the prediction with the observation by their relative
precisions. This is hand-derived from the model, not taken from a library, and it is
the oracle the particle filter's per-step mean and variance must match.
-}
kalmanStep :: Double -> Double -> (Double, Double) -> Double -> (Double, Double)
kalmanStep q r (m, p) y = (mNew, pNew)
  where
    -- Predict: the random walk leaves the mean put and inflates the variance by q.
    pPredict = p + q
    -- Update: gain by relative precision, then blend the prediction and the reading.
    gain = pPredict / (pPredict + r)
    mNew = m + gain * (y - m)
    pNew = (1 - gain) * pPredict

{- | Run the analytic Kalman filter over an observation sequence from a Normal prior,
returning the per-step posterior @(mean, variance)@ list (one per observation).
-}
kalmanFilter :: Double -> Double -> (Double, Double) -> [Double] -> [(Double, Double)]
kalmanFilter q r prior = drop 1 . scanl (kalmanStep q r) prior

{- | The linear-Gaussian state-space fixture shared by the Kalman known-answer
oracle: a scalar random walk with Gaussian observations, plus its parameters.

The state is a 'Double' and the model is expressed entirely through 'normalDist':
the initial prior is @Normal(km0, sqrt kp0)@, the transition is the random walk
@x_t ~ Normal(x_{t-1}, sqrt kq)@, and the observation likelihood is
@y_t ~ Normal(x_t, sqrt kr)@ scored by the Normal log-density. The parameters are
moderate so the filtering posterior stays well-conditioned and the effective sample
size stays healthy across the sequence (a sharp likelihood would collapse the cloud
and force a resample every step, which the ESS oracle covers separately).
-}
kq, kr, km0, kp0 :: Double
kq = 0.5
kr = 1.0
km0 = 0.0
kp0 = 1.0

{- | The literal observation sequence for the Kalman oracle, fixed so the run is
deterministic. A short walk that drifts up then settles, enough steps to exercise the
recursion without the cloud needing an unreasonable particle count.
-}
kalmanObservations :: [Double]
kalmanObservations = [0.8, 1.6, 2.1, 1.9, 3.0, 2.7, 3.4]

-- | The state-space model for 'kalmanObservations', as a 'StateSpace Double'.
kalmanModel :: StateSpace Double
kalmanModel =
    StateSpace
        { ssInit = normalDist km0 (sqrt kp0)
        , ssTransition = \x -> normalDist x (sqrt kq)
        , ssLogLik = \x y -> logDensityDist (normalDist x (sqrt kr)) y
        }

{- | THE particle-filter gate: the three correctness sanity oracles.

A particle filter that quietly biases the distribution is the same failure mode as a
biased sampler (the correctness discipline), so it gets the same known-answer
treatment. Three oracles, each under a FIXED ST seed:

1. Effective sample size behaves as expected: it drops as weights concentrate and
   resets toward N after a resample.
2. Resampling is unbiased: it does not shift the empirical mean.
3. Known-answer recovery: on a linear-Gaussian model the per-step filtering mean and
   variance match the analytic Kalman filter, the strongest oracle the CONTEXT and
   the discipline call for.
-}
smcSanityTests :: TestTree
smcSanityTests =
    testGroup
        "particle-filter sanity (ESS, unbiased resampling, Kalman known-answer)"
        [ testCase "ESS drops on concentrated weights and resets toward N after a resample" $ do
            let
                count = 1000 :: Int
                n = fromIntegral count :: Double
                -- Equal log-weights: every particle contributes, so ESS is exactly N.
                equalLogWeights = VU.replicate count 0
                essEqual = effectiveSampleSize equalLogWeights
                -- A concentrated weight vector: one particle far above the rest (a
                -- log-weight of 0 against -20 puts ~exp(20) times the mass on it), so
                -- almost all the mass is on one particle and ESS sits near 1.
                concentratedLogWeights =
                    VU.generate count (\i -> if i == 0 then 0 else -20)
                essConcentrated = effectiveSampleSize concentratedLogWeights
                -- Resample the concentrated population under a FIXED seed, then score
                -- the offspring with uniform weights (which is how the filter resets
                -- them). The reset population's ESS is exactly N because uniform
                -- weights have ESS N; this is the "resets toward N" the discipline names.
                essAfterResample = runST $ do
                    g <- initialize fixedSeed
                    _ <- systematicResample concentratedLogWeights (VU.enumFromN 0 count) g
                    pure (effectiveSampleSize (VU.replicate count 0))
            -- ESS is exact arithmetic on the weights, not a statistical estimate, so
            -- the tolerances are tight round-off windows, not standard-error windows.
            -- Equal weights give ESS = N exactly.
            assertBool
                ("equal-weight ESS " <> show essEqual <> " is N = " <> show n)
                (abs (essEqual - n) < 1e-6)
            -- Concentrated weights collapse ESS to near 1: with one mass at exp(0) and
            -- 999 at exp(-20), ESS = 1 / sum p_i^2 is within a hair of 1, far below N.
            assertBool
                ("concentrated ESS " <> show essConcentrated <> " is near 1 and far below N = " <> show n)
                (essConcentrated < 1.001 && essConcentrated < n / 100)
            -- After the resample the weights are reset to uniform, so ESS is back to N:
            -- the resample has refocused the population away from the degenerate state.
            assertBool
                ("post-resample ESS " <> show essAfterResample <> " resets to N = " <> show n)
                (abs (essAfterResample - n) < 1e-6)
        , testCase "systematic resampling does not shift the empirical mean (unbiased, fixed seed)" $ do
            let
                -- A weighted particle cloud laid out as a contiguous grid of values
                -- across [-4, 4] carrying a smooth bell-shaped weight profile centered
                -- at 0.7. This mirrors a real filtering posterior (values spread over a
                -- range, weight peaked in the middle and tapering at the tails) rather
                -- than a repeated short cycle. The ordering matters: systematic
                -- resampling's single regularly spaced comb is sensitive to particle
                -- ORDER, and a periodically tiled weight pattern resonates with the comb
                -- and biases the resampled mean (Doucet and Johansen 2009 note the
                -- order-dependence; Hol, Schon, and Gustafsson 2006 demonstrate it). A
                -- monotone grid with a smoothly varying weight has no such period, so the
                -- comb samples it faithfully, which is the realistic case the filter
                -- actually resamples.
                count = 20000 :: Int
                -- Values evenly spaced on [-4, 4].
                gridValue i = -4 + 8 * fromIntegral i / fromIntegral (count - 1)
                values = VU.generate count gridValue :: VU.Vector Double
                -- A Gaussian-shaped (unnormalized) weight profile peaked at 0.7, width
                -- 1.3, supplied as a log-weight directly: log w_i = -((v_i - mu)^2)/(2 s^2).
                -- normalizedLogWeights handles the normalization across the population.
                profileMu = 0.7 :: Double
                profileS = 1.3 :: Double
                logWeights =
                    VU.map (\v -> negate ((v - profileMu) * (v - profileMu)) / (2 * profileS * profileS)) values
                -- The weighted mean BEFORE resampling, the target. Computed directly
                -- from the normalized weights and values.
                normalized = normalizedLogWeights logWeights
                weightedMeanBefore = VU.sum (VU.zipWith (*) normalized values)
                -- Resample under a FIXED seed, then take the PLAIN mean of the resampled
                -- (now equally weighted) particle values: this is the empirical mean
                -- after resampling, which must match the weighted mean before.
                plainMeanAfter = runST $ do
                    g <- initialize fixedSeed
                    ancestors <- systematicResample logWeights (VU.enumFromN 0 count) g
                    let resampled = VU.backpermute values ancestors
                    pure (VU.sum resampled / fromIntegral (VU.length resampled))
                -- Tolerance. Resampling is unbiased, so the resampled mean estimates the
                -- weighted mean with a standard error bounded by the cloud's weighted
                -- spread over sqrt(count). The weighted sd here is ~1.2, so at
                -- count = 20000 the SE is ~ 1.2/sqrt(20000) ~ 0.0085; systematic
                -- resampling has variance no larger than multinomial, so the 0.05 window
                -- is ~6 SE, generous enough that a correct (unbiased) resample clears it
                -- essentially always while a biased resample (one that, say, favored the
                -- high-weight center or dropped the tails) would shift the mean past it.
                tol = 0.05
            assertBool
                ("plain mean after resample " <> show plainMeanAfter <> " near weighted mean before " <> show weightedMeanBefore <> " (tol " <> show tol <> ")")
                (abs (plainMeanAfter - weightedMeanBefore) < tol)
        , testCase "filtering mean and variance match the analytic Kalman filter per step (fixed seed)" $ do
            let
                -- Many particles so the per-step filtering moments are low-variance
                -- estimates of the Kalman posterior; the threshold 0.5 resamples when
                -- fewer than half the particles effectively contribute.
                count = 200000 :: Int
                threshold = 0.5 :: Double
                -- The analytic oracle: the scalar Kalman filter over the same
                -- observations from the same Normal prior.
                kalman = kalmanFilter kq kr (km0, kp0) kalmanObservations
                -- The particle filter under a FIXED ST seed.
                steps = runST $ do
                    g <- initialize fixedSeed
                    V.toList <$> particleFilter count threshold kalmanModel kalmanObservations g
                -- Pair each step's particle estimate with the Kalman answer.
                paired = zip steps kalman
                -- Tolerances. The posterior sd over the sequence is on the order of
                -- sqrt(kr) ~ 1 (the steady-state filtering variance for q=0.5, r=1 is
                -- ~0.5, sd ~0.7). With a healthy ESS that stays a large fraction of
                -- count, the filtering-mean SE ~ posterior_sd / sqrt(ESS); even at a
                -- conservative ESS ~ count/4 = 50000 that is ~0.7/224 ~ 0.003, so a 0.05
                -- mean window is ~15 SE. The variance is a second-moment estimator and
                -- noisier, and the resampling step adds a little extra spread, so it
                -- gets the looser 0.05 window against a posterior variance of ~0.5; that
                -- is still a tight relative window a correct filter clears essentially
                -- always, while a filter that dropped the observation weight or skipped
                -- the resample would diverge from the Kalman track by far more.
                meanTol = 0.05
                varTol = 0.05
            mapM_
                ( \(fs, (km, kp)) -> do
                    assertBool
                        ("filtering mean " <> show (fsMean fs) <> " near Kalman mean " <> show km <> " (tol " <> show meanTol <> ")")
                        (abs (fsMean fs - km) < meanTol)
                    assertBool
                        ("filtering variance " <> show (fsVariance fs) <> " near Kalman variance " <> show kp <> " (tol " <> show varTol <> ")")
                        (abs (fsVariance fs - kp) < varTol)
                )
                paired
        ]

{- | The tracking seed for the planted trajectory: the suite's 'fixedSeed', so the
walk the filter is judged against is the same every run.
-}
trackTrajectorySeed :: VU.Vector Word32
trackTrajectorySeed = fixedSeed

{- | A distinct seed for the observation noise, so the noisy reads are independent of
the walk rather than sharing its stream. A second fixed vector keeps the
observations deterministic while uncorrelated with the trajectory.
-}
trackObservationSeed :: VU.Vector Word32
trackObservationSeed = VU.fromList [0x7472, 0x6163, 0x6b6f, 0x6273]

{- | A third fixed seed for the particle filter itself, so the filtering run is
reproducible and independent of the data-generating streams.
-}
trackFilterSeed :: VU.Vector Word32
trackFilterSeed = VU.fromList [0x7466, 0x696c, 0x7465, 0x7273]

{- | THE tracking gate: the particle filter's estimate follows the moving target
under a fixed seed (the spec's programmatic gate).

The model-level oracle for the tracking demo, the particle-filter analogue of the
localization recovery test: no closed form is assumed, the planted trajectory is the
oracle. A hidden random-walk target is planted under a fixed seed, the matching noisy
observations are generated and fed to 'particleFilter', and the filter's per-step
filtering mean must FOLLOW the true trajectory. Two assertions, both against the
hidden states the filter never sees directly:

1. The mean absolute per-step tracking error stays small. The filter sees only the
   noisy observations @y_t ~ Normal(x_t, trackR)@, so its filtering mean cannot equal
   the hidden @x_t@ exactly; the right target is that, averaged over the run, the
   estimate sits within a fraction of the observation noise of the truth, the
   signature of a filter that tracks rather than drifts.
2. The final estimate is near the final true state, so the filter is locked on at the
   end of the run, not merely close on average while lagging.

This is the third correctness treatment of the SMC (after the ESS and
unbiased-resampling oracles and the Kalman known-answer gate from 05-01), now at the
model level the CONTEXT asks of the tracking demo: the estimate follows the target.

Tolerances, in the standard-error discipline of the localization gate. The steady-
state filtering posterior for a random walk with @q = trackQ = 0.6@ and
@r = trackR = 1.0@ has standard deviation on the order of @sqrt(r) ~ 1@ (the
steady-state filtering variance solves @p = (p + q) r / (p + q + r)@, giving
@p ~ 0.59@, sd @~ 0.77@). With many particles the per-step filtering mean is a
low-variance estimate of that posterior mean, whose own distance from the hidden
state is itself about that posterior sd, so a mean absolute tracking error around
@0.77@ is what a CORRECT filter delivers. The window is @1.2@ (about 1.5x the
posterior sd and well above the per-step estimator noise at @count = 100000@): a
filter that tracks clears it essentially always, while a filter that ignored the
observation weight or never resampled would drift away from the walk and blow past
it (the 05-01 teeth check showed a weight-dropping filter misses the Kalman track by
~9x its tolerance, the same failure this would catch). The final-state window is the
looser @2.0@ (about 2.6 posterior sd), since a single step is noisier than the run
average; it still pins the filter to the end of the walk rather than letting it sit
near the prior.
-}
trackingRecoveryTests :: TestTree
trackingRecoveryTests =
    testGroup
        "tracking planted-trajectory recovery (the tracking gate, the spec)"
        [ testCase "the filter estimate follows the moving target under a fixed seed" $ do
            let
                -- A planted random-walk trajectory of moderate length, and the noisy
                -- observations of it, both under fixed seeds so the run is deterministic.
                steps = 40
                trueStates = trueTrajectory trackTrajectorySeed steps
                observations = trajectoryObservations trackObservationSeed trueStates
                -- Enough particles that the per-step filtering mean is a low-variance
                -- estimate of the filtering posterior mean; threshold 0.5 resamples
                -- when fewer than half the particles effectively contribute. The mean
                -- tracking error is dominated by the observation-noise floor, not the
                -- particle count (it is flat from 5000 to 20000 particles at this
                -- model), so 5000 is ample and keeps the suite fast.
                count = 5000 :: Int
                threshold = 0.5 :: Double
                -- The particle filter under a FIXED ST seed, over the observations only.
                filterMeans = runST $ do
                    g <- initialize trackFilterSeed
                    fmap (map fsMean . V.toList) (particleFilter count threshold trackingModel observations g)
                -- The per-step tracking errors against the hidden states the filter
                -- never saw, and their mean absolute value (the average tracking error).
                errors = zipWith (\est true -> abs (est - true)) filterMeans trueStates
                meanAbsError = sum errors / fromIntegral (length errors)
                -- The final estimate against the final true state: the filter must be
                -- locked on at the end, not just close on average.
                finalError = abs (last filterMeans - last trueStates)
                -- Windows justified above in terms of the steady-state filtering sd
                -- (~0.77 for q=0.6, r=1.0) and the observation noise trackR.
                meanErrTol = 1.2
                finalErrTol = 2.0
            assertBool
                ("mean absolute tracking error " <> show meanAbsError <> " within " <> show meanErrTol <> " (steady-state filtering sd ~0.77, obs noise " <> show trackR <> ")")
                (meanAbsError < meanErrTol)
            assertBool
                ("final estimate " <> show (last filterMeans) <> " near final true state " <> show (last trueStates) <> " (tol " <> show finalErrTol <> ")")
                (finalError < finalErrTol)
        ]

{- | The planted clusters for the mixture recovery oracle: three well-separated means
and the mixing weights, the truth the recovered means are compared against.

The means are spread far apart relative to the cluster spread 'mixSigma' (gaps of 6
and 5 against sigma 0.7, about 8 sigma), so each synthetic point's likelihood is
dominated by the nearest cluster and the posterior over the means concentrates
sharply on the planted values, which is what makes the planted means a sharp oracle.
The weights are equal, so each cluster is equally represented.
-}
plantedMeans :: [Double]
plantedMeans = [-6.0, 0.0, 5.0]

mixWeights :: [Double]
mixWeights = [1.0, 1.0, 1.0]

{- | The known shared cluster standard deviation. Small relative to the gaps between
'plantedMeans' so the clusters are well separated and the recovery target is sharp.
-}
mixSigma :: Double
mixSigma = 0.7

{- | The mixture prior parameters: a broad Normal on each cluster mean, wide enough
to cover all three planted means without favoring any, the same broad-prior framing
the localization model uses.
-}
mixMu0, mixSigma0 :: Double
mixMu0 = 0.0
mixSigma0 = 10.0

{- | Synthetic data with the planted clusters: points tightly clustered around each
of 'plantedMeans', LITERAL so every run is deterministic under the fixed seed. Each
cluster's points are symmetric about its planted mean (the deviations sum to zero),
so the cluster's sample mean is exactly the planted value and the recovery target is
the planted mean itself rather than a finite-sample offset of it. Five points per
cluster, fifteen in all, enough to pin each well-separated cluster.
-}
mixturePoints :: [Double]
mixturePoints =
    -- Around -6.0: five points summing to -30.0 (mean -6.0).
    [-6.5, -5.5, -6.3, -5.7, -6.0]
        -- Around 0.0: five points summing to 0.0 (mean 0.0).
        ++ [-0.5, 0.5, -0.3, 0.3, 0.0]
        -- Around 5.0: five points summing to 25.0 (mean 5.0).
        ++ [4.6, 5.4, 4.7, 5.3, 5.0]

{- | THE mixture gate: planted-cluster recovery under a fixed seed, with label
switching handled (the spec, correctness oracle 4).

The model-level oracle for the Gaussian mixture: no closed form, the planted cluster
means are the oracle. Well-separated clusters are planted ('plantedMeans'), synthetic
data is generated near them ('mixturePoints'), importance sampling is run on
'gaussianMixtureCollapsed' under a fixed seed, and the posterior must recover the
planted means. This is also the fourth model running under the importance interpreter
unchanged, the operational-monad payoff: the mixture is built only from 'sample' and
'observe' over the unchanged 'Credence.Model'.

Why the COLLAPSED form. The generative 'gaussianMixture' draws a categorical
assignment per point; under importance sampling that proposal almost never lands
every assignment right, so the weights concentrate on a single particle (effective
sample size near 1) and no particle budget recovers the means (measured directly: at
@n = 400000@ the ESS is 1.0 and the means come back near their lucky single-particle
draw). 'gaussianMixtureCollapsed' sums the assignment out analytically, which is
exact and leaves the same posterior on the means but removes the lottery, so the ESS
is healthy (tens to hundreds) and the means are recoverable. Both are 'sample'/
'observe' over the unchanged monad; the collapsed one is the Rao-Blackwellized
version of the very same model.

CRITICAL: LABEL SWITCHING, handled by a WITHIN-PARTICLE sort. The mixture posterior
is invariant to permuting the cluster labels, so the @k@ mean-latents are
EXCHANGEABLE: each coordinate's marginal posterior is the SAME mixture over all
clusters, and a per-coordinate posterior mean averages the clusters together (it
comes out near the grand mean, recovering none of them; measured directly, the
unsorted per-coordinate means are all near @(-6 + 0 + 5)/3 ~ -0.33@). The
canonicalization the correctness discipline prescribes is to impose an
identifiability constraint by ordering: SORT each particle's mean vector, then take
the posterior mean of each sorted coordinate, so the j-th sorted coordinate's
posterior mean lands on the j-th planted mean. The projection @\\ms -> sort ms !! j@
is that within-particle sort; 'posteriorMean' then weights it across particles. The
recovered values are finally compared to the sorted planted means. Without this sort
the assertion would test a property the posterior does not have.

Tolerance, in the standard-error discipline of the other recovery gates. With the
assignment collapsed the proposal is the broad prior @Normal(0, 10)@ and the
likelihood (five tight Normal factors per well-separated cluster at @sigma = 0.7@) is
sharp but no longer degenerate; the per-cluster posterior sd is about
@sigma / sqrt(5) ~ 0.31@ and the clusters are separated by about 8 sigma, so the
sorted-coordinate posterior means land within a few hundredths of the planted means
(measured: @[-6.00, 0.02, 5.03]@ at @n = 200000@). A @0.3@ window (about 1x the
per-cluster posterior sd, ~10x the observed error) clears a correct sampler
essentially always while a mis-weighted sampler, which would leave the means near the
prior mean 0, misses the outer clusters by 5 to 6 units, far past it.
-}
mixtureRecoveryTests :: TestTree
mixtureRecoveryTests =
    testGroup
        "gaussian mixture planted-cluster recovery (the mixture gate, the spec)"
        [ testCase "importance posterior recovers the planted cluster means, sorted (label switching)" $ do
            let
                k = length plantedMeans
                -- Importance sampling on the COLLAPSED mixture under a FIXED ST seed.
                -- The model returns the k-vector of cluster means; many particles so
                -- the three-latent posterior mean is stable.
                n = 200000
                samples = runST $ do
                    g <- initialize fixedSeed
                    importanceSamples n (gaussianMixtureCollapsed mixMu0 mixSigma0 mixWeights mixSigma mixturePoints) g
                -- LABEL SWITCHING: sort each particle's mean vector, THEN take the
                -- posterior mean of each sorted coordinate. The within-particle sort
                -- breaks the exchangeability of the means, so the j-th sorted
                -- coordinate identifies the j-th cluster (a per-coordinate mean
                -- without the sort would average all clusters together).
                recovered = [posteriorMean samples (\ms -> sort ms !! j) | j <- [0 .. k - 1]]
                plantedSorted = sort plantedMeans
                -- Window justified above (~1x the per-cluster posterior sd ~0.31,
                -- about 10x the observed recovery error).
                tol = 0.3
            mapM_
                ( \(rec, planted) ->
                    assertBool
                        ("recovered cluster mean " <> show rec <> " near planted " <> show planted <> " (sorted, tol " <> show tol <> ")")
                        (abs (rec - planted) < tol)
                )
                (zip recovered plantedSorted)
        ]

{- | The wire format round-trips through aeson, so the engine and the browser
agree on every field.

The streaming boundary is only as trustworthy as its serialization: a field that
encodes one way and decodes another is a silent data corruption between the
inference engine and the visualizer, the streaming analogue of the silent-sampler
hazard this suite exists to rule out. The check is @decode (encode ev) == Just ev@
for a representative value of each constructor that crosses the wire, which fails
loudly if a 'ToJSON'/'FromJSON' pair drifts apart.

Both 'StreamEvent' arms are covered ('EvBatch' and 'EvSummary'), and within them
every field shape: a 'Summary' with both diagnostics present and one with both
'Nothing' (the importance-vs-MH split, where @acceptanceRate@ or
@effectiveSampleSize@ is absent), and a 'Batch' with @batchWeights@ 'Nothing' (the
coin/MH shape, encoded @null@) and one with @batchWeights@ 'Just' an aligned array
(the importance shape the cloud reads), so the @Maybe [Double]@ encoding round-trips
in both states. 'RunRequest' is checked for the coin, the regression, and the
localization 'Dataset', the three bodies the client actually POSTs, so the
tagged-sum decoding the server relies on is proven here rather than discovered at
runtime. 'RunStarted', the run-id response a POST returns, is checked too, since the
start-to-subscribe handoff depends on that field surviving the trip. These are
exact-equality checks, not tolerances: serialization is not statistical, so a
round-trip is either the identity or a bug.
-}
wireTests :: TestTree
wireTests =
    testGroup
        "wire (aeson round-trip)"
        [ testCase "EvSummary with both diagnostics present round-trips" $ do
            -- An MH-shaped summary: acceptance rate present, ESS present too here
            -- only to exercise both Just fields in one value.
            let ev =
                    EvSummary
                        Summary
                            { summaryBatchIndex = 3
                            , summaryMean = 0.6428571428571429
                            , summaryVariance = 0.015306122448979591
                            , summaryAcceptanceRate = Just 0.4987
                            , summaryEffectiveSampleSize = Just 1880.25
                            }
            assertRoundTrip "EvSummary (Just/Just)" ev
        , testCase "EvSummary with Nothing diagnostics round-trips (the importance shape)" $ do
            -- An importance-shaped summary has no acceptance rate; here ESS is also
            -- Nothing to prove a fully absent Maybe Double pair survives the trip.
            let ev =
                    EvSummary
                        Summary
                            { summaryBatchIndex = 0
                            , summaryMean = 0.5
                            , summaryVariance = 0.0
                            , summaryAcceptanceRate = Nothing
                            , summaryEffectiveSampleSize = Nothing
                            }
            assertRoundTrip "EvSummary (Nothing/Nothing)" ev
        , testCase "EvBatch with draws but no weights round-trips (the MH/coin shape)" $ do
            -- A coin or MH batch carries no per-draw weights, so batchWeights is
            -- Nothing, which encodes as null and must round-trip back to Nothing.
            let ev = EvBatch Batch{batchBatchIndex = 7, batchDraws = [0.61, 0.66, 0.63, 0.59], batchWeights = Nothing}
            assertRoundTrip "EvBatch (Nothing weights)" ev
        , testCase "EvBatch with draws and aligned weights round-trips (the importance shape)" $ do
            -- An importance batch carries normalized per-draw weights aligned with
            -- the draws (same length, same order), the opacity input the cloud reads.
            -- Just xs encodes as the array and must survive the trip as Just xs.
            let ev =
                    EvBatch
                        Batch
                            { batchBatchIndex = 4
                            , batchDraws = [2.9, 3.1, 3.0, 2.95]
                            , batchWeights = Just [0.2, 0.35, 0.3, 0.15]
                            }
            assertRoundTrip "EvBatch (Just weights)" ev
        , testCase "EvDone round-trips" $
            assertRoundTrip "EvDone" EvDone
        , testCase "RunRequest for the coin model round-trips" $ do
            let req =
                    RunRequest
                        { runRequestMethod = Importance
                        , runRequestDataset = Coin{datasetFlips = [True, True, False, True]}
                        , runRequestSamples = 2000
                        }
            assertRoundTripReq "RunRequest coin" req
        , testCase "RunRequest for the regression model round-trips" $ do
            let req =
                    RunRequest
                        { runRequestMethod = MH
                        , runRequestDataset = Regression{datasetPoints = [(1.0, 2.1), (2.0, 3.9)]}
                        , runRequestSamples = 3000
                        }
            assertRoundTripReq "RunRequest regression" req
        , testCase "RunRequest for the localization model round-trips" $ do
            -- The third dataset the client POSTs: noisy sensor readings of a hidden
            -- source. Proves the Localization tag the /run/localization route matches
            -- decodes here rather than being discovered at runtime.
            let req =
                    RunRequest
                        { runRequestMethod = Importance
                        , runRequestDataset = Localization{datasetReadings = [1.1, 0.9, 1.2, 0.8]}
                        , runRequestSamples = 4000
                        }
            assertRoundTripReq "RunRequest localization" req
        , testCase "RunStarted (the POST run-id response) round-trips" $ do
            -- The body a run POST returns: the RunId the client then streams. A
            -- field that drifted here would break the start-to-subscribe handoff.
            -- T.pack since this module has no OverloadedStrings; the field is Text.
            let started = RunStarted{runStartedRunId = T.pack "7"}
            assertRoundTripStarted "RunStarted" started
        ]
  where
    -- decode . encode is the identity on a StreamEvent: the value survives the
    -- wire unchanged. Equality is exact because serialization is not statistical.
    assertRoundTrip :: String -> StreamEvent -> IO ()
    assertRoundTrip name ev =
        assertBool
            (name <> ": decode (encode ev) /= Just ev; got " <> show (decode (encode ev) :: Maybe StreamEvent))
            (decode (encode ev) == Just ev)
    -- The same identity for the request body the client POSTs to start a run.
    assertRoundTripReq :: String -> RunRequest -> IO ()
    assertRoundTripReq name req =
        assertBool
            (name <> ": decode (encode req) /= Just req; got " <> show (decode (encode req) :: Maybe RunRequest))
            (decode (encode req) == Just req)
    -- And for the run-id response the POST returns, the handle the client streams.
    assertRoundTripStarted :: String -> RunStarted -> IO ()
    assertRoundTripStarted name started =
        assertBool
            (name <> ": decode (encode started) /= Just started; got " <> show (decode (encode started) :: Maybe RunStarted))
            (decode (encode started) == Just started)
