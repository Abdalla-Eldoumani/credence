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
import Credence.Model (Model (..), observe)
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32)
import Statistics.Distribution (logDensity, logProbability)
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

-- The stage 1 gate is conjugate-analytic agreement (importance sampling on
-- the coin model versus the closed-form Beta posterior); that lands with the
-- sampler in 01-02. This suite proves the foundation under it: the log-space
-- helper, every distribution's log-density against the independent statistics
-- oracle, and every distribution's sampler tied to that same parameterization by
-- a moment-matching test. See the correctness checks for the
-- doctrine. A wrong sampler looks plausible, so these checks, not the eye, are
-- what prove correctness.
tests :: TestTree
tests = testGroup "credence" [logSumExpTests, densityTests, momentTests, modelTests]

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
        , testCase "Categorical at i=1 vs hand-derived log(w_i / sum w)" $ do
            -- No clean statistics constructor for an arbitrary categorical, so the
            -- reference is derived by hand: the normalized log-probability of an
            -- index is log w_i minus log of the total weight.
            let ws = VU.fromList [1, 3, 6 :: Double]
                i = 1
                ref = log (ws VU.! i) - log (VU.sum ws)
            assertCloseDensity "Categorical" (logDensityDist (categoricalDist ws) i) ref
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
