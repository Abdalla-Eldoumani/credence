{- | A moving-target tracking problem as a 'StateSpace' for the particle filter, the
fourth model the engine runs and the first one that is a state-space model rather
than a 'Credence.Model.Model'.

This module adds no inference machinery. Like 'Credence.Model.Localization' it only
describes a model and hands it to an existing interpreter: where localization is a
'Model' the importance and MH backends run, tracking is a 'StateSpace' the
'Credence.Inference.SMC.particleFilter' runs. The filter, the resampling, and the
log-space weighting all live in "Credence.Inference.SMC"; this module supplies the
three pieces a 'StateSpace' needs (an initial prior, a transition kernel, and a
scalar observation log-likelihood) and the planted truth the recovery oracle judges
the filter against.

The target is a one-dimensional random walk observed with Normal noise, the
canonical filtering problem and the same linear-Gaussian shape the Kalman oracle
already validated the filter on, now read as a moving target the cloud should
follow: the hidden state @x_t@ drifts by @x_t ~ Normal(x_{t-1}, q)@ and each step is
seen as @y_t ~ Normal(x_t, r)@. A random walk (rather than a constant-velocity
model) keeps the state a single 'Double', which drops straight into the filter's
scalar 'Credence.Inference.SMC.FilterStep' projection and keeps the demo's cloud
one-dimensional, matching the localization cloud the visualizer reuses.

The correctness oracle is a planted-trajectory recovery test, the particle-filter
analogue of the localization gate: a known trajectory is generated under a fixed
seed, the matching noisy observations are fed to the filter, and the filter's
per-step filtering mean must track the true state within a documented tolerance (the
estimate follows the target). This is the model-level gate the
tracking demo must clear for correctness, on top of the Kalman
known-answer gate the filter itself already passes. The planted trajectory is the
oracle; no closed form stands in for it.

Building a 'StateSpace' performs no sampling. The 'ssInit' and 'ssTransition'
'Dist's draw nothing until the filter threads a generator, and 'ssLogLik' is a pure
log-density. Every piece is 'normalDist', so the model carries the same
standard-deviation parameterization the distribution library was proven on, in
log-space throughout.
-}
module Credence.Model.Tracking (
    trackingModel,
    trueTrajectory,
    trajectoryObservations,
    trackQ,
    trackR,
    trackX0,
) where

import Control.Monad.ST (runST)
import Credence.Distribution (logDensityDist, normalDist, sampleDist)
import Credence.Inference.SMC (StateSpace (..))
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32)
import System.Random.MWC (initialize)

{- | The transition noise standard deviation @q@: how far the target drifts per
step. Moderate so the walk wanders visibly without the cloud needing an
unreasonable particle count to keep up, the same well-conditioned regime the Kalman
oracle uses.
-}
trackQ :: Double
trackQ = 0.6

{- | The observation noise standard deviation @r@: how noisy each read of the target
is. Larger than @q@ so a single observation does not pin the state and the cloud has
real filtering work to do (the posterior is genuinely a blend of the dynamics and
the reads, which is what the cloud is meant to show).
-}
trackR :: Double
trackR = 1.0

{- | The target's starting position @x_0@, the mean of the initial-state prior. The
recovery test plants its trajectory from this point so the filter's initial cloud,
drawn from 'ssInit', already brackets the truth.
-}
trackX0 :: Double
trackX0 = 0.0

{- | The initial-state prior standard deviation: a modest spread around 'trackX0' so
the first particle cloud covers the planted start without being so broad the first
observation has to do all the work.
-}
trackInitSigma :: Double
trackInitSigma = 1.0

{- | The moving-target tracking model as a 'StateSpace Double'.

A one-dimensional random-walk target observed with Normal noise: the initial state
is @Normal(trackX0, trackInitSigma)@, the target drifts by the random walk
@x_t ~ Normal(x_{t-1}, trackQ)@, and each observation is @y_t ~ Normal(x_t,
trackR)@ scored by the Normal log-density. This is exactly the three-piece
'StateSpace' shape "Credence.Inference.SMC" documents (a Gaussian random walk is
@ssTransition = \\x -> normalDist x trackQ@ and a Gaussian observation is
@ssLogLik = \\x y -> logDensityDist (normalDist x trackR) y@), so the filter tracks
it with no new code. The state is a single 'Double', so the filter's scalar
'Credence.Inference.SMC.FilterStep' projection summarizes the target position
directly and the cloud renders one-dimensionally, the way the localization cloud
does.

All three of @trackQ@, @trackR@, and @trackInitSigma@ are positive by construction,
the same precondition 'normalDist' trusts of its standard deviation, so they are not
guarded here.
-}
trackingModel :: StateSpace Double
trackingModel =
    StateSpace
        { ssInit = normalDist trackX0 trackInitSigma
        , ssTransition = (`normalDist` trackQ)
        , ssLogLik = \x y -> logDensityDist (normalDist x trackR) y
        }

{- | The planted hidden trajectory of @n@ states under a fixed seed, the truth the
filter is judged against and the curve the visualizer marks crisp.

Starting at 'trackX0', each successive state is drawn from the same random-walk
transition the model uses, @x_t ~ Normal(x_{t-1}, trackQ)@, threading a generator
seeded by the supplied seed vector. Because the seed is fixed the trajectory is
deterministic, which is what makes it a stable oracle: the recovery test compares the
filter's per-step mean to exactly this sequence. The walk is generated rather than
written literally so the dynamics that produce it are the model's own, not a
hand-drawn shape the filter might match for the wrong reason. The returned list is
@x_1 .. x_n@; @x_0@ is the prior mean the filter's initial cloud centers on, not an
observed step.
-}
trueTrajectory :: VU.Vector Word32 -> Int -> [Double]
trueTrajectory seed n = runST $ do
    g <- initialize seed
    let walk 0 _ = pure []
        walk k prev = do
            next <- sampleDist (normalDist prev trackQ) g
            rest <- walk (k - 1) next
            pure (next : rest)
    walk n trackX0

{- | The noisy observation sequence for a planted trajectory under a fixed seed, the
input the filter actually runs on.

Each true state @x_t@ is corrupted by independent Normal noise of standard deviation
'trackR', @y_t ~ Normal(x_t, trackR)@, the model's own observation kernel, with a
generator seeded by the supplied vector so the observations are deterministic. The
recovery test plants a trajectory with 'trueTrajectory', generates the matching
observations here, and feeds only the observations to the filter, then checks the
filter's estimate against the hidden trajectory it never saw directly. Seeding this
from a different vector than the trajectory keeps the observation noise independent
of the walk.
-}
trajectoryObservations :: VU.Vector Word32 -> [Double] -> [Double]
trajectoryObservations seed states = runST $ do
    g <- initialize seed
    mapM (\x -> sampleDist (normalDist x trackR) g) states
