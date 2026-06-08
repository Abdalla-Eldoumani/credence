{- | One-dimensional source localization as a 'Model' fixture, the third model the
inference engine runs unchanged.

This module adds no machinery. Like 'coinModel' and 'linearRegression' it is a
single 'Model' value built only from 'sample' and 'observe' over the existing
monad, and 'Credence.Model' is untouched. The operational representation pays off
a third time: a hidden scalar location with many noisy sensor reads is expressed
in the same monad and interpreted by the same two backends, importance sampling
and trace-based Metropolis-Hastings, with no change to either interpreter. The one
'Sample' (the source position) is addressed by execution position, the same
fixed-structure addressing the coin and regression models use, so MH can run it.

The observation model is the simplest one that concentrates: each sensor returns a
noisy reading of the source position itself, @reading_i ~ Normal(theta, sigma)@
with a known noise standard deviation, the direct-position-read form rather than a
noisy distance (a distance read would fold the sign of @theta - sensorPos@ away and
make the posterior bimodal, which is not what the cloud is meant to show). The
prior is a broad @Normal(mu0, sigma0)@ on the location.

The correctness oracle is a planted-source recovery test, not a closed form. The
posterior here is in fact conjugate Normal-Normal, but the model is still proven by
planting a known @theta@ and asserting the importance
posterior recovers it: the posterior mean lands within a tolerance derived from the
estimator's standard error of the planted value, AND the posterior spread shrinks
as readings accumulate (the concentration the cloud renders). The planted answer is
the oracle; no fabricated formula stands in for it. A recovery check, not the eye,
is the proof.

Building the model performs no sampling and no IO. It allocates the same inert
instruction tree any model is: the 'sample' draws nothing until an interpreter
supplies a generator, and each 'observe' only records a Normal log-likelihood
increment. An interpreter is what draws and what accumulates the weight. Every
score is 'logDensityDist' of a 'normalDist', so the model carries the same
standard-deviation parameterization the distribution library was proven on.
-}
module Credence.Model.Localization (
    localize,
) where

import Credence.Distribution (logDensityDist, normalDist)
import Credence.Model (Model, observe, sample)

{- | One-dimensional localization of a hidden source from noisy position reads.

A hidden source sits at position @theta@ with a Normal prior @Normal(mu0, sigma0)@.
Each of @n@ sensors returns a reading that is the source position corrupted by
independent Normal noise of known standard deviation @sigma@, @reading_i ~
Normal(theta, sigma)@. Given prior mean @mu0@, prior standard deviation @sigma0@,
sensor noise standard deviation @sigma@, and the list of @readings@, the model
draws @theta@ once from its prior, then for each reading adds the log-likelihood of
that reading under @Normal(theta, sigma)@ via 'observe', and returns @theta@. This
mirrors 'coinModel' and 'linearRegression' beat for beat: draw the latent once,
score every datum, return the latent.

The shape is what makes the posterior concentrate. Each reading is a Normal
observation centered on @theta@, so the likelihood is @prod_i Normal(reading_i;
theta, sigma)@, which sharpens around the readings' mean as @n@ grows; with the
Normal prior the posterior on @theta@ is conjugate Normal whose variance falls like
@1 / (1/sigma0^2 + n/sigma^2)@. The recovery test plants a @theta@, generates
readings near it, and asserts the importance posterior mean recovers it and the
posterior variance shrinks from a small reading count to a large one; this comment
states the framing the test mirrors, it does not stand in for that test.

Both @sigma0 > 0@ and @sigma > 0@ are the caller's precondition, the same way
'normalDist' trusts its standard deviation, so they are not guarded here. The
readings are literal in the tests, which keeps every run deterministic under a
fixed seed.
-}
localize ::
    -- | prior mean @mu0@ on the source position
    Double ->
    -- | prior standard deviation @sigma0@ on the source position
    Double ->
    -- | known sensor-noise standard deviation @sigma@
    Double ->
    -- | observed sensor readings @[reading_i]@
    [Double] ->
    -- | the posterior latent: the source position @theta@
    Model Double
localize mu0 sigma0 sigma readings = do
    theta <- sample (normalDist mu0 sigma0)
    mapM_ (observe . logDensityDist (normalDist theta sigma)) readings
    pure theta
