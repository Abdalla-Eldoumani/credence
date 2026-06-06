{- | Bayesian linear regression as a 'Model' fixture, the second model the
inference engine runs unchanged.

This module adds no machinery. It is a single 'Model' value built only from
'sample' and 'observe' over the existing monad, exactly as 'coinModel' is, and
'Credence.Model' is untouched. The payoff of the operational representation is
precisely this: a second, structurally different model (a continuous latent with
many observations, rather than one Beta draw) is expressed in the same monad and
interpreted by the same two backends, importance sampling and trace-based
Metropolis-Hastings, with no change to either interpreter or to the monad. The
trace addresses its one 'Sample' by execution position, the importance sampler
draws it from the prior and reweights, and neither knows or cares that the model
is regression rather than a coin.

Building the model performs no sampling and no IO. It allocates the same inert
instruction tree any model is: the 'sample' draws nothing until an interpreter
supplies a generator, and each 'observe' only records a log-likelihood increment.
An interpreter is what draws and what accumulates the weight.

The fixture is conjugate Normal-Normal with known noise, which is what makes it a
correctness oracle. With a Normal prior on the coefficient and a Normal
likelihood whose standard deviation is fixed, the posterior on the coefficient is
itself Normal with a closed form, so the agreement test in "test/Spec.hs" can
check the sampled posterior against an exact answer in mean and variance rather
than by eye. See the correctness discipline for why that closed-form check
is the only honest proof a sampler works.
-}
module Credence.Model.Regression (
    linearRegression,
) where

import Credence.Distribution (logDensityDist, normalDist)
import Credence.Model (Model, observe, sample)

{- | Single-coefficient Bayesian linear regression through the origin.

The model is the line @y = w * x@ with a Normal prior on the slope @w@ and Normal
observation noise of known standard deviation. Given prior mean @mu0@, prior
standard deviation @sigma0@, known likelihood standard deviation @sigmaL@, and a
list of data points @[(x_i, y_i)]@, it draws the slope once from its prior, then
for each point adds the log-likelihood of @y_i@ under @Normal(w * x_i, sigmaL)@
via 'observe', and returns @w@. This mirrors 'coinModel' beat for beat: draw the
latent, score every datum, return the latent.

The shape matters for the analytic check. Because the prior is @Normal(mu0,
sigma0)@ and each observation contributes a Normal likelihood in @w@ with the
point's @x_i@ weighting it, the posterior on @w@ is conjugate Normal. In
precision form (precision is one over variance), the posterior precision is
@1 / sigma0^2 + (sum_i x_i^2) / sigmaL^2@ and the posterior mean is
@posteriorVar * (mu0 / sigma0^2 + (sum_i x_i * y_i) / sigmaL^2)@. The agreement
test derives those from the scalar Normal-Normal form and asserts the sampled
posterior matches them; this comment states the framing the test must mirror, it
does not stand in for that test.

Both @sigma0 > 0@ and @sigmaL > 0@ are the caller's precondition, the same way
'normalDist' trusts its standard deviation, so they are not guarded here. The
data points are literal in the tests, which keeps every run deterministic under a
fixed seed.

The model carries the same parameterization the distribution library was proven
on: every score is 'logDensityDist' of a 'normalDist', whose standard-deviation
parameterization the moment tests tie to its sampler. Nothing here exponentiates
a weight; 'observe' accumulates Normal log-densities in log-space and the
interpreter sums them.
-}
linearRegression ::
    -- | prior mean @mu0@ on the slope
    Double ->
    -- | prior standard deviation @sigma0@ on the slope
    Double ->
    -- | known likelihood standard deviation @sigmaL@
    Double ->
    -- | observed points @[(x_i, y_i)]@
    [(Double, Double)] ->
    -- | the posterior latent: the slope @w@
    Model Double
linearRegression mu0 sigma0 sigmaL pts = do
    w <- sample (normalDist mu0 sigma0)
    mapM_ (\(x, y) -> observe (logDensityDist (normalDist (w * x) sigmaL) y)) pts
    pure w
