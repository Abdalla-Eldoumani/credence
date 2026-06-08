{- | A Gaussian mixture model for clustering as a 'Model' fixture, the fifth model
the inference engine runs unchanged and the first with more than one latent and a
data-dependent random choice.

This module adds no machinery. Like 'Credence.Model.coinModel',
'Credence.Model.Regression.linearRegression', and
'Credence.Model.Localization.localize' it is a single 'Model' value built only from
'sample' and 'observe' over the existing monad, and 'Credence.Model' is untouched.
The operational representation pays off a fourth time, and the most pointed time: a
clustering model whose every observation draws a latent assignment and is scored
under the assigned cluster is expressed in the same two primitives, and the
importance interpreter runs it with no change. Importance sampling is the
interpreter that needs no addressing (see "Credence.Inference.Importance"), so it
handles the per-point categorical assignment that makes the mixture's control flow
data-dependent; that is exactly why the recovery oracle below runs under importance
rather than the trace-based MH, which is scoped to fixed-structure models.

The model is the standard finite Gaussian mixture. There are @k@ clusters with
known mixing weights and a known shared cluster standard deviation; the unknowns are
the @k@ cluster means. This module exposes the mixture in two equivalent forms, both
built only from 'sample'/'observe' over the unchanged monad:

* 'gaussianMixture' is the generative form written beat for beat: draw each cluster
  mean from a broad Normal prior, then for each data point draw a cluster assignment
  from a 'Credence.Distribution.categoricalDist' over the mixing weights and 'observe'
  that point's Normal log-likelihood under the assigned cluster's mean. The per-point
  assignment is a data-dependent random choice, the one
  the importance interpreter handles without addressing.

* 'gaussianMixtureCollapsed' is the SAME model with the discrete assignment summed
  out analytically (Rao-Blackwellized): each data point 'observe's the marginal
  mixture log-likelihood @log sum_z weight_z * Normal(point; mean_z, sigma)@,
  combined in log-space with 'Credence.logSumExp'. Marginalizing a discrete nuisance
  latent is a standard, exact, correctness-preserving move, so the two builders
  define the SAME posterior over the cluster means. The collapsed form exists because
  importance sampling cannot recover the means from the assignment-sampling form: with
  one categorical draw per point the forward proposal almost never lands every
  assignment right, the importance weights concentrate on a single particle (effective
  sample size collapses to ~1), and no particle budget fixes it. Summing the
  assignment out removes that lottery, so the effective sample size is healthy and the
  posterior over the means is recoverable. The recovery oracle and the streaming
  server both run the collapsed form for this reason; see the note on the recovery test
  below.

The correctness oracle is a planted-cluster recovery test (the fourth correctness
oracle), not a closed form: well-separated clusters are planted, synthetic data is
generated near them, importance sampling is run on 'gaussianMixtureCollapsed' under a
fixed seed, and the posterior must recover the planted means within a documented
tolerance. The mandatory subtlety is LABEL SWITCHING. The mixture posterior is
invariant to permuting the cluster labels (relabeling clusters leaves the likelihood
unchanged), so the @k@ mean-latents are EXCHANGEABLE: each coordinate's marginal
posterior is the same mixture over all clusters, and a per-coordinate posterior mean
would average the clusters together and recover none of them. The canonicalization
used here is to impose an identifiability constraint by ordering: SORT the
mean vector WITHIN each particle, then take the posterior mean of each sorted
coordinate, which lands the j-th sorted coordinate on the j-th planted mean. The test
then compares those sorted recovered means to the sorted planted means; see
"test/Spec.hs". The planted means are the oracle.

Building either model performs no sampling and no IO. It allocates the same inert
instruction tree any model is: each 'sample' draws nothing until an interpreter
supplies a generator, and each 'observe' only records a log-likelihood increment.
Every Normal score is 'logDensityDist' of a 'normalDist', so the model carries the
same standard-deviation parameterization the distribution library was proven on; the
assignment draw is a 'categoricalDist' whose normalization the distribution library
does in log-space, and the collapsed likelihood combines the per-cluster terms with
'logSumExp'. Nothing here exponentiates a weight.
-}
module Credence.Model.Mixture (
    gaussianMixture,
    gaussianMixtureCollapsed,
) where

import Credence (logSumExp)
import Credence.Distribution (categoricalDist, logDensityDist, normalDist)
import Credence.Model (Model, observe, sample)
import Data.Vector.Unboxed qualified as VU

{- | A finite Gaussian mixture over @k@ clusters with known mixing weights and a
known shared cluster standard deviation, returning the posterior latent: the vector
of @k@ cluster means.

Given a prior mean @mu0@ and prior standard deviation @sigma0@ on each cluster mean,
the unnormalized mixing @weights@ (one per cluster, the categorical the assignment
is drawn from), the known cluster standard deviation @sigma@, and the observed data
@points@, the model:

1. draws @k@ cluster means, one per mixing weight, each from the broad prior
   @Normal(mu0, sigma0)@ (the @k@ latents, addressed by execution position the way
   every fixed prefix is); then

2. for each data point, draws a cluster assignment @z@ from
   @categoricalDist weights@ (the data-dependent choice importance sampling handles
   without addressing) and 'observe's that point under the assigned cluster's mean,
   @logDensityDist (normalDist (means !! z) sigma) point@.

It returns the list of @k@ cluster means. This mirrors the other model fixtures'
shape (draw the latent(s), score every datum, return the latent), generalized to
several latents and a per-datum latent assignment.

The likelihood each point contributes is the Normal density under its drawn cluster.
Marginalizing the assignment, that is the mixture likelihood
@sum_z weight_z * Normal(point; mean_z, sigma)@ up to the categorical normalizer, so
importance sampling's draw-from-prior-and-reweight targets the true mixture
posterior; the planted-cluster recovery test is what arbitrates that, this comment
states the framing it mirrors. With well-separated planted clusters each point's
likelihood is dominated by the nearest cluster, so the posterior over the means
concentrates on the planted means, which is the recovery the test asserts and the
visualizer renders.

Preconditions, unguarded the same way the other models trust theirs: @sigma0 > 0@
and @sigma > 0@ (as 'normalDist' trusts its standard deviation), and @weights@
non-negative with at least one positive entry and the same length as the intended
@k@ (as 'categoricalDist' trusts its weights). The data are literal in the tests, so
every run is deterministic under a fixed seed.
-}
gaussianMixture ::
    -- | prior mean @mu0@ on each cluster mean
    Double ->
    -- | prior standard deviation @sigma0@ on each cluster mean
    Double ->
    -- | unnormalized mixing weights, one per cluster (fixes @k@)
    [Double] ->
    -- | known shared cluster standard deviation @sigma@
    Double ->
    -- | observed data points
    [Double] ->
    -- | the posterior latent: the @k@ cluster means
    Model [Double]
gaussianMixture mu0 sigma0 weights sigma points = do
    -- Draw one cluster mean per mixing weight from the broad prior. These are the k
    -- latents, in a fixed prefix so each is addressed by its execution position.
    means <- mapM (const (sample (normalDist mu0 sigma0))) weights
    let meanVec = VU.fromList means
        catWeights = VU.fromList weights
    -- For each point: draw its cluster assignment from the mixing categorical, then
    -- score the point under the assigned cluster's mean. The assignment is the
    -- data-dependent Sample importance sampling handles without addressing.
    mapM_
        ( \point -> do
            z <- sample (categoricalDist catWeights)
            observe (logDensityDist (normalDist (meanVec VU.! z) sigma) point)
        )
        points
    pure means

{- | The collapsed (Rao-Blackwellized) Gaussian mixture: the same model as
'gaussianMixture' with the per-point cluster assignment summed out analytically,
returning the @k@ cluster means.

The arguments and the posterior over the means are identical to 'gaussianMixture'.
The only difference is the likelihood: instead of drawing an assignment @z@ per point
and scoring under cluster @z@, each point 'observe's the MARGINAL mixture
log-likelihood, the discrete latent summed out,

@log sum_z weight_z * Normal(point; mean_z, sigma)  -  log (sum_z weight_z)@,

computed in log-space with 'Credence.logSumExp' over the per-cluster terms
@log weight_z + logDensityDist (normalDist mean_z sigma) point@ and normalized by the
log total weight (so the mixing weights need not be normalized, the same convention
'categoricalDist' uses). Summing over @z@ is exact, so this is the same joint as
'gaussianMixture' with the assignment marginalized, hence the same posterior on the
means.

Why it exists: importance sampling degenerates on the assignment-sampling form
(effective sample size near 1, no budget recovers the means), because the forward
proposal almost never draws every point's assignment correctly. Marginalizing the
assignment removes that lottery, so the effective sample size stays healthy and the
posterior over the means is recoverable. The recovery oracle and the streaming server
run this form for that reason. It is still 'sample' (the @k@ means) and 'observe'
(the marginal per point) over the unchanged 'Credence.Model', so the operational
representation is untouched.

Preconditions are 'gaussianMixture's: @sigma0 > 0@, @sigma > 0@, and @weights@
non-negative with a positive entry and length @k@.
-}
gaussianMixtureCollapsed ::
    -- | prior mean @mu0@ on each cluster mean
    Double ->
    -- | prior standard deviation @sigma0@ on each cluster mean
    Double ->
    -- | unnormalized mixing weights, one per cluster (fixes @k@)
    [Double] ->
    -- | known shared cluster standard deviation @sigma@
    Double ->
    -- | observed data points
    [Double] ->
    -- | the posterior latent: the @k@ cluster means
    Model [Double]
gaussianMixtureCollapsed mu0 sigma0 weights sigma points = do
    -- The same k cluster-mean latents from the same broad prior.
    means <- mapM (const (sample (normalDist mu0 sigma0))) weights
    let logWeights = map log weights
        logTotal = log (sum weights)
        -- The marginal mixture log-likelihood of one point: logSumExp over clusters
        -- of (log mixing weight + Normal log-density under that cluster's mean),
        -- minus the log total weight to normalize the mixing distribution. All in
        -- log-space, so no per-cluster term is exponentiated before being combined.
        marginalLogLik point =
            logSumExp
                ( VU.fromList
                    [ logW + logDensityDist (normalDist m sigma) point
                    | (logW, m) <- zip logWeights means
                    ]
                )
                - logTotal
    mapM_ (observe . marginalLogLik) points
    pure means
