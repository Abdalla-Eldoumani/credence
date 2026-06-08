# Inference and correctness

This page explains how each inference method works and how each is proven
correct. The correctness part is not an afterthought. It is the distinctive value
of the project, so it gets the longer treatment.

The premise behind all of it: **a wrong sampler produces a plausible-looking
posterior.** A bug in an acceptance ratio, a mis-weighted particle, or a sampler
and density that disagree on a parameterization will all return a smooth,
believable, wrong answer. You cannot catch that by looking at the histogram. So
correctness here is proven against exact answers, under fixed seeds, with
documented tolerances, never by eye.

## The three methods

### Importance sampling

Importance sampling is the simplest interpreter of the model and the first to
ship. It runs the program forward: each `Sample` draws from its prior, nothing
cleverer is proposed, and each `Observe` reweights the execution by its
likelihood. A single forward run yields one *particle*: a sampled value paired
with the total log-likelihood of the observations along that run. Many runs give
a weighted sample of the posterior, where the weights correct the mismatch
between the prior we drew from and the posterior we want.

The correctness of the mechanics is entirely about staying in log-space. Each
observation contributes a log-likelihood, and a run's weight is their sum, which
can be a large negative number. Exponentiating it on its own underflows to zero
and the posterior collapses or goes to `NaN`. So weights are combined through a
stable `logSumExp`, and normalization subtracts the log-sum-exp inside the
exponent, which keeps the largest weight at `exp 0 = 1` and every other a
well-scaled fraction. A posterior expectation is then a weighted average against
those normalized weights. The posterior mean and variance are computed from a
single normalization (the variance by `E[x^2] - E[x]^2`, clamped at zero so
second-moment round-off cannot produce a small negative).

Importance sampling needs no addressing, so it runs *any* model, including those
whose structure depends on random outcomes (the assignment-sampling form of the
Gaussian mixture, for instance).

### Trace-based single-site Metropolis-Hastings

Metropolis-Hastings is the second interpreter and the first whose correctness is
not evident from its structure. Importance sampling draws from the prior and
reweights; nothing about it can be subtly wrong once the weights stay in
log-space. MH is different. It proposes a change to one random choice, recomputes
the program, and accepts or rejects by a ratio that is easy to get quietly wrong,
and a wrong ratio yields a smooth, plausible, incorrect posterior. So the module
implements the acceptance ratio in its *general form* from the primary source and
leaves the proof to the oracles.

This is the database-driven single-site MCMC of Wingate, Stuhlmuller, and Goodman
(2011). The current state is a trace: the database of random choices addressed by
execution position. One step:

1. Pick one address uniformly among the trace's choices.
2. Resample that single choice from its prior (the proposal kernel for the
   perturbed site).
3. Recompute the program with that one choice swapped and every other held fixed
   (replay), producing the proposed trace and its return value.
4. Accept or reject by the general-form Metropolis-Hastings ratio, in log-space:
   draw `u` uniform on `(0,1)` and accept iff `log u < log A`.

Wingate et al. give the acceptance probability for a move from trace `x` to
proposed trace `x'` as

```
A(x -> x') = min(1, [ p(x') / p(x) ] * [ q(x | x') / q(x' | x) ])
```

where `p` is the trace's unnormalized probability and `q(. | .)` is the proposal
density. The implementation computes `log A` from that expression directly. The
trace probability factors, in log-space, into the sum of every choice's prior
log-density plus the run's observe log-weight:

```
log p(x) = (sum over addresses a of logPrior_a(x)) + logLik(x)
```

and the proposal, single-site resampling from the prior with a uniform choice of
which site to perturb, contributes

```
q(x' | x) = (1 / |x|)  * f_{a*}(value'_{a*})
q(x  | x') = (1 / |x'|) * f_{a*}(value_{a*})
```

where `a*` is the perturbed address, `|x|` is the number of choices in `x`, and
`f_{a*}` is the prior of the choice at `a*`. The code sums the full prior and full
likelihood of each trace and forms the full proposal correction rather than
asserting any hand-reduced formula. For a fixed-structure model most of the
proposal correction cancels against the prior difference and the move reduces to a
likelihood ratio, but that reduction is *derived* numerically, not assumed, so the
correctness check can arbitrate it. Everything is in log-space; no raw log-weight
is ever exponentiated.

A run returns the chain of visited values (one per step, including repeats on
rejected steps, so the empirical distribution over the chain is the estimate) and
the measured acceptance rate. The tests assert that rate lies strictly inside
`(0,1)`: a chain that never moves or always moves is a bug even if a moment
happens to match.

### The sequential Monte Carlo particle filter

A particle filter is for a different shape of problem: a latent state that evolves
in time, observed noisily at each step, where the target is the filtering
distribution `p(x_t | y_1..y_t)` updated as each observation arrives. It carries a
population of weighted particles forward, and the one operation that makes it more
than parallel importance sampling is *resampling*: when the weight concentrates on
a few particles the rest are dead weight, so the population is periodically
resampled in proportion to weight and the weights reset to uniform, refocusing
computation on the live region of state space.

A state-space model is expressed entirely through the existing distribution
library, so the numerics stay in one place. A `StateSpace` is three pieces: a
prior `Dist` for the initial state, a transition kernel `s -> Dist s` for the
propagation, and a scalar observation log-likelihood `s -> Double -> Double`. No
new sampling or density machinery is introduced; the filter only sequences
distribution operations and reweights.

The loop is the standard sequential-importance-resampling filter. Draw the initial
particles from the prior; their log-weights start at zero. For each observation:
propagate every particle through the transition kernel, add each particle's
observation log-likelihood to its log-weight (in log-space, additively), then
record the effective sample size and the weighted filtering mean and variance
*before* any resample, so they are the genuine filtering estimate. If the
effective sample size falls below a threshold (a fraction of the particle count,
commonly one half), systematically resample and reset every weight to uniform;
otherwise carry the weighted population forward.

Two diagnostics matter:

- **Effective sample size** is `1 / sum_i w_i^2` over the normalized weights. It
  ranges from 1 when all the mass sits on one particle to `N` when the `N` weights
  are equal, so it measures how many particles are effectively contributing.
- **Systematic resampling** draws `N` ancestor indices in proportion to the
  normalized weights with a single uniform draw on `[0, 1/N)`, forming `N` equally
  spaced comb positions and inverting the cumulative weight at each. A particle of
  weight `w` spawns about `N*w` offspring, exactly `N*w` in expectation, which is
  what makes the scheme unbiased; the single shared offset is what makes it
  lower-variance than drawing `N` independent uniforms. This follows Doucet and
  Johansen (2009) and Arulampalam et al. (2002).

The weights stay in log-space throughout and become a probability vector only
through the same normalization the importance interpreter uses.

## The correctness discipline

Every method above can be subtly wrong in a way that looks right. The defense is a
test suite of *oracles*: checks that compare the approximation to an answer known
by other means, that would fail if the inference were wrong, and that run under
fixed seeds so a pass is reproducible and not luck. The suite is written with
`tasty` and `tasty-hunit`, every statistical test seeds its generator explicitly
with a fixed seed in `ST`, and every tolerance carries a comment explaining its
value in terms of sample count and estimator variance. The suite runs on Linux in
CI with the seeds fixed, so a failure in CI is a real failure, not noise.

The independent reference for log-densities is the `statistics` package, a
test-only dependency. The runtime densities deliberately do not route through it,
because an implementation checked against itself checks nothing.

### Conjugate agreement: Beta-Bernoulli

The single most important check. The coin model is Beta-Bernoulli conjugate: a
prior `Beta(a, b)` with `h` heads and `t` tails has posterior exactly
`Beta(a + h, b + t)`. The test runs importance sampling on the coin model under a
fixed seed and asserts the weighted posterior matches that analytic Beta in
**both mean and variance.** Mean alone is not enough: a sampler can hit the mean
while getting the spread wrong, so the variance is checked too. The analytic
moments are computed from the closed-form Beta formulas and independently
cross-checked against the `statistics` package's Beta distribution, so a typo in
either path is caught by the other. The trace-based methods are not trusted until
this holds.

### Conjugate agreement: Normal-Normal

A second conjugate oracle, independent of the first and on a structurally
different model: a continuous latent with many observations rather than one Beta
draw. The single-coefficient linear regression fixture (`y = w * x` with a Normal
prior on the slope and known observation noise) is conjugate Normal-Normal, so the
posterior on the slope is an exact Normal whose mean and variance come from the
precision-form formulas. Importance sampling must reproduce both. Again the
analytic values are cross-checked against the `statistics` package before the
sampler is judged. Metropolis-Hastings agreement on the same regression fixture is
also exercised, across several seeds, to show the estimate is stable rather than
right by luck on one draw.

### Detailed balance on a known stationary target

The oracle for Metropolis-Hastings. A wrong acceptance ratio yields a smooth,
plausible, wrong stationary distribution, so the proof is a small discrete target
whose stationary distribution is known in closed form, against which the chain's
empirical distribution is compared.

The crucial detail is that the target uses a **non-uniform** prior on purpose. The
proposal redraws from the prior, so for a uniform prior the proposal is symmetric
and the Hastings correction term vanishes identically: a chain that wrongly
dropped that correction would still pass against a uniform target, giving the test
no teeth on the proposal term. With a non-uniform categorical prior reshaped by a
categorical likelihood, the correction does not cancel, the posterior over the
index is `prior * likelihood` normalized, and dropping the correction skews the
result toward the high-prior index and fails the test loudly. The acceptance rate
is also asserted strictly inside `(0,1)`.

### Moment matching: tying a sampler to its density

This is the defense against the **parameterization trap**, the failure mode
described in [architecture.md](architecture.md): a sampler and a density that are
each correct for a different parameterization of the same family pass every
isolated test yet are silently wrong together. For each distribution, the test
draws many samples under a fixed seed and asserts the empirical mean and variance
match the closed-form moments *of the same parameterization the log-density uses.*

The Gamma case has the trap's teeth. credence uses shape and **scale** (not rate).
`mwc-random`'s sampler takes shape then scale, and the `statistics` oracle reads
its second argument as scale, so all three agree; the moment test ties the
sampler's empirical mean `k*theta` and variance `k*theta^2` to that
parameterization. A shape/rate sampler against a shape/scale density would land
far outside the window while every separate test stayed green.

Alongside the moment tests, the support guards are pinned directly: out-of-domain
inputs must score `-Infinity` and crucially **not** `NaN`. The boundary
`Beta(1,1)` at `x = 0` is exactly the `0 * log 0 = NaN` trap; the test asserts the
result is infinite, negative, and not `NaN`.

### The Kalman known-answer comparison for the particle filter

A particle filter that quietly biases the distribution is the same failure mode as
a biased sampler, so it gets the same known-answer treatment, in three parts under
fixed seeds:

1. **Effective sample size behaves correctly:** it is exactly `N` for equal
   weights, drops to near 1 when the weight concentrates on one particle, and
   resets to `N` after a resample reweights the offspring uniformly.
2. **Resampling is unbiased:** systematic resampling does not shift the empirical
   mean of a weighted cloud. (The test uses a monotone grid of values with a
   smoothly varying weight, because systematic resampling's single regularly
   spaced comb is sensitive to particle order and a periodically tiled weight
   pattern would resonate with the comb. That order-dependence is noted by Doucet
   and Johansen 2009 and demonstrated by Hol, Schon, and Gustafsson 2006.)
3. **Per-step agreement with the Kalman filter:** on a linear-Gaussian random walk
   (`x_t = x_{t-1} + N(0, q)` observed as `y_t = x_t + N(0, r)`), the filtering
   distribution stays Normal and its mean and variance follow the scalar Kalman
   recursion exactly. The particle filter's per-step filtering mean and variance
   must match that analytic Kalman track. The Kalman recursion is hand-derived
   from the model, not taken from a library, so it is a genuine independent
   answer.

### Recovery of planted parameters

For models with no convenient closed form, the oracle is a *planted* parameter:
generate data from a known value, then assert the posterior recovers it. These
checks assert both that the posterior mean lands on the planted value within a
tolerance derived from the estimator's standard error, and that the posterior
*concentrates* (the spread shrinks as data accumulates), because a mean that lands
while the spread does not shrink would mean the model conditions but does not
sharpen.

- **Localization** plants a source position, conditions on noisy reads centered on
  it, and asserts the importance posterior recovers the position and that the
  variance is strictly smaller at a large reading count than at a small one.
- **Tracking** plants a hidden random-walk trajectory under a fixed seed,
  generates matching noisy observations, feeds *only* the observations to the
  particle filter, and asserts the filtering mean follows the true trajectory it
  never saw directly (small mean absolute error over the run, and a final estimate
  near the final true state).
- **Mixture** plants well-separated cluster means, generates data near them, and
  asserts the posterior recovers the means. This case carries two subtleties worth
  naming.

#### Two subtleties in the mixture recovery

**Why the collapsed form.** The generative Gaussian mixture draws a categorical
assignment per data point. Under importance sampling that proposal almost never
lands every assignment right, so the weights concentrate on a single particle
(effective sample size near 1) and no particle budget recovers the means. The
collapsed form sums the assignment out analytically (Rao-Blackwellization), which
is exact and leaves the same posterior over the means but removes the lottery, so
the effective sample size is healthy and the means are recoverable. Both forms are
built only from `sample` and `observe` over the unchanged monad; the collapsed one
is the Rao-Blackwellized version of the very same model, and it is what the
recovery oracle and the streaming server run.

**Label switching.** The mixture posterior is invariant to permuting the cluster
labels, so the `k` mean-latents are exchangeable: each coordinate's marginal
posterior is the same mixture over all clusters, and a per-coordinate posterior
mean would average the clusters together and recover none of them. The fix is to
impose an identifiability constraint by ordering: sort each particle's mean vector
*before* taking the posterior mean of each sorted coordinate, so the `j`-th sorted
coordinate's posterior mean lands on the `j`-th planted mean. The streaming server
does the same within-particle sort before emitting cluster locations, which is why
the demo lands cleanly on the planted clusters.

## Why this matters

Taken together, these oracles are the reason the visualizer can claim its
approximation is right. Two independent conjugate gates (Beta-Bernoulli and
Normal-Normal) pin importance sampling and Metropolis-Hastings to exact
posteriors in mean and variance; detailed balance pins the MH acceptance ratio to
a known stationary target with the Hastings term made load-bearing; moment
matching ties every sampler to its own log-density and closes the parameterization
trap; the Kalman comparison pins the particle filter to an exact filtering
recursion step by step; and planted-parameter recovery covers the models without a
closed form. The convergence animation is the part you see, but these checks are
what make it trustworthy.
