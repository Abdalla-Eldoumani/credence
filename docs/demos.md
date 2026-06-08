# Demos

The gallery has six demos. Each one is a generative model, a way to feed it data,
and a live panel that animates the posterior converging. Where a model has a
closed-form answer, the exact result is drawn on top of the approximation. This
page says what each demo shows and which inference method drives it.

All six share the same machinery: a `POST` starts a run, the engine streams
running summaries and a thinned subsample of draws over Server-Sent Events, and
the front end animates the result on the client clock (see
[architecture.md](architecture.md)).

## Coin

**Model:** Beta-Bernoulli. **Method:** importance sampling or Metropolis-Hastings.
**Overlay:** the exact Beta posterior.

Infer a coin's bias from a run of flips. Draw a bias `theta` once from a
`Beta(a, b)` prior, then score each flip's Bernoulli likelihood under `theta`. The
posterior is conjugate, `Beta(a + heads, b + tails)`, so the visualizer can draw
the exact Beta curve on top of the streamed histogram and you watch the bars rise
to meet it. This is the demo that most directly shows the project's premise: the
approximation and the truth in the same frame.

It is also the model the command-line tool runs, printing the posterior mean and
an equal-tailed credible interval beside the analytic answer.

## Regression

**Model:** single-coefficient Bayesian linear regression. **Method:** importance
sampling or Metropolis-Hastings. **Overlay:** the exact posterior mean.

Fit a line `y = w * x` to noisy points you can drag. Draw the slope `w` from a
broad Normal prior, then score each point's Normal likelihood under
`Normal(w * x, sigma)` with known noise. The fixture is conjugate Normal-Normal,
so the posterior on the slope is an exact Normal. The visualizer renders the
posterior as a Canvas **band**: many translucent lines `y = w_i * x` for slopes
drawn from the posterior, which composite into a wash that contracts onto the
trend as the run converges. The band is drawn with a per-frame line cap and the
slopes are sorted before tweening, so the spread narrows smoothly rather than
flickering.

## Localization

**Model:** one-dimensional source localization. **Method:** importance sampling or
Metropolis-Hastings. **Oracle:** planted-source recovery (no closed form is
assumed).

Locate a hidden source from noisy sensor reads. Draw the source position `theta`
once from a broad Normal prior, then score each reading under
`Normal(theta, sigma)`. The observation model is the direct-position read rather
than a noisy distance, on purpose: a distance read folds away the sign of the
offset and makes the posterior bimodal, which a single cloud reads poorly. The
visualizer renders a Canvas **cloud** of sampled positions, each particle's
opacity scaled by its weight, concentrating on the source as the reads accumulate.

## Method race

**Model:** localization. **Methods:** importance sampling and Metropolis-Hastings,
**side by side, at once.**

The same problem and the same data, run under both inference backends
simultaneously. The simultaneity is the whole point. A single start bumps two
**concurrent** runs; the server keys each by its own run id, so the two clouds
open their own streams and animate together. This is not importance-then-MH
sequencing. Each column carries its own cloud, diagnostics, and traceplot, and you
watch both methods collapse onto the same hidden source from the same reads,
comparing how each converges.

This demo is the clearest illustration of the core design: one model, two
interpreters, no change to either.

## Tracking

**Model:** a moving-target random walk, expressed as a state-space model.
**Method:** the sequential Monte Carlo particle filter. **Oracle:**
planted-trajectory recovery, cross-checked against an analytic Kalman filter.

Track a moving target from noisy reads. The hidden state is a one-dimensional
random walk `x_t ~ Normal(x_{t-1}, q)`, seen as `y_t ~ Normal(x_t, r)`. The
particle filter carries a weighted cloud of plausible positions forward,
resampling when the weight concentrates on too few particles. The visualizer
streams the filtering cloud step by step, so as the run advances the weighted
cloud walks along the trajectory and the estimate locks onto the target. This is
the showpiece: a live particle cloud following a hidden target over time.

The particle filter is held to the analytic Kalman filter step by step in the test
suite, so the cloud you see is the genuine filtering distribution, not an
approximation that merely looks plausible.

## Mixture

**Model:** a Gaussian mixture for clustering. **Method:** importance sampling on
the collapsed (Rao-Blackwellized) form. **Oracle:** planted-cluster recovery, with
label switching handled.

Cluster synthetic data with a Gaussian mixture over `k` clusters with known
weights and a known cluster spread; the unknowns are the `k` cluster means. The
posterior over the means concentrates onto the planted clusters as the run
converges.

Two design choices make this demo work, both explained in detail in
[inference.md](inference.md). First, the engine runs the **collapsed** form, with
the per-point cluster assignment summed out analytically, because the
assignment-sampling form degenerates under importance sampling (its effective
sample size collapses to about 1 and no particle budget recovers the means).
Second, because the mixture posterior is invariant to permuting the cluster
labels, the means are exchangeable; the server sorts each particle's mean vector
before emitting cluster locations, so the streamed means are identifiable and land
on the planted clusters rather than averaging together.
