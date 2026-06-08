# Architecture

credence has two halves joined by a JSON stream. The engine is a Haskell library
plus a small streaming server; the front end is a Next.js application that
animates what the engine sends. This page walks the engine from the model
representation outward to the wire, then crosses into the browser.

```
  model (sample / observe)         a model is an inert syntax tree
        |
        |  interpreted by
        v
  +---------------------+---------------------+---------------------+
  | importance sampling |  trace-based MH     |  particle filter    |
  +---------------------+---------------------+---------------------+
        |                     |                     |
        +---------------------+---------------------+
        |  running summaries + thinned subsample (JSON)
        v
  Scotty server  --- worker thread --> bounded queue --> SSE stream
        |
        |  text/event-stream over 127.0.0.1:3001
        v
  Next.js front end: SVG histograms / traceplots, Canvas clouds / bands,
  client-clock animation, analytic overlay
```

## The model monad

A model is a monadic computation over exactly two primitives:

- `sample :: Dist a -> Model a` draws a value from a distribution.
- `observe :: Double -> Model ()` factors the current execution by a likelihood,
  supplied as a log-likelihood increment.

Everything else is the `Monad` instance threading those two together. The
representation is operational and explicit: a `Model` is a GADT whose
constructors reify each instruction rather than hiding it behind a typeclass or a
transformer stack.

```haskell
data Model a where
    Pure   :: a -> Model a
    Sample :: Dist a -> (a -> Model b) -> Model b
    Observe :: Double -> Model b -> Model b   -- log-likelihood increment
```

`Sample d k` draws from `d` and continues with `k` applied to the drawn value;
the drawn value's type is existential, so an interpreter learns it only by
supplying a generator. `Observe w m` carries a log-likelihood increment `w` that
an interpreter sums into the execution's log-weight. Bind is a structural rewrite
over this tree and never forces a distribution or a weight:

```haskell
Pure x     >>= f = f x
Sample d k >>= f = Sample d (\x -> k x >>= f)
Observe w m >>= f = Observe w (m >>= f)
```

Because bind only rewrites structure, the monad laws hold by that same recursion,
and an interpreter is free to give `Sample` and `Observe` whatever meaning it
likes. Building a model performs no sampling and no IO. It allocates a structure
that an interpreter walks; the generator is threaded and the log-weight is
accumulated only inside the interpreter.

This is the load-bearing design choice. Reifying the program as an inert
structure is what lets one model be run by several interpreters and be inspected
by the visualizer, at the cost of more explicit plumbing than the
typeclass-and-transformer style. That trade favors transparency, which is the
point of a language built to teach. The design follows Scibior, Ghahramani, and
Gordon, "Practical Probabilistic Programming with Monads," Haskell Symposium
2015.

The payoff shows up every time a new model is added. The coin model, linear
regression, source localization, and the Gaussian mixture are each a single
`Model` value built only from `sample` and `observe`. None of them changes the
monad, and none of them changes any interpreter. A second, structurally different
model (a continuous latent with many observations, rather than one Beta draw) is
expressed in the same two primitives and interpreted by the same backends, with
no change to either.

## The distribution library

A `Dist a` reifies a distribution as the two operations inference needs of it: a
way to draw a value, and a way to score a value in log-space.

```haskell
data Dist a = Dist
    { sampleDist     :: forall g m. (StatefulGen g m) => g -> m a
    , logDensityDist :: a -> Double
    }
```

The sampler is rank-2 over the `StatefulGen` interface so one `Dist` runs both in
`ST s` (a fixed seed, for deterministic tests) and in `IO` (the streaming
engine), without committing to a monad at the type. The log-density is a pure
function returning a log-probability under the *same* parameterization the
sampler draws from. Six distributions are provided: Bernoulli, Beta, Normal,
Gamma, Categorical, and Uniform.

Pairing the sampler and the density in one value, under one parameterization, is
deliberate. The classic trap is a sampler and a density that are each correct for
a *different* parameterization of the same family (Gamma shape/rate against
shape/scale, for instance): every isolated test passes and the combination is
silently wrong. credence pairs them by construction and proves they agree with a
moment-matching test (see [inference.md](inference.md)). The samplers come from
`mwc-random`. The log-densities are written by hand over `math-functions` and
deliberately do not route through the `statistics` package, which is reserved as
the independent test oracle.

Two numerical disciplines run through the library. First, everything is in
log-space, and log-probabilities are combined with a numerically stable
`logSumExp` (factor out the maximum, so the largest term is `exp 0 = 1` and the
rest are well-scaled fractions). Second, support boundaries are guarded
explicitly so an impossible outcome scores `-Infinity` rather than `NaN`. At
`Beta(1,1)` evaluated at `x = 0`, the unguarded density computes `0 * log 0 =
NaN`, which would silently poison every downstream sum; the guard returns an
honest `-Infinity` instead. A categorical scores an out-of-range index as
`-Infinity` rather than panicking on an out-of-bounds vector read, because that
index is data reaching the density through `observe` in a user model.

## The execution trace

Trace-based Metropolis-Hastings needs each random choice to keep a stable
identity across runs, so a proposal can perturb one choice and recompute the
program with every other choice held fixed. The trace is the substrate for that.

A `Trace` records, for one run of a model, every value its `Sample` nodes drew,
keyed by the node's position in execution order, together with the run's total
`Observe` log-weight. This is the "database of values" of Wingate, Stuhlmuller,
and Goodman, "Lightweight Implementations of Probabilistic Programming Languages
Via Transformational Compilation," AISTATS 2011. Addressing is by execution-order
position: the first `Sample` is address 0, the next 1, and so on, assigned by a
counter the interpreter increments at each `Sample`.

For *fixed-structure* models, the only models MH is scoped to, this addressing is
stable across runs: the same control flow visits the same `Sample` nodes in the
same order, so address `n` always names the same choice. Models whose control
flow branches on a random outcome are out of scope for MH; importance sampling
handles them and needs no addressing.

Each `Entry` in the trace is existential over the drawn type (because `Sample`
hides it) and bundles three things at one address: the drawn value, its prior
log-density already evaluated at that value, and an action to redraw and rescore
from the same `Dist`. MH reads the per-site prior log-density to score a trace
and calls the resample action to propose a fresh value at one site. The one place
the hidden type must meet the continuation it feeds is replay, where a stored
value is fed back into its `Sample` node's continuation; that single coercion is
contained in one function and is sound exactly because fixed-structure addressing
guarantees address `n` carries the type the `n`-th `Sample` expects. Three tests
(address stability, detailed balance, and conjugate agreement) would all fail if
that assumption were ever violated.

## The inference interpreters

Each inference method is an interpreter of the model (or, for the particle
filter, of a state-space model expressed through the same distribution library).
All three are polymorphic over the generator, so the test suite drives them in
`ST` under a fixed seed and the server drives them in `IO`.

- **Importance sampling** runs the program forward, drawing each `Sample` from
  its prior and letting each `Observe` reweight the execution. One forward run is
  a particle: a sampled value paired with the total log-likelihood along that
  run. It needs no addressing, so it runs any model.
- **Trace-based Metropolis-Hastings** proposes a change to one random choice,
  recomputes the program with every other choice held fixed, and accepts or
  rejects by the general-form acceptance ratio in log-space.
- **The particle filter** carries a population of weighted particles through a
  state-space model, resampling when the weight concentrates on too few
  particles.

How each one works in detail, and how each is proven correct, is the subject of
[inference.md](inference.md).

## The streaming boundary

The boundary between the engine and the browser is a JSON stream defined in one
small module (`Credence.Wire`). It holds the wire types and the pure validation
that bounds an incoming request, and no inference at all. The server builds wire
values from the output of the already-tested interpreters; the client decodes
them.

What crosses the wire is deliberately not every sample. A run produces far more
samples than should be sent to a browser, so the wire carries running
**summaries** plus a thinned **subsample** of draws:

- A `Summary` is the converging posterior in a few numbers: the batch index, the
  posterior mean and variance, and two method-specific diagnostics that are
  absent when they do not apply (an MH acceptance rate, or an importance-sampling
  effective sample size).
- A `Batch` is a thinned handful of raw draws for a histogram, with optional
  per-draw normalized weights aligned with those draws (the opacity-by-weight
  channel the particle clouds read).
- A `StreamEvent` is the tagged envelope each stream frame carries: a batch, a
  summary, or a terminal done marker.

No raw `observe`-style log-weight ever leaves the engine. The log-space weights
stay inside; only finished, already-normalized summary statistics reach the wire.
Field names are camelCase and form a stable contract: the round-trip is checked
by a test, because a field that encodes one way and decodes another is a silent
data corruption between the engine and the visualizer.

### The server

The streaming server (built on Scotty and Warp) binds loopback only, on
`127.0.0.1:3001`. A `POST` to `/run/<model>` starts an inference run for a chosen
model, method, and dataset, and returns a fresh run id. A `GET` on
`/stream/<runId>` opens a Server-Sent Events stream (`text/event-stream`) that
pushes that run's batched samples and running summaries as `data:` lines while it
converges.

The concurrency model is one worker per run, feeding one bounded queue:

- Each run's inference loop runs on its own `Async` worker thread and hands
  events to the stream handler through its own bounded `TBQueue`.
- That bound *is* the back-pressure. When a run's queue fills, the worker's
  enqueue blocks, so a fast sampler cannot outrun a slow client and the memory
  held between them stays capped.
- Each `POST` mints a run id, spawns a worker into a fresh queue, and registers
  the keyed run in a shared map. Two `POST`s in flight do not cancel each other,
  so independent runs (importance and Metropolis-Hastings on the same data)
  stream side by side. This is what the method-race demo uses.
- A run removes its own entry when it finishes, and the number of live runs is
  bounded so abandoned runs cannot leak workers without limit. A backstop prunes
  any worker that finished without self-removing.

The worker drives the chosen method *incrementally*. A run grows in fixed-size
steps up to a capped length, emitting a summary and a thinned batch each step, so
the posterior is observable as a converging sequence rather than computed in one
shot. The single generator is threaded across every step, so each larger run
draws fresh randomness and the estimates genuinely converge. The server also
holds the latest summary per run, so a client that reconnects mid-run after a
dropped stream recovers the current state immediately rather than waiting for the
next batch.

Request bounding, the live-run cap, and the loopback bind are covered in
[security.md](security.md).

## The front end

The front end is a model gallery plus a live panel per demo. The boundary
contract on the browser side mirrors the wire schema exactly: an `EventSource`
client opens `/stream/<runId>`, parses each tagged event, and dispatches to
handlers for summaries, batches, and completion. The same client serves every
demo; concurrent runs stay independent because each is keyed by its own run id.

Rendering is split between SVG and Canvas by what each is good at:

- **SVG** draws the posterior histograms and the traceplots. The coin demo, for
  example, accumulates a running histogram of streamed draws across batches,
  normalizes it to a density, and draws it as a row of bars, with the exact
  analytic Beta curve as a path on top.
- **Canvas** draws the regression bands and the particle clouds, where many
  translucent primitives (hundreds of lines or dots) need to composite into a
  wash. The renderer draws at most a bounded number of lines per frame, so a
  large sample set never stalls the main thread.

The animation is paced on the **client clock**, not on network arrival. A tween
holds a current array of values and a target array, and each animation frame eases
the current toward the target with a cubic-bezier curve evaluated in JS over a
fixed window (around 210 ms). When a new summary arrives, it updates the target;
a burst of arrivals re-aims the tween rather than snapping the bars. This
decoupling of render pacing from network arrival is the motion contract that
keeps the convergence smooth under a slow or bursty stream. For the regression
band and the particle clouds, the rendered values are sorted before tweening, so
the tween is a contraction of order statistics: the spread narrows smoothly as
the posterior concentrates, which is the band collapsing onto the trend or the
cloud tightening onto the source. A `prefers-reduced-motion` setting jumps
straight to each target instead of easing.

The **analytic overlay** is the project's signature. Where a model has a
closed-form answer, the exact posterior is drawn on top of the streamed
approximation: the Beta curve for the coin, the exact mean for regression. The
overlay shares its density function with the empty-state prior, so the curve the
sampler is converging toward is the same one drawn before any data arrives. Live
diagnostics (the acceptance rate for Metropolis-Hastings, the effective sample
size for importance sampling) are shown beside the figure and flag a run that hit
its length cap without settling.

## Sources

The design and the methods rest on published work, cited here because the docs
teach the machinery:

- Scibior, Ghahramani, and Gordon, "Practical Probabilistic Programming with
  Monads," Haskell Symposium 2015 -- the monadic representation where inference
  methods are interpreters of one model.
- Wingate, Stuhlmuller, and Goodman, "Lightweight Implementations of
  Probabilistic Programming Languages Via Transformational Compilation," AISTATS
  2011 -- the trace as a database of addressed random choices, and the
  general-form acceptance ratio for single-site MH.
- Doucet and Johansen, "A Tutorial on Particle Filtering and Smoothing: Fifteen
  Years Later" (2009), and Arulampalam, Maskell, Gordon, and Clapp, "A Tutorial
  on Particle Filters for Online Nonlinear/Non-Gaussian Bayesian Tracking" (IEEE
  Transactions on Signal Processing, 2002) -- the sequential-importance-resampling
  filter and systematic resampling.
