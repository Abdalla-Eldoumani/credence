# credence documentation

credence is a small probabilistic programming language embedded in Haskell, with
a live visualizer that animates inference as it runs. You write a short
generative model, give it data, and watch the posterior converge toward the
hidden answer. Where a model has a closed-form answer, the visualizer draws it on
top of the approximation so you can see the sampler land on the truth.

These pages explain how the system works and why it is built the way it is. They
teach the Bayesian machinery rather than assume it, and they are honest about
where the approximations break down.

## Where to start

- **[architecture.md](architecture.md)** -- the whole system end to end: the
  model monad and its two primitives, the design where every inference method is
  an interpreter of one model, the distribution library, the execution trace, the
  streaming boundary, and the web front end.
- **[inference.md](inference.md)** -- how each inference method works (importance
  sampling, trace-based Metropolis-Hastings, the particle filter) and the
  correctness discipline that holds them to closed-form answers rather than to the
  eye. This is the project's distinctive value; read it.
- **[development.md](development.md)** -- setup, the build and test commands, the
  test suite, and the formatters.
- **[security.md](security.md)** -- the local-demo security posture: the loopback
  bind, request bounding, back-pressure, and what crosses the wire.
- **[demos.md](demos.md)** -- the six gallery demos and what each one shows.

## The one-paragraph version

A model is a monadic computation over two operations: `sample` draws from a
distribution, and `observe` weights the current execution by a likelihood. A
model is built as an inert syntax tree that performs no sampling on its own. Each
inference backend is an interpreter that walks that tree: importance sampling
draws from the prior and reweights, trace-based Metropolis-Hastings gives every
random choice a stable address so a proposal can perturb one and recompute the
rest, and a particle filter carries a population of weighted particles through a
state-space model. Everything works in log-space. A Haskell server runs the
inference on a worker thread and streams running summaries and a thinned
subsample of draws over Server-Sent Events to a Next.js front end, which animates
the posterior on the client clock with the analytic answer overlaid.

## What it is not

It is not a competitor to Stan, PyMC, Gen, or Turing. It does no gradient-based
inference (no HMC or NUTS) and no variational inference. It is a from-scratch,
educational language whose point is to show inference happening and to prove,
against exact answers, that the approximation is right.

## License

MIT. See [LICENSE](../LICENSE).
