# credence

A small probabilistic programming language embedded in Haskell, with a live
visualizer that animates inference as it runs. You write a short generative model,
give it data, and watch the posterior converge to the hidden answer: a coin's
bias emerging from a run of flips, a band of plausible regression lines collapsing
onto noisy points, a cloud of guesses tightening around a hidden location. Where a
model has an exact answer, the visualizer draws it on top of the approximation so
you can see the sampler land on the truth.

## What it is

A model in credence is a monadic computation over two primitives: draw from a
distribution, and observe data by weighting the execution by its likelihood. The
same model runs under several inference backends, because each backend is just an
interpreter of the model:

- Importance sampling, the simplest method and the first to ship.
- Trace-based Metropolis-Hastings, which gives each random choice a stable
  identity so a proposal can perturb one and recompute the rest.
- A sequential Monte Carlo particle filter, for streaming and time-series demos.

The engine works in log-space throughout, carries a seedable random generator, and
streams samples and running summaries to the web front end as it converges.

## What it is not

It is not a competitor to Stan, PyMC, Gen, or Turing. It does not do gradient-based
inference (no HMC or NUTS) and does not do variational inference. It is a
from-scratch, educational language whose point is to show inference happening and
to prove, against exact answers, that the approximation is right. The docs teach
the Bayesian machinery rather than assume it, including where the approximations
break down.

## Demos

- Coin bias: drag in a sequence of flips and watch the Beta posterior animate, with
  the exact conjugate answer overlaid.
- Bayesian linear regression: a scatter of noisy points and a band of plausible
  lines collapsing onto the trend, with the uncertainty shaded.
- Localization: noisy bearings to a hidden source and a posterior cloud
  concentrating on the spot.
- A method-race view comparing the inference backends on one problem, and a
  particle-filter tracking demo as the showpiece.

## Setup

The engine runs on GHC 9.6.7 with cabal 3.14.2.0, both installed through ghcup.
On Windows, run it inside WSL (Ubuntu); for fast file watching keep the repo on
the Linux filesystem under `~/projects` rather than under `/mnt/c`.

    # from the repo root
    cabal build all
    cabal freeze        # once, to lock the resolved plan
    cabal test
    cabal run credence-server   # serves on 127.0.0.1:3001

The front end runs on Node 24. Under WSL, localhost forwards to Windows, so the
dev server is reachable from a host browser.

    cd web
    npm ci
    npm run dev         # http://localhost:3000

## Tests

Correctness here cannot be eyeballed, because a wrong sampler produces a
plausible-looking posterior. The suite checks the approximate posterior against
closed-form answers (Beta-Bernoulli, Normal-Normal) within documented tolerances,
under fixed seeds, and runs on Linux in CI.

    cabal test

## Documentation

The `docs/` directory explains how the system works and why it is built the way it
is.

- [docs/README.md](docs/README.md) - overview and index.
- [docs/architecture.md](docs/architecture.md) - the model monad, the interpreter
  design, the distribution library, the trace, the streaming boundary, and the
  front end.
- [docs/inference.md](docs/inference.md) - how each inference method works and the
  correctness discipline that proves it against exact answers.
- [docs/development.md](docs/development.md) - setup, build and test commands, and
  the formatters.
- [docs/security.md](docs/security.md) - the local-demo security posture.
- [docs/demos.md](docs/demos.md) - the six gallery demos and what each shows.

## License

MIT. See LICENSE.
