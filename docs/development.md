# Development

This page covers setting up the toolchain, building and running both halves of
the project, the test suite, and the formatters.

## Prerequisites

The engine and the web front end have separate toolchains.

| Component | Version | Notes |
| --- | --- | --- |
| GHC | 9.6.7 | The Stackage LTS 22.44 compiler. |
| cabal | 3.14.2.0 | The build tool. |
| Node.js | 24 LTS | For the web front end. |

GHC and cabal are best installed through [ghcup](https://www.haskell.org/ghcup/),
which manages both. The Haskell library versions are pinned in `credence.cabal`
and locked by `cabal.project` together with a freeze file, so the resolved build
plan is reproducible. Do not widen those bounds casually; they are a known-good
set against GHC 9.6.7.

On Windows, run the engine inside WSL (Ubuntu). For fast file watching, keep the
repository on the Linux filesystem rather than under `/mnt/c`. Under WSL,
localhost forwards to the Windows host, so the web dev server and the engine are
both reachable from a host browser without extra configuration.

## The engine

Run these from the repository root.

```bash
cabal build all              # the first build also compiles dependencies
cabal freeze                 # once, to lock the resolved plan into a freeze file
cabal test                   # the statistical suite, under fixed seeds
cabal run credence           # the CLI
cabal run credence-server    # the streaming engine on 127.0.0.1:3001
```

The project builds three Haskell targets from one library:

- **`credence`** (library) -- the model monad, the distribution library, the
  trace, and the three inference interpreters. This is the heart of the project;
  the rest is wiring.
- **`credence`** (executable) -- a command-line tool that runs the coin model
  under importance sampling and prints the posterior mean and an equal-tailed
  credible interval next to the closed-form Beta answer. It is the human-facing
  proof the pipeline works.
- **`credence-server`** (executable) -- the Scotty streaming server that the web
  front end talks to.

### The CLI

The CLI runs the coin posterior end to end. It reads a flip sequence and a Beta
prior, runs importance sampling, and prints the estimate beside the exact answer:

```bash
cabal run credence -- --flips HHTHT --prior-a 2 --prior-b 2 --particles 100000
```

`--flips` is required (a string of `H`/`h` for heads and `T`/`t` for tails); the
prior defaults to `Beta(2, 2)` and the particle count to a value large enough to
land close to the analytic answer on a short sequence. Because it is a
demonstration rather than a statistical test, it seeds from system entropy, so
successive runs vary slightly around the printed analytic column.

## The front end

Run these from the `web/` directory. The front end is a Next.js application
(App Router) with React and TypeScript.

```bash
cd web
npm ci
npm run dev          # http://localhost:3000
npm run typecheck
npm run lint
npm run build
```

`npm run dev` serves the gallery and the live demos. With the engine running on
`127.0.0.1:3001`, each demo can start a run and stream its convergence.

Styling is bespoke editorial CSS with CSS variables and CSS Modules. There is no
Tailwind. The histograms and traceplots are SVG; the regression bands and
particle clouds are Canvas.

## The test suite

The test suite is the backbone of the project, not an afterthought. A wrong
sampler produces a plausible-looking posterior, so the only way to know inference
works is a check that would fail if it did not. The suite is built with `tasty`
and `tasty-hunit`, and it runs on Linux in CI with the seeds fixed, so a failure
in CI is a real failure rather than noise.

What the suite proves, in brief (the full account is in
[inference.md](inference.md)):

- **Conjugate agreement** on Beta-Bernoulli and Normal-Normal: importance
  sampling and Metropolis-Hastings match the analytic posterior in mean and
  variance.
- **Detailed balance:** the MH chain converges to a known stationary distribution,
  with a non-uniform prior so the Hastings correction is load-bearing.
- **Moment matching:** each distribution's sampler and log-density share one
  parameterization, which closes the shape/rate-versus-shape/scale trap.
- **Log-density agreement:** each density matches the independent `statistics`
  reference; out-of-domain inputs score `-Infinity`, never `NaN`.
- **Particle-filter sanity:** effective sample size behaves correctly, resampling
  is unbiased, and the per-step filtering mean and variance match an analytic
  Kalman filter.
- **Recovery:** planted parameters (a localization source, a tracking trajectory,
  mixture cluster means) are recovered from generated data.
- **Wire round-trip and request validation:** every wire type survives
  `decode . encode` unchanged, and the request validator bounds samples and
  dataset length and rejects mismatched or empty requests.

Three rules hold throughout:

1. **Every statistical test seeds the generator explicitly,** using `ST` with a
   fixed seed. An unseeded test is not a test.
2. **Tolerances are generous and documented.** Each carries a comment explaining
   its value in terms of sample count and estimator variance. A test that flakes
   one run in twenty is worse than none, because it teaches everyone to ignore
   red.
3. **`statistics` is a test-only dependency,** the independent oracle for
   log-densities. Runtime density code does not route through it, or the oracle
   and the implementation would be the same code and the check would be
   meaningless.

The front end has its own lightweight checks for the diagnostics helpers, run
with the standard Node test runner via `npm run` scripts.

## Formatters and linters

| Tool | Scope | Command |
| --- | --- | --- |
| fourmolu | Haskell formatting | `fourmolu --mode check $(git ls-files '*.hs')` |
| hlint | Haskell linting | `hlint .` |
| Biome | TypeScript and web lint/format | `npm run lint` (from `web/`) |

Biome handles both linting and formatting on the web side; ESLint and Prettier
are not used.

## Conventions

- Log-space everywhere. Combine log-probabilities with the stable `logSumExp`.
  Watch for `NaN` and underflow and fail loudly rather than returning a plausible
  wrong number.
- Correctness is proven against analytic posteriors, never by eye. Fixed seeds in
  every statistical test.
- Comments explain why, not what.
