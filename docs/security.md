# Security posture

credence is a local demo: a streaming engine on your own machine driving a
visualizer in your own browser. The security model fits that scope. The engine
binds loopback only and treats every request as untrusted input to be bounded
before any work begins, so a single request (or a bug in a local client) cannot
exhaust compute or memory. This page states the posture plainly, including the one
setting that is acceptable only because of the loopback bind.

## The loopback bind

The streaming server binds `127.0.0.1` explicitly. The port (`3001`) is not
exposed beyond the host. Under WSL, localhost forwards to the Windows host, so a
host browser reaches the engine over loopback without the port ever being public.

This bind is the foundation of everything below. The threat model is a local
client, or a bug in one, not a remote attacker. The defenses are about bounding
local resource use, not about authenticating or authorizing a network peer.

## Bounding an untrusted request

A run is started by a `POST` whose body is untrusted JSON. The body is decoded
first, and a failed decode returns `400` before any run is allocated, so a
malformed body never reaches a model constructor.

A decoded body is still untrusted, because the sample count and the dataset are
client-supplied. A pure validator (`validateRunRequest`, unit-tested without a
running server) bounds the request before a worker is spawned. It rejects, each
with a `400` whose message names the problem:

- a sample count that is **non-positive** (a run must draw at least one particle
  or step);
- a sample count **over the maximum** (so one request cannot ask for an unbounded
  computation);
- a dataset **longer than the maximum** (every batch rebuilds the model over the
  whole dataset, so the per-batch cost is linear in dataset length, and an
  unbounded dataset is the clearest way one request could exhaust memory or pin a
  worker);
- a dataset whose **constructor does not match the route** (so the coin route
  cannot be driven with regression points);
- an **empty tracking sequence** (the filter summarizes its last step, which does
  not exist for an empty sequence).

Only when every bound holds does the validator return the dataset a model can be
built from. An over-budget value is rejected rather than silently clamped, so a
client gets a clear error instead of a run that quietly does less than it asked.

Beyond the boundary check, the worker applies a second, smaller ceiling on how
long any single run streams, so even an in-bounds request produces a responsive,
finite convergence animation rather than one very long run.

## Back-pressure and concurrency limits

Each run gets its own bounded queue between its worker thread and its stream
handler. That bound is the back-pressure: when a run's queue fills, the worker
blocks on the enqueue until the client catches up, so a fast sampler cannot
outrun a slow client and the memory held between them stays capped, regardless of
how fast the sampler runs.

The number of concurrent live runs is also capped. Each `POST` spawns a worker
into its own queue; without a cap, a client that posts repeatedly without ever
draining (or whose runs block on a full queue because no one connected) would
accumulate workers without bound. Before inserting a new run the registry prunes
finished runs; if it is still at the cap, the `POST` is refused with a `503`
rather than spawning past the bound. A run also removes its own registry entry
when it completes, and a backstop prunes any worker that finished without
self-removing, so the live count reflects only genuinely running work.

Together these give three independent bounds: the validated, capped run length
bounds the per-run computation; each run's bounded queue caps its in-flight
memory regardless of producer speed; and the live-run cap bounds the worker count
regardless of how many requests arrive.

A client-supplied run id on the stream route only indexes the in-memory registry.
An unknown id reaches a benign "no active run" frame and grants no capability
beyond reading a stream the same origin started.

## What crosses the wire

Only finished, already-normalized summary statistics reach the browser. No raw
`observe`-style log-weight ever leaves the engine: the log-space weights stay
inside, and the wire carries running summaries plus a thinned subsample of draws
with their normalized weights. Keeping the subsample bounded is also what keeps
the streaming volume fixed per batch however large the run.

## The CORS header

The web app on `localhost:3000` calls the engine on `127.0.0.1:3001`, which is a
cross-origin request the browser blocks without a permissive
`Access-Control-Allow-Origin` header. The engine sends one that allows any origin.

This is wide open on purpose, and it is a development convenience that is safe
**only** because the server binds loopback and is unreachable off the host. On
loopback the header grants nothing a local process does not already have. If the
engine were ever bound to a public interface, this would have to be narrowed to
the specific web origin (or the header dropped and the app served same-origin).
The loopback bind is the precondition that makes the open header acceptable; the
two are a pair.

## Summary

| Concern | Defense |
| --- | --- |
| Remote access | The server binds `127.0.0.1` only; the port is not public. |
| Malformed request | JSON decode failure returns `400` before any run is allocated. |
| Oversized request | A pure validator caps sample count and dataset length and rejects mismatches, returning `400`. |
| Fast producer, slow client | A bounded per-run queue blocks the worker (back-pressure). |
| Worker accumulation | A live-run cap refuses excess runs with `503`; finished runs are pruned. |
| Weight leakage | Only normalized summary weights cross the wire; raw log-weights stay in the engine. |
| Cross-origin reads | A permissive CORS header, acceptable only because of the loopback bind. |
