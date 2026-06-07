{-# LANGUAGE OverloadedStrings #-}

{- | The inference engine's streaming HTTP surface.

A POST starts an inference run for a chosen model, method, and dataset and returns
a fresh 'RunId'; a GET on @\/stream\/\<runId\>@ opens a Server-Sent Events stream
(@text\/event-stream@) that pushes that run's batched samples and running summaries
as @data:@ lines while it converges. Each run's inference loop runs on its OWN
thread (an 'Async' worker) and hands events to the stream handler through its OWN
BOUNDED 'TBQueue'. That bound is the back-pressure: when a run's queue fills, its
worker's 'writeTBQueue' blocks, so a fast sampler cannot outrun a slow client and
memory between the two stays capped (the edge-case notes, streaming volume). WSL2
forwards localhost to the Windows host, so this loopback port is reachable from a
host browser without extra configuration.

Concurrent per-run streams keyed by RunId: each POST mints a RunId, spawns a worker
into a fresh bounded queue, and registers the keyed run in a shared map; two POSTs
in flight do not cancel each other, so independent runs (importance and MH on the
same data) stream side by side. A run removes its own entry at 'EvDone', and the
number of live runs is bounded ('maxLiveRuns') so abandoned runs cannot leak
workers unboundedly.

The server runs the already-verified inference unchanged: the worker only calls
'importanceSamples' and 'mhChain' (and 'posteriorMean'/'normalizedLogWeights' to
summarize), then shapes their output into 'Credence.Wire' values. No sampler,
density, or acceptance ratio is reimplemented here; correctness lives in the
library and its tests.

Trust boundaries (see the plan's threat model): the POST body is untrusted JSON,
so it is decoded with @aeson@ and a failed decode returns 400 before any run is
allocated rather than feeding junk into a model constructor (the plan). A
client-supplied RunId on @\/stream\/\<id\>@ only indexes the in-memory map; an
unknown id reaches a benign no-active-run frame and grants no capability beyond
reading a stream the same origin started (the plan). The development CORS header
opens cross-origin reads, accepted because the server binds 127.0.0.1 only. Each
run's bounded queue caps its in-flight memory regardless of producer speed
(the plan), the live-run cap bounds the worker count regardless of POST volume
(the plan), and the run length is capped so a huge @samples@ value cannot drive an
unbounded computation.
-}
module Main (main) where

import Control.Concurrent.Async (Async, async, poll)
import Control.Concurrent.STM (
    TBQueue,
    TVar,
    atomically,
    modifyTVar',
    newTBQueueIO,
    newTVarIO,
    readTBQueue,
    readTVar,
    readTVarIO,
    writeTBQueue,
    writeTVar,
 )
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Primitive (RealWorld)
import Credence.Inference.Importance (importanceSamples, normalizedLogWeights, posteriorMean)
import Credence.Inference.MH (mhChain)
import Credence.Model (Model, coinModel)
import Credence.Model.Localization (localize)
import Credence.Model.Regression (linearRegression)
import Credence.Wire (
    Batch (..),
    Dataset (..),
    Method (..),
    RunRequest (..),
    RunStarted (..),
    StreamEvent (..),
    Summary (..),
 )
import Data.Aeson (decode, encode)
import Data.ByteString.Builder (Builder, lazyByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Network.HTTP.Types.Status (status400, status503)
import Network.Wai.Handler.Warp (setHost, setPort)
import System.Random.MWC (Gen, createSystemRandom)
import Web.Scotty (
    ActionM,
    Options (..),
    body,
    captureParam,
    defaultOptions,
    get,
    post,
    raw,
    scottyOpts,
    setHeader,
    status,
    stream,
 )

{- | The loopback port the engine binds. Per the project notes the streaming engine lives
on 127.0.0.1:3001; WSL2 forwards localhost so a host browser reaches it.
-}
enginePort :: Int
enginePort = 3001

{- | The bound on each run's worker-to-stream queue. Small on purpose: it is the
back-pressure budget, not a buffer to smooth bursts. A producer that gets this far
ahead of its drainer blocks on 'writeTBQueue' until the client catches up, so a
run's in-flight memory never exceeds this many events no matter how fast the
sampler runs (the plan, edge case streaming volume). 64 is enough to keep the
stream busy without letting the worker race ahead.
-}
queueBound :: Int
queueBound = 64

{- | The maximum number of live runs the registry holds at once. Each POST spawns a
worker into its own queue; without a cap, a client that POSTs repeatedly without
ever draining (or whose runs block on a full queue because no one connected) would
accumulate workers without bound (the plan). 8 is generous for the demos (the method
race needs two at a time) while still bounding the worker and memory count. Before
inserting a new run the registry first prunes finished runs; if it is still at this
cap the POST is refused with a 503 rather than spawning past the bound.
-}
maxLiveRuns :: Int
maxLiveRuns = 8

{- | The maximum run length the worker will drive to, regardless of the @samples@
value in the request. A request asking for far more is silently capped to this,
which bounds the per-run computation so a large (or hostile) @samples@ cannot pin
the engine. Within the cap the run is driven incrementally so the posterior is
observable as it converges, rather than computed in one shot.
-}
runCap :: Int
runCap = 20000

{- | The increment the run grows by each batch. The worker emits a batch every
@batchStep@ additional samples, so a run of @n@ samples produces about
@n / batchStep@ summary updates. Smaller means a smoother convergence animation
and more frames; larger means fewer, coarser updates. This is the
\"drive in increments\" knob the plan calls for.
-}
batchStep :: Int
batchStep = 500

{- | The number of raw draws a 'Batch' carries to the client. The engine may draw
tens of thousands of values; only this many, evenly thinned, reach the browser
per batch (the edge-case notes, streaming volume). The wire 'Batch' type makes
the subsample explicit; this is its size.
-}
subsampleSize :: Int
subsampleSize = 60

{- | A run's identity: an opaque text key into the registry, allocated from a
monotonic counter (rendered as decimal text). It is only ever an index into an
in-memory map and carries no capability beyond reading the stream it names.
-}
type RunId = Text

{- | The live state of one run. Each run owns its channel and worker independently
of every other, which is what lets runs stream concurrently.

'runQueue' is the bounded channel this run's worker fills and its stream drains.
'runLatest' holds the most recent 'Summary' (or 'Nothing' until the first batch),
so a client that connects or reconnects mid-run gets the current state at once
rather than waiting for the next batch (edge case SSE drop, reconnect resume).
'runWorker' is the worker handle, kept so the registry can poll whether it has
finished when pruning.
-}
data RunState = RunState
    { runQueue :: TBQueue StreamEvent
    , runLatest :: TVar (Maybe Summary)
    , runWorker :: Async ()
    }

{- | The shared run registry: the live runs keyed by 'RunId' and the source of the
next id. 'regRuns' is the map every POST inserts into and every @\/stream\/\<id\>@
looks up; 'regNextId' is the monotonic counter the next RunId is rendered from. A
single 'Registry' is created in 'main' and shared across all requests.
-}
data Registry = Registry
    { regRuns :: TVar (Map RunId RunState)
    , regNextId :: TVar Int
    }

main :: IO ()
main = do
    -- The shared registry of live runs. Each POST inserts a keyed run; each
    -- /stream/<id> looks one up. Runs do not replace each other.
    registry <-
        Registry
            <$> newTVarIO Map.empty
            <*> newTVarIO 0
    let opts =
            defaultOptions
                { -- Bind loopback only: the port must not be exposed beyond the host.
                  -- setHost "127.0.0.1" makes Warp listen on 127.0.0.1.
                  settings = setHost "127.0.0.1" (setPort enginePort (settings defaultOptions))
                }
    scottyOpts opts $ do
        -- POST starts a run for each model. The route fixes which model the
        -- request's Dataset must match; a mismatched dataset is a 400. The
        -- response carries the fresh RunId the client then streams.
        post "/run/coin" (startRun registry Coin{datasetFlips = []})
        post "/run/regression" (startRun registry Regression{datasetPoints = []})
        post "/run/localization" (startRun registry Localization{datasetReadings = []})
        -- GET drains one run's queue as an event stream, keyed by the RunId
        -- captured from the path.
        get "/stream/:runId" (streamRun registry)

{- | Decode the POSTed 'RunRequest', allocate a fresh 'RunId', spawn its worker,
register the keyed run, and return the RunId as JSON.

The body is untrusted, so it is decoded with @aeson@'s 'decode' and a parse failure
returns 400 with no run allocated (the plan): a malformed body never reaches a model
constructor. The first argument is a witness of which dataset constructor this route
expects (its contents are ignored); if the decoded request carries the other
constructor, that too is a 400, so @\/run\/coin@ cannot be driven with regression
points. On success a new RunId, bounded queue, and worker are created and inserted
into the registry; the worker begins feeding its queue at once. If the registry is
already full ('maxLiveRuns', after pruning finished runs) the run is refused with a
503 rather than spawning past the bound.
-}
startRun :: Registry -> Dataset -> ActionM ()
startRun registry expected = do
    reqBody <- body
    case decode reqBody of
        Nothing -> badRequest "could not parse run request as json"
        Just req ->
            case matchDataset expected (runRequestDataset req) of
                Nothing -> badRequest "dataset does not match this route"
                Just dataset -> do
                    mRunId <- liftIO (launch registry req dataset)
                    case mRunId of
                        Nothing -> do
                            status status503
                            corsHeader
                            raw "too many live runs, retry shortly"
                        Just runId -> do
                            corsHeader
                            setHeader "Content-Type" "application/json"
                            raw (encode (RunStarted{runStartedRunId = runId}))
  where
    badRequest msg = do
        status status400
        corsHeader
        raw (TLE.encodeUtf8 msg)

{- | Confirm the decoded dataset is the constructor this route serves, returning it
normalized. @\/run\/coin@ accepts only 'Coin', @\/run\/regression@ only
'Regression'; anything else is 'Nothing' and the caller turns that into a 400. This
keeps an invalid dataset from ever reaching a model constructor, the same defense
the JSON decode gives against a malformed body.
-}
matchDataset :: Dataset -> Dataset -> Maybe Dataset
matchDataset Coin{} d@Coin{} = Just d
matchDataset Regression{} d@Regression{} = Just d
matchDataset Localization{} d@Localization{} = Just d
matchDataset _ _ = Nothing

{- | Allocate and register a fresh run, or refuse if the registry is full.

Pruning first removes any finished runs (their workers have completed and
self-removed at 'EvDone', but a run whose client disconnected before draining can
finish without its key being read again; pruning by 'poll' is the backstop). If the
live count is still at 'maxLiveRuns' the run is refused ('Nothing'). Otherwise a new
RunId is drawn from the counter, a fresh bounded queue and latest-summary slot are
built, the worker is spawned, and the keyed 'RunState' is inserted. Each run gets
its OWN queue at 'queueBound', so the back-pressure contract is per-run and one run
filling its queue never blocks another. Returns the new RunId.
-}
launch :: Registry -> RunRequest -> Dataset -> IO (Maybe RunId)
launch registry req dataset = do
    pruneFinished registry
    live <- Map.size <$> readTVarIO (regRuns registry)
    if live >= maxLiveRuns
        then pure Nothing
        else do
            runId <- atomically $ do
                n <- readTVar (regNextId registry)
                writeTVar (regNextId registry) (n + 1)
                pure (T.pack (show n))
            queue <- newTBQueueIO (fromIntegral queueBound)
            latest <- newTVarIO Nothing
            gen <- createSystemRandom
            worker <- async (runWorkerLoop registry runId req dataset queue latest gen)
            atomically $
                modifyTVar'
                    (regRuns registry)
                    (Map.insert runId RunState{runQueue = queue, runLatest = latest, runWorker = worker})
            pure (Just runId)

{- | Drop runs whose worker has finished from the registry.

A run normally deletes its own key at 'EvDone' (see 'runWorkerLoop'), but a worker
that finished without that path (or a future failure mode) would otherwise linger.
This polls every run's 'Async' and removes the ones that have completed, so the
live count 'launch' checks against 'maxLiveRuns' reflects only genuinely running
work. It is the leak backstop the cap depends on.
-}
pruneFinished :: Registry -> IO ()
pruneFinished registry = do
    runs <- readTVarIO (regRuns registry)
    finished <- mapMaybeFinished (Map.toList runs)
    unless (null finished) $
        atomically $
            modifyTVar' (regRuns registry) (\m -> foldr Map.delete m finished)
  where
    -- The keys whose worker has completed (poll returns Just).
    mapMaybeFinished [] = pure []
    mapMaybeFinished ((rid, st) : rest) = do
        done <- poll (runWorker st)
        let here = case done of
                Just _ -> [rid]
                Nothing -> []
        (here <>) <$> mapMaybeFinished rest

{- | The worker: drive the chosen method incrementally, pushing a 'Summary' and a
'Batch' per step and a terminal 'EvDone' at the end, then remove this run from the
registry.

The run grows by 'batchStep' each iteration up to @min samples runCap@, so the
posterior is emitted as a converging sequence rather than one final answer. Each
step calls the verified interpreter for the chosen method on the model built from
the dataset, computes the running summary (mean and variance, plus the
method-specific diagnostic), thins a subsample for the histogram, and writes both
events to this run's bounded queue. 'writeTBQueue' blocks when the queue is full,
which is the back-pressure that keeps the worker from outrunning the drainer. The
single generator is threaded across every step, so each larger run draws fresh
randomness and the estimates genuinely converge rather than repeating. After
writing 'EvDone' the worker deletes its own key, so a completed run does not linger
in the registry (the self-removal half of the leak guard).
-}
runWorkerLoop ::
    Registry ->
    RunId ->
    RunRequest ->
    Dataset ->
    TBQueue StreamEvent ->
    TVar (Maybe Summary) ->
    Gen RealWorld ->
    IO ()
runWorkerLoop registry runId req dataset queue latest gen = do
    let total = min (max 1 (runRequestSamples req)) runCap
        -- The batch boundaries: 500, 1000, ..., up to total, ending exactly at total.
        steps = batchSizes total
    forM_ (zip [0 ..] steps) $ \(idx, n) -> do
        (summ, draws, weights) <- computeBatch (runRequestMethod req) dataset n idx gen
        -- Record the latest summary for reconnect resume, then enqueue both the
        -- summary and the thinned batch. The writes block if the client is behind.
        atomically (writeTVar latest (Just summ))
        atomically (writeTBQueue queue (EvSummary summ))
        atomically (writeTBQueue queue (EvBatch (Batch{batchBatchIndex = idx, batchDraws = draws, batchWeights = weights})))
    atomically (writeTBQueue queue EvDone)
    -- Self-removal: a completed run drops out of the registry so its slot frees up
    -- for the next POST. The drainer may still read the already-enqueued EvDone.
    atomically (modifyTVar' (regRuns registry) (Map.delete runId))

{- | The increasing run lengths the worker steps through, e.g. for total 1700 and
step 500 this is @[500, 1000, 1500, 1700]@. Always ends exactly at @total@ so the
final batch reflects the full requested run, and never exceeds it. A @total@ at or
below one step is a single batch.
-}
batchSizes :: Int -> [Int]
batchSizes total = go batchStep
  where
    go n
        | n >= total = [total]
        | otherwise = n : go (n + batchStep)

{- | Run one batch of the chosen method at run length @n@ and summarize it into a
'Summary' (tagged with the batch index), a thinned list of raw draws, and the
optional per-draw normalized weights aligned with those draws.

For 'Importance': draw @n@ particles, take the weighted posterior mean and the
weighted second moment (variance @= E[x^2] - mean^2@), and the effective sample
size @1 / sum(normalizedWeight^2)@ from the log-weight column; acceptance rate is
'Nothing' (importance has no accept step). The returned weights are the normalized
importance weights thinned by the SAME stride and in the SAME order as the draws (so
@weights !! i@ belongs to @draws !! i@), which is the opacity-by-weight input the
cloud reads (the edge-case notes: only the bounded subsample, with its weights,
crosses the wire). For 'MH': run an @n@-step chain, take the plain mean and variance
over the visited values and the measured acceptance rate from 'mhChain'; ESS is
'Nothing' (the chain ESS is an autocorrelation-based quantity not computed here), and
the weights are 'Nothing' (the chain has no per-draw importance weight). The
subsample is an even thinning of the latent values for the client histogram. Every
statistic comes from the verified library functions; this only arranges their output.
-}
computeBatch ::
    Method ->
    Dataset ->
    Int ->
    Int ->
    Gen RealWorld ->
    IO (Summary, [Double], Maybe [Double])
computeBatch method dataset n idx gen =
    case method of
        Importance -> do
            samples <- importanceSamples n (buildModel dataset) gen
            let mean = posteriorMean samples id
                second = posteriorMean samples (\x -> x * x)
                var = max 0 (second - mean * mean)
                -- ESS from the normalized importance weights: 1 / sum(w_i^2).
                logWs = VU.convert (V.map snd samples)
                normW = normalizedLogWeights logWs
                ess = 1 / VU.sum (VU.map (\w -> w * w) normW)
                draws = thin subsampleSize (V.toList (V.map fst samples))
                -- Thin the normalized weights by the SAME stride and order as the
                -- draws so weights !! i belongs to draws !! i. Both lists are length
                -- n before thinning, and thin is a deterministic function of length,
                -- so the kept indices coincide and the alignment holds.
                weights = thin subsampleSize (VU.toList normW)
                summ =
                    Summary
                        { summaryBatchIndex = idx
                        , summaryMean = mean
                        , summaryVariance = var
                        , summaryAcceptanceRate = Nothing
                        , summaryEffectiveSampleSize = Just ess
                        }
            pure (summ, draws, Just weights)
        MH -> do
            (chain, rate) <- mhChain n (buildModel dataset) gen
            let cn = fromIntegral (V.length chain)
                mean = V.sum chain / cn
                second = V.sum (V.map (\x -> x * x) chain) / cn
                var = max 0 (second - mean * mean)
                draws = thin subsampleSize (V.toList chain)
                summ =
                    Summary
                        { summaryBatchIndex = idx
                        , summaryMean = mean
                        , summaryVariance = var
                        , summaryAcceptanceRate = Just rate
                        , summaryEffectiveSampleSize = Nothing
                        }
            pure (summ, draws, Nothing)

{- | Build the 'Credence.Model' the run conditions on from its 'Dataset'.

The coin model uses a Beta(2,2) prior, the same weakly-informative prior the
conjugate gate runs on; the regression model uses the broad Normal(0,3) prior and
unit noise the Normal-Normal gate uses; the localization model uses the broad
Normal(0,5) prior and unit sensor noise the recovery test plants the source under.
These priors are fixed here rather than sent on the wire: the demos are about
visualizing convergence for a known model, not about tuning priors from the client.
The model is structure only; building it draws nothing, and the chosen interpreter
is what runs it.
-}
buildModel :: Dataset -> Model Double
buildModel (Coin flips) = coinModel 2 2 flips
buildModel (Regression pts) = linearRegression 0 3 1 pts
buildModel (Localization readings) = localize 0 5 1 readings

{- | Evenly thin a list to at most @k@ elements, preserving order and span.

For a histogram the client needs a representative spread, not the first @k@ or a
random @k@, so this strides through the list keeping every @(length / k)@-th
element. A list already at or under @k@ is returned whole. This is the subsample
the 'Batch' carries; it is bounded so the wire volume per batch is fixed however
large the run.
-}
thin :: Int -> [Double] -> [Double]
thin k xs
    | len <= k = xs
    | otherwise = [x | (i, x) <- zip [0 ..] xs, i `mod` stride == 0]
  where
    len = length xs
    stride = max 1 (len `div` k)

{- | Drain one run's queue to the client as a @text\/event-stream@, keyed by the
'RunId' captured from the path.

Sets the SSE content type and @Cache-Control: no-cache@ so proxies and the browser
do not buffer or cache the stream, and the development CORS header (see
'corsHeader'). The RunId is read from the @\/stream\/\<runId\>@ capture and looked
up in the registry. On a hit, the run's latest 'Summary' is sent first so a client
connecting or reconnecting mid-run recovers the current state immediately
(edge case SSE drop, reconnect resume) rather than waiting for the next batch, then
the loop writes one SSE frame per event as @data: <json>\\n\\n@, flushing after each
so events arrive promptly, and stops when it reads 'EvDone'. On a miss (an unknown
or already-completed RunId) it sends a single comment frame and returns, so the
connection does not hang.
-}
streamRun :: Registry -> ActionM ()
streamRun registry = do
    runId <- captureParam "runId"
    setHeader "Content-Type" "text/event-stream"
    setHeader "Cache-Control" "no-cache"
    corsHeader
    runs <- liftIO (readTVarIO (regRuns registry))
    case Map.lookup runId runs of
        Nothing ->
            -- Unknown run: a single comment frame keeps the connection from hanging
            -- and tells a client to POST first. EventSource ignores comment lines.
            stream $ \send flush -> do
                send (lazyByteString ": no active run for this id, post to /run/coin, /run/regression, or /run/localization first\n\n")
                flush
        Just st ->
            stream $ \send flush -> do
                -- Reconnect resume: replay the latest summary if one exists.
                latest <- readTVarIO (runLatest st)
                forM_ latest $ \s -> do
                    send (sseFrame (EvSummary s))
                    flush
                drain (runQueue st) send flush

{- | The read-and-emit loop: pull events off a run's bounded queue and write each as
an SSE frame until 'EvDone'.

Blocking on 'readTBQueue' is the consumer half of the back-pressure: the drainer
takes events as fast as it can write them and no faster, and because the queue is
bounded the worker blocks once it gets ahead. Each event is flushed immediately so
the convergence animation downstream is driven by real arrivals. 'EvDone' is
emitted to the client and then ends the loop, closing the stream.
-}
drain :: TBQueue StreamEvent -> (Builder -> IO ()) -> IO () -> IO ()
drain queue send flush = go
  where
    go = do
        ev <- atomically (readTBQueue queue)
        send (sseFrame ev)
        flush
        when (notDone ev) go
    notDone EvDone = False
    notDone _ = True

{- | Render one 'StreamEvent' as a single SSE frame: @data: <json>\\n\\n@.

SSE delimits events by a blank line and prefixes each data line with @data:@. The
event is encoded to JSON with @aeson@ and wrapped in exactly that framing, so the
client's EventSource @onmessage@ receives the JSON as the event data. The double
newline terminates the event.
-}
sseFrame :: StreamEvent -> Builder
sseFrame ev = lazyByteString ("data: " <> encode ev <> "\n\n")

{- | The development CORS header: allow any origin to read the engine.

The web app on localhost:3000 calls the engine on 127.0.0.1:3001, a cross-origin
request the browser blocks without this header (edge case CORS). It is wide open
on purpose for local development. This MUST be tightened before any non-local
deployment: the server is loopback-bound (127.0.0.1) so this is safe locally, but
a public bind with @*@ here would expose the engine to any site. Tighten to the
specific web origin (or drop the header and serve same-origin) before deploying.
-}
corsHeader :: ActionM ()
corsHeader = setHeader "Access-Control-Allow-Origin" "*"
