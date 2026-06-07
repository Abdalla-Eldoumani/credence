{-# LANGUAGE OverloadedStrings #-}

{- | The inference engine's streaming HTTP surface.

A POST starts an inference run for a chosen model, method, and dataset; a GET
opens a Server-Sent Events stream (@text/event-stream@) that pushes batched
samples and running summaries as @data:@ lines while the run converges. The
inference loop runs on its OWN thread (an 'Async' worker) and hands events to the
stream handler through a BOUNDED 'TBQueue'. That bound is the back-pressure: when
the queue fills, the worker's 'writeTBQueue' blocks, so a fast sampler cannot
outrun a slow client and memory between the two stays capped (the edge-case notes,
streaming volume). WSL2 forwards localhost to the Windows host, so this loopback
port is reachable from a host browser without extra configuration.

The server runs the already-verified inference unchanged: the worker only calls
'importanceSamples' and 'mhChain' (and 'posteriorMean'/'normalizedLogWeights' to
summarize), then shapes their output into 'Credence.Wire' values. No sampler,
density, or acceptance ratio is reimplemented here; correctness lives in the
library and its tests.

Trust boundaries (see the plan's threat model): the POST body is untrusted JSON,
so it is decoded with @aeson@ and a failed decode returns 400 rather than feeding
junk into a model constructor (the plan). The development CORS header opens
cross-origin reads, accepted because the server binds 127.0.0.1 only (the plan,
the plan). The bounded queue is the sole channel from worker to stream, capping
in-flight memory regardless of producer speed (the plan), and the run length is
capped so a huge @samples@ value cannot drive an unbounded computation (the plan).
-}
module Main (main) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (
    TBQueue,
    TVar,
    atomically,
    newTBQueueIO,
    newTVarIO,
    readTBQueue,
    readTVarIO,
    writeTBQueue,
    writeTVar,
 )
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Primitive (RealWorld)
import Credence.Inference.Importance (importanceSamples, normalizedLogWeights, posteriorMean)
import Credence.Inference.MH (mhChain)
import Credence.Model (Model, coinModel)
import Credence.Model.Regression (linearRegression)
import Credence.Wire (
    Batch (..),
    Dataset (..),
    Method (..),
    RunRequest (..),
    StreamEvent (..),
    Summary (..),
 )
import Data.Aeson (decode, encode)
import Data.ByteString.Builder (Builder, lazyByteString)
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Network.HTTP.Types.Status (status400)
import Network.Wai.Handler.Warp (setHost, setPort)
import System.Random.MWC (Gen, createSystemRandom)
import Web.Scotty (
    ActionM,
    Options (..),
    body,
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

{- | The bound on the worker-to-stream queue. Small on purpose: it is the
back-pressure budget, not a buffer to smooth bursts. A producer that gets this
far ahead of the drainer blocks on 'writeTBQueue' until the client catches up, so
in-flight memory never exceeds this many events no matter how fast the sampler
runs (the plan, edge case streaming volume). 64 is enough to keep the stream busy
without letting the worker race ahead.
-}
queueBound :: Int
queueBound = 64

{- | The maximum run length the worker will drive to, regardless of the @samples@
value in the request. A request asking for far more is silently capped to this,
which bounds the per-run computation so a large (or hostile) @samples@ cannot pin
the engine (the plan). Within the cap the run is driven incrementally so the
posterior is observable as it converges, rather than computed in one shot.
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

{- | The live state of the single current run, or 'Nothing' before the first POST.

One run at a time: a new POST cancels the previous worker and replaces this whole
record. 'runQueue' is the bounded channel the worker fills and the stream drains.
'runLatest' holds the most recent 'Summary' (or 'Nothing' until the first batch),
so a client that connects or reconnects mid-run gets the current state at once
rather than waiting for the next batch (edge case SSE drop, reconnect resume).
'runWorker' is the worker handle, kept so the next POST can 'cancel' it.
-}
data RunState = RunState
    { runQueue :: TBQueue StreamEvent
    , runLatest :: TVar (Maybe Summary)
    , runWorker :: Async ()
    }

main :: IO ()
main = do
    -- The shared slot for the current run. Starts empty; a POST fills it and a
    -- later POST swaps it, cancelling the prior worker first.
    runRef <- newTVarIO (Nothing :: Maybe RunState)
    let opts =
            defaultOptions
                { -- Bind loopback only: the port must not be exposed beyond the host
                  -- (the plan). setHost "127.0.0.1" makes Warp listen on 127.0.0.1.
                  settings = setHost "127.0.0.1" (setPort enginePort (settings defaultOptions))
                }
    scottyOpts opts $ do
        -- POST starts (or restarts) a run for each model. The route fixes which
        -- model the request's Dataset must match; a mismatched dataset is a 400.
        post "/run/coin" (startRun runRef Coin{datasetFlips = []})
        post "/run/regression" (startRun runRef Regression{datasetPoints = []})
        -- GET drains the current run's queue as an event stream.
        get "/stream" (streamRun runRef)

{- | Decode the POSTed 'RunRequest', cancel any in-flight worker, and spawn a
fresh one for the chosen method and dataset.

The body is untrusted, so it is decoded with @aeson@'s 'decode' and a parse
failure returns 400 with no work started (the plan): a malformed body never
reaches a model constructor. The first argument is a witness of which dataset
constructor this route expects (its contents are ignored); if the decoded request
carries the other constructor, that too is a 400, so @/run/coin@ cannot be driven
with regression points. On success the previous run's worker is cancelled and its
state replaced atomically, then a new bounded queue and worker are installed and
the worker begins feeding the queue.
-}
startRun :: TVar (Maybe RunState) -> Dataset -> ActionM ()
startRun runRef expected = do
    reqBody <- body
    case decode reqBody of
        Nothing -> badRequest "could not parse run request as json"
        Just req ->
            case matchDataset expected (runRequestDataset req) of
                Nothing -> badRequest "dataset does not match this route"
                Just dataset -> do
                    liftIO (launch runRef req dataset)
                    corsHeader
                    -- A tiny ack body; the client cares about the stream, not this.
                    raw "started"
  where
    badRequest msg = do
        status status400
        corsHeader
        raw (TLE.encodeUtf8 msg)

{- | Confirm the decoded dataset is the constructor this route serves, returning
it normalized. @/run/coin@ accepts only 'Coin', @/run/regression@ only
'Regression'; anything else is 'Nothing' and the caller turns that into a 400.
This keeps an invalid dataset from ever reaching a model constructor, the same
defense the JSON decode gives against a malformed body.
-}
matchDataset :: Dataset -> Dataset -> Maybe Dataset
matchDataset Coin{} d@Coin{} = Just d
matchDataset Regression{} d@Regression{} = Just d
matchDataset _ _ = Nothing

{- | Replace the current run: cancel the old worker, build a fresh bounded queue
and latest-summary slot, spawn the new worker, and store the new state.

The cancel-then-replace is what makes restart safe (the spec): the prior worker
stops pushing, a brand-new empty queue is installed so no stale events leak into
the new run, and the new worker starts clean. The bound on the new queue is the
same back-pressure contract; restart does not weaken it.
-}
launch :: TVar (Maybe RunState) -> RunRequest -> Dataset -> IO ()
launch runRef req dataset = do
    prior <- readTVarIO runRef
    forM_ prior (cancel . runWorker)
    queue <- newTBQueueIO (fromIntegral queueBound)
    latest <- newTVarIO Nothing
    gen <- createSystemRandom
    worker <- async (runWorkerLoop req dataset queue latest gen)
    atomically (writeTVar runRef (Just RunState{runQueue = queue, runLatest = latest, runWorker = worker}))

{- | The worker: drive the chosen method incrementally, pushing a 'Summary' and a
'Batch' per step and a terminal 'EvDone' at the end.

The run grows by 'batchStep' each iteration up to @min samples runCap@, so the
posterior is emitted as a converging sequence rather than one final answer. Each
step calls the verified interpreter for the chosen method on the model built from
the dataset, computes the running summary (mean and variance, plus the
method-specific diagnostic), thins a subsample for the histogram, and writes both
events to the bounded queue. 'writeTBQueue' blocks when the queue is full, which
is the back-pressure that keeps the worker from outrunning the drainer. The single
generator is threaded across every step, so each larger run draws fresh randomness
and the estimates genuinely converge rather than repeating.
-}
runWorkerLoop ::
    RunRequest ->
    Dataset ->
    TBQueue StreamEvent ->
    TVar (Maybe Summary) ->
    Gen RealWorld ->
    IO ()
runWorkerLoop req dataset queue latest gen = do
    let total = min (max 1 (runRequestSamples req)) runCap
        -- The batch boundaries: 500, 1000, ..., up to total, ending exactly at total.
        steps = batchSizes total
    forM_ (zip [0 ..] steps) $ \(idx, n) -> do
        (summ, draws) <- computeBatch (runRequestMethod req) dataset n idx gen
        -- Record the latest summary for reconnect resume, then enqueue both the
        -- summary and the thinned batch. The writes block if the client is behind.
        atomically (writeTVar latest (Just summ))
        atomically (writeTBQueue queue (EvSummary summ))
        atomically (writeTBQueue queue (EvBatch (Batch{batchBatchIndex = idx, batchDraws = draws})))
    atomically (writeTBQueue queue EvDone)

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
'Summary' (tagged with the batch index) plus a thinned list of raw draws.

For 'Importance': draw @n@ particles, take the weighted posterior mean and the
weighted second moment (variance @= E[x^2] - mean^2@), and the effective sample
size @1 / sum(normalizedWeight^2)@ from the log-weight column; acceptance rate is
'Nothing' (importance has no accept step). For 'MH': run an @n@-step chain, take
the plain mean and variance over the visited values and the measured acceptance
rate from 'mhChain'; ESS is 'Nothing' (the chain ESS is an autocorrelation-based
quantity not computed here). The subsample is an even thinning of the latent
values for the client histogram. Every statistic comes from the verified library
functions; this only arranges their output.
-}
computeBatch ::
    Method ->
    Dataset ->
    Int ->
    Int ->
    Gen RealWorld ->
    IO (Summary, [Double])
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
                summ =
                    Summary
                        { summaryBatchIndex = idx
                        , summaryMean = mean
                        , summaryVariance = var
                        , summaryAcceptanceRate = Nothing
                        , summaryEffectiveSampleSize = Just ess
                        }
            pure (summ, draws)
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
            pure (summ, draws)

{- | Build the 'Credence.Model' the run conditions on from its 'Dataset'.

The coin model uses a Beta(2,2) prior, the same weakly-informative prior the
conjugate gate runs on; the regression model uses the broad Normal(0,3) prior and
unit noise the Normal-Normal gate uses. These priors are fixed here rather than
sent on the wire: the demos are about visualizing convergence for a known model,
not about tuning priors from the client. The model is structure only; building it
draws nothing, and the chosen interpreter is what runs it.
-}
buildModel :: Dataset -> Model Double
buildModel (Coin flips) = coinModel 2 2 flips
buildModel (Regression pts) = linearRegression 0 3 1 pts

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

{- | Drain the current run's queue to the client as a @text/event-stream@.

Sets the SSE content type and @Cache-Control: no-cache@ so proxies and the browser
do not buffer or cache the stream, and the development CORS header (see
'corsHeader'). If a run is in progress, the latest 'Summary' is sent first so a
client connecting or reconnecting mid-run recovers the current state immediately
(edge case SSE drop, reconnect resume) rather than waiting for the next batch.
Then it loops 'readTBQueue', writing one SSE frame per event as @data: <json>\\n\\n@,
flushing after each so events arrive promptly, and stops when it reads 'EvDone'.
If no run has started, it sends a comment frame and returns, so a bare GET does not
hang.
-}
streamRun :: TVar (Maybe RunState) -> ActionM ()
streamRun runRef = do
    setHeader "Content-Type" "text/event-stream"
    setHeader "Cache-Control" "no-cache"
    corsHeader
    mState <- liftIO (readTVarIO runRef)
    case mState of
        Nothing ->
            -- No run yet: a single comment frame keeps the connection from hanging
            -- and tells a client to POST first. EventSource ignores comment lines.
            stream $ \send flush -> do
                send (lazyByteString ": no active run, post to /run/coin or /run/regression first\n\n")
                flush
        Just st ->
            stream $ \send flush -> do
                -- Reconnect resume: replay the latest summary if one exists.
                latest <- readTVarIO (runLatest st)
                forM_ latest $ \s -> do
                    send (sseFrame (EvSummary s))
                    flush
                drain (runQueue st) send flush

{- | The read-and-emit loop: pull events off the bounded queue and write each as
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
