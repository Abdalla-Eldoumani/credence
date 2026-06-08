{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The engine-to-browser wire format: aeson JSON types for batched samples and
running summaries.

This module is the boundary between the inference engine and the web visualizer.
It defines exactly what crosses the Server-Sent Events stream and what the client
POSTs to start a run: the JSON types, plus the pure validation
('validateRunRequest') that bounds an incoming request before any run is allocated.
It holds no inference, no sampler, and no log-density. The server builds the wire
values from the output of the already-verified interpreters
("Credence.Inference.Importance", "Credence.Inference.MH") and the client decodes
them; keeping the contract and its input bounds in one small module is what lets
the two sides evolve against a single source of truth.

== Bounding an untrusted request

A decoded request is still untrusted: @samples@ and the dataset array are
client-supplied and, unbounded, a single request could pin a worker or exhaust
memory. 'validateRunRequest' is the pure check that rejects an over-budget
@samples@ ('maxSamples'), an over-long dataset ('maxDatasetLength'), a non-positive
@samples@, a dataset whose constructor does not match the route, and a tracking run
with no observations, returning the normalized 'Dataset' only when every bound
holds. It lives here, beside the types, so it is unit-testable without a server.

== What the wire carries, and what it deliberately does not

A run produces far more samples than should be sent to a browser,
so to bound streaming volume the wire carries running /summaries/
plus a /subsample/ of draws, never every sample. 'Summary' is the converging
posterior in a few numbers; 'Batch' is a thinned handful of raw draws for a
histogram. No raw 'observe'-style log-weight is ever exposed: the log-space
weights stay inside the engine, and only finished summary statistics (already
normalized) reach the wire. Every number on the wire is a plain 'Double'.

== Field-name convention (stable, documented)

JSON field names are camelCase and match the Haskell record field names with the
record's type prefix dropped (so @summaryBatchIndex@ becomes @batchIndex@). Sum
types ('Method', 'Dataset', 'StreamEvent') are tagged: a @tag@ field names the
constructor and the remaining fields carry its contents. These names are the
contract the EventSource client decodes, so they are stable; changing one is
a wire break, not a refactor.
-}
module Credence.Wire (
    Method (..),
    Dataset (..),
    DatasetKind (..),
    RunRequest (..),
    RunStarted (..),
    Summary (..),
    Batch (..),
    StreamEvent (..),
    maxSamples,
    maxDatasetLength,
    datasetKind,
    datasetLength,
    RequestError (..),
    requestErrorMessage,
    validateRunRequest,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
 )
import Data.Aeson.Types (
    Options (..),
    SumEncoding (..),
    camelTo2,
 )
import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Drop a record-field prefix and lower the next initial, so @summaryBatchIndex@
encodes as @batchIndex@. The prefix is the part to strip (typically the type name
lower-cased), and what remains has its first character lowered to stay camelCase.
A field whose name does not start with the prefix is left as written, which keeps
the function total if a record is ever extended without the convention.
-}
dropPrefix :: String -> String -> String
dropPrefix prefix field =
    case drop (length prefix) field of
        (c : cs) | take (length prefix) field == prefix -> toLower c : cs
        _ -> field

{- | aeson options for a record type: strip the given field-name prefix and emit
camelCase. Shared by every record below so the convention lives in one place.
-}
recordOptions :: String -> Options
recordOptions prefix =
    defaultOptions{fieldLabelModifier = dropPrefix prefix}

{- | aeson options for a tagged sum: a @tag@ field names the constructor and the
contents sit beside it. @camelTo2@ lowercases the constructor name into the tag
(so @EvBatch@ is tagged @ev_batch@-style only if needed); here the constructors
are already the names the client matches on, so the tag is the constructor name
verbatim via the identity modifier, with record fields inside each constructor
de-prefixed the same way as the standalone records.
-}
taggedOptions :: Options
taggedOptions =
    defaultOptions
        { sumEncoding = TaggedObject{tagFieldName = "tag", contentsFieldName = "contents"}
        , constructorTagModifier = id
        , fieldLabelModifier = camelTo2 '_'
        }

{- | The inference method a run uses: likelihood-weighted importance sampling or
trace-based single-site Metropolis-Hastings. Encoded as a tagged sum with the
constructor name in a @tag@ field, e.g. @{"tag":"Importance"}@. The client sends
this inside a 'RunRequest'; the server dispatches on it to the matching verified
interpreter.
-}
data Method = Importance | MH
    deriving (Eq, Show, Generic)

instance ToJSON Method where
    toJSON = genericToJSON taggedOptions

instance FromJSON Method where
    parseJSON = genericParseJSON taggedOptions

{- | The data a run conditions on, one constructor per demo model. @Coin@ carries
the observed flips (@True@ for heads); @Regression@ carries the observed points as
@(x, y)@ pairs; @Localization@ carries the noisy sensor readings of a hidden source
position; @Tracking@ carries the noisy observation sequence of a moving target (one
scalar read per time step, fed to the particle filter); @Mixture@ carries the
synthetic data points to be clustered. Tagged so a single 'RunRequest' type serves
the @/run/coin@, @/run/regression@, @/run/localization@, @/run/tracking@, and
@/run/mixture@ routes: @{"tag":"Coin","flips":[true,false]}@,
@{"tag":"Regression","points":[[1,2.1]]}@, @{"tag":"Localization","readings":[1.1,0.9]}@,
@{"tag":"Tracking","observations":[1.0,1.2,0.9]}@, or
@{"tag":"Mixture","points":[-6.1,0.2,5.0]}@. The server validates the constructor
against the route and builds the corresponding model.
-}
data Dataset
    = Coin {datasetFlips :: [Bool]}
    | Regression {datasetPoints :: [(Double, Double)]}
    | Localization {datasetReadings :: [Double]}
    | Tracking {datasetObservations :: [Double]}
    | Mixture {datasetMixturePoints :: [Double]}
    deriving (Eq, Show, Generic)

instance ToJSON Dataset where
    toJSON = genericToJSON datasetOptions

instance FromJSON Dataset where
    parseJSON = genericParseJSON datasetOptions

{- | Options for 'Dataset': tagged sum with de-prefixed record fields, so the
@datasetFlips@/@datasetPoints@ accessors encode as @flips@/@points@.
-}
datasetOptions :: Options
datasetOptions =
    defaultOptions
        { sumEncoding = TaggedObject{tagFieldName = "tag", contentsFieldName = "contents"}
        , constructorTagModifier = id
        , fieldLabelModifier = dropPrefix "dataset"
        }

{- | The body the client POSTs to start a run: which 'Method', the 'Dataset' to
condition on, and a sample budget @samples@ (the number of importance particles
or MH steps the run drives toward). The fields are flat, so a request reads
@{"method":{"tag":"Importance"},"dataset":{"tag":"Coin","flips":[true,false]},"samples":2000}@.
The server caps @samples@ to a bounded run length regardless of the value sent
(bounding streaming volume), so a large number cannot exhaust the
engine.
-}
data RunRequest = RunRequest
    { runRequestMethod :: Method
    , runRequestDataset :: Dataset
    , runRequestSamples :: Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON RunRequest where
    toJSON = genericToJSON (recordOptions "runRequest")

instance FromJSON RunRequest where
    parseJSON = genericParseJSON (recordOptions "runRequest")

{- | The response a run POST returns: the 'RunId' the server allocated for the run
that just started. The client streams this run by opening @\/stream\/\<runId\>@.
Encodes as @{"runId":"3"}@. The RunId is an opaque text identifier (the server
renders it from a monotonic counter); the client treats it as a stable handle and
does not parse it. This is the handle that makes concurrent runs addressable: each
POST returns a distinct RunId, so two runs in flight stream independently.
-}
newtype RunStarted = RunStarted
    { runStartedRunId :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON RunStarted where
    toJSON = genericToJSON (recordOptions "runStarted")

instance FromJSON RunStarted where
    parseJSON = genericParseJSON (recordOptions "runStarted")

{- | A running summary of the posterior as it converges: the batch index it was
computed at, the posterior mean and variance, and two method-specific diagnostics
that are 'Nothing' when the method does not produce them. @acceptanceRate@ is the
MH accept/reject fraction (@Nothing@ for importance sampling, which has no
accept step); @effectiveSampleSize@ is the importance-weight ESS,
@1 / sum(normalizedWeight_i^2)@ (@Nothing@ for MH, whose ESS is a different,
autocorrelation-based quantity not computed here). Encodes as
@{"batchIndex":3,"mean":0.64,"variance":0.015,"acceptanceRate":null,"effectiveSampleSize":1880.2}@.
-}
data Summary = Summary
    { summaryBatchIndex :: Int
    , summaryMean :: Double
    , summaryVariance :: Double
    , summaryAcceptanceRate :: Maybe Double
    , summaryEffectiveSampleSize :: Maybe Double
    }
    deriving (Eq, Show, Generic)

instance ToJSON Summary where
    toJSON = genericToJSON (recordOptions "summary")

instance FromJSON Summary where
    parseJSON = genericParseJSON (recordOptions "summary")

{- | A thinned batch of raw draws for the client's histogram: the batch index, a
/subsample/ of the latent values, and optional per-draw normalized weights aligned
with that subsample. Keeping the draws a subsample is the wire-level half of
bounding streaming volume: the engine may draw hundreds of
thousands of values, but only a bounded handful per batch reach the browser.

@batchWeights@ is @Just ws@ for importance sampling, where @ws !! i@ is the
NORMALIZED self-normalized weight of @draws !! i@ (the two lists are the same length
and the same order, both thinned by the same stride), so the cloud can scale each
particle's opacity by its weight. It is @Nothing@ for Metropolis-Hastings, whose
chain has no per-draw importance weight. This stays inside the no-raw-log-weight
rule: these are finished, normalized summary weights, never the raw 'observe'
log-weights, which never leave the engine. The field is additive, so a coin or
regression batch (which sets it 'Nothing') still round-trips. Under aeson defaults
@Nothing@ encodes as @null@ and @Just xs@ as the array, so an importance batch reads
@{"batchIndex":3,"draws":[0.61,0.66],"weights":[0.5,0.5]}@ and an MH batch reads
@{"batchIndex":3,"draws":[0.61,0.66],"weights":null}@.
-}
data Batch = Batch
    { batchBatchIndex :: Int
    , batchDraws :: [Double]
    , batchWeights :: Maybe [Double]
    }
    deriving (Eq, Show, Generic)

instance ToJSON Batch where
    toJSON = genericToJSON (recordOptions "batch")

instance FromJSON Batch where
    parseJSON = genericParseJSON (recordOptions "batch")

{- | The tagged envelope each @data:@ line on the stream carries: a 'Batch' of
draws, a 'Summary' of the posterior, or a terminal 'EvDone' marking the run
complete. The client reads one event per SSE frame and dispatches on the @tag@:
@{"tag":"EvBatch","contents":{...}}@, @{"tag":"EvSummary","contents":{...}}@, or
@{"tag":"EvDone"}@. 'EvDone' is what tells the client to stop the stream loop and
the server's drainer to close the connection.
-}
data StreamEvent
    = EvBatch Batch
    | EvSummary Summary
    | EvDone
    deriving (Eq, Show, Generic)

instance ToJSON StreamEvent where
    toJSON = genericToJSON taggedOptions

instance FromJSON StreamEvent where
    parseJSON = genericParseJSON taggedOptions

{- | The largest @samples@ budget a run is allowed to request.

A run is driven incrementally and the engine already caps the run length it will
actually compute, but a request asking for an enormous budget is rejected here at
the boundary rather than silently clamped, so the client gets a clear error instead
of a run that quietly does less than it asked. The value is generous for the demos
(far more than any of them needs to converge) while still finite, so a single
request cannot ask the engine to draw an unbounded number of particles or steps.
The engine binds loopback only, so the concern is a local client (or a bug in one)
pinning a worker, not a remote attacker; a finite ceiling is the defense.
-}
maxSamples :: Int
maxSamples = 500000

{- | The largest dataset length a run is allowed to condition on.

Every batch rebuilds the model over the whole dataset (and for the filter and the
collapsed mixture, runs the population over every point), so the per-batch cost is
linear in the dataset length. An unbounded dataset is the clearest way a single
parseable request could exhaust memory or pin a worker, so the length is capped
here. A few thousand points is far past what the demos use (their fixtures are
tens of points) while keeping the per-batch work bounded.
-}
maxDatasetLength :: Int
maxDatasetLength = 4096

{- | The five dataset constructors named as a plain enum, so a route can say which
constructor it serves and the request validator can compare against it without
carrying a witness value. Each route fixes exactly one 'DatasetKind'.
-}
data DatasetKind
    = CoinKind
    | RegressionKind
    | LocalizationKind
    | TrackingKind
    | MixtureKind
    deriving (Eq, Show)

-- | The 'DatasetKind' of a decoded 'Dataset', for matching a request to its route.
datasetKind :: Dataset -> DatasetKind
datasetKind Coin{} = CoinKind
datasetKind Regression{} = RegressionKind
datasetKind Localization{} = LocalizationKind
datasetKind Tracking{} = TrackingKind
datasetKind Mixture{} = MixtureKind

-- | The number of observations a 'Dataset' carries, regardless of constructor.
datasetLength :: Dataset -> Int
datasetLength (Coin flips) = length flips
datasetLength (Regression pts) = length pts
datasetLength (Localization readings) = length readings
datasetLength (Tracking observations) = length observations
datasetLength (Mixture points) = length points

{- | Why a 'RunRequest' was refused, each carrying the offending value so the 400
message can name it. These are the only ways a syntactically valid (aeson-decoded)
request can still be rejected before a run is allocated.
-}
data RequestError
    = {- | The request's dataset constructor is not the one this route serves
      (expected kind, then the kind actually sent).
      -}
      DatasetRouteMismatch DatasetKind DatasetKind
    | -- | @samples@ was zero or negative; a run must draw at least one.
      NonPositiveSamples Int
    | -- | @samples@ exceeded 'maxSamples' (the value sent).
      SamplesTooLarge Int
    | -- | The dataset length exceeded 'maxDatasetLength' (the length sent).
      DatasetTooLong Int
    | {- | A tracking run was given no observations; the filter has no step to take
      and summarizing an empty run is undefined, so it is rejected up front.
      -}
      EmptyTrackingObservations
    deriving (Eq, Show)

-- | A human-readable, client-facing message for a 'RequestError', for the 400 body.
requestErrorMessage :: RequestError -> String
requestErrorMessage (DatasetRouteMismatch expected got) =
    "dataset does not match this route: expected "
        <> show expected
        <> " but got "
        <> show got
requestErrorMessage (NonPositiveSamples n) =
    "samples must be a positive integer, got " <> show n
requestErrorMessage (SamplesTooLarge n) =
    "samples " <> show n <> " exceeds the maximum of " <> show maxSamples
requestErrorMessage (DatasetTooLong len) =
    "dataset length " <> show len <> " exceeds the maximum of " <> show maxDatasetLength
requestErrorMessage EmptyTrackingObservations =
    "tracking requires at least one observation"

{- | Validate a decoded 'RunRequest' against the route that received it, returning
the normalized 'Dataset' on success or the first 'RequestError' on failure.

This is the pure core of the server's input hardening, factored out of the request
handler so it can be unit-tested without a running server. The checks, in order:

1. the dataset constructor matches the route's 'DatasetKind' (so @\/run\/coin@
   cannot be driven with regression points, the same defense the JSON decode gives
   against a malformed body, now extended to the constructor);
2. @samples@ is strictly positive (a run must draw at least one particle or step);
3. @samples@ does not exceed 'maxSamples' (so one request cannot ask for an
   unbounded computation);
4. the dataset length does not exceed 'maxDatasetLength' (so one request cannot
   make each batch's linear pass over the data unbounded); and
5. a tracking request carries at least one observation (the filter summarizes its
   last step, which does not exist for an empty sequence).

The order is deliberate: the route mismatch is reported first because it is the
coarsest error, then the cheap scalar @samples@ checks, then the dataset checks.
Only the first failure is returned, which is all a client needs to correct one
request. On success the caller has a 'Dataset' it can build a model from, knowing
every bound has been honored.
-}
validateRunRequest :: DatasetKind -> RunRequest -> Either RequestError Dataset
validateRunRequest expected req
    | actual /= expected = Left (DatasetRouteMismatch expected actual)
    | n <= 0 = Left (NonPositiveSamples n)
    | n > maxSamples = Left (SamplesTooLarge n)
    | datasetLength dataset > maxDatasetLength = Left (DatasetTooLong (datasetLength dataset))
    | expected == TrackingKind && datasetLength dataset == 0 = Left EmptyTrackingObservations
    | otherwise = Right dataset
  where
    dataset = runRequestDataset req
    actual = datasetKind dataset
    n = runRequestSamples req
