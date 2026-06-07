{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The engine-to-browser wire format: aeson JSON types for batched samples and
running summaries.

This module is the boundary between the inference engine and the web visualizer.
It defines exactly what crosses the Server-Sent Events stream and what the client
POSTs to start a run, and nothing else: no inference, no sampler, no log-density.
The server builds these values from the output of the already-verified
interpreters ("Credence.Inference.Importance", "Credence.Inference.MH") and the
client decodes them; keeping the contract in one small module is what lets the
two sides evolve against a single source of truth.

== What the wire carries, and what it deliberately does not

A run produces far more samples than should be sent to a browser
(the edge-case notes, streaming volume), so the wire carries running /summaries/
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
contract 03-02's EventSource client decodes, so they are stable; changing one is
a wire break, not a refactor.
-}
module Credence.Wire (
    Method (..),
    Dataset (..),
    RunRequest (..),
    RunStarted (..),
    Summary (..),
    Batch (..),
    StreamEvent (..),
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
(the edge-case notes, streaming volume), so a large number cannot exhaust the
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
with that subsample. Keeping the draws a subsample is the wire-level half of the
streaming-volume rule (the edge-case notes): the engine may draw hundreds of
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
