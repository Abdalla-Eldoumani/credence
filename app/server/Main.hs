{-# LANGUAGE OverloadedStrings #-}

{- | The inference engine's HTTP surface. stage 3 grows this into the
streaming server: a POST starts an inference run for a chosen model, method,
and dataset, and a GET opens a Server-Sent Events stream
(Content-Type text/event-stream) that pushes batched samples and running
summaries as the run converges. The inference loop runs on its own thread and
hands batches to the stream handler through a bounded queue so a fast sampler
cannot outrun a slow client. For now this is the health check that proves the
server builds and binds. WSL2 forwards localhost to the Windows host, so this
port is reachable from a host browser without extra configuration.
-}
module Main (main) where

import Web.Scotty (get, scotty, text)

enginePort :: Int
enginePort = 3001

main :: IO ()
main =
    scotty enginePort $
        get "/health" $
            text "ok"
