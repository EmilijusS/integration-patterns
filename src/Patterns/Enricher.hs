module Patterns.Enricher where

import Pipes

import Patterns.Message

enricher :: (a -> IO b) -> Pipe (Message a) (Message b) IO r
enricher func = do
    message <- await
    enriched <- lift $ func $ body message
    yield message {body = enriched}
    enricher func