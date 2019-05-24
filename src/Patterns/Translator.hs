module Patterns.Translator where

import Pipes

import Patterns.Message

translator :: (Monad m) => (a -> b) -> Pipe (Message a) (Message b) m r
translator func = do
    message <- await
    yield message {body = (func $ body message)}
    translator func
