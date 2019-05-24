module Patterns.Wiretap where

import Pipes
import Pipes.Concurrent

import Control.Concurrent.Async (async)
import Control.Monad (when)

import Patterns.Message

wiretap :: Consumer (Message a) IO () -> Pipe (Message a) (Message a) IO ()
wiretap cons = do
    message <- await
    (o, i) <- lift $ spawn unbounded
    lift $ async $ runEffect $ fromInput i >-> cons
    yield message
    isSent <- lift $ atomically $ send o message
    when isSent $ wiretap cons
    