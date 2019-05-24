module Patterns.Router where

import Pipes
import Pipes.Concurrent

import Control.Concurrent.Async (async)
import Control.Monad (when)

import Patterns.Message


router1 :: Monad m => (a -> Bool) -> Pipe (Message a) (Message a) m r
router1 func  = do
    message <- await
    when (func $ body message) (yield message)
    router1 func
                           
                            
router2 :: (a -> Bool) -> Output (Message a) -> Pipe (Message a) (Message a) IO ()
router2 func output = do
    message <- await
    if func $ body message
        then do
            isSent <- lift $ atomically $ send output message
            when isSent $ router2 func output
        else yield message >> router2 func output



router :: (a -> Bool) -> Consumer (Message a) IO () -> Pipe (Message a) (Message a) IO ()
router func cons = do
    (o, i) <- lift $ spawn unbounded
    lift $ async $ runEffect $ fromInput i >-> cons
    router2 func o
