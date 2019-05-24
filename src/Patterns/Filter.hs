module Patterns.Filter where

import Pipes    

import Prelude hiding (filter)
import Control.Monad (when)

import Patterns.Message


filter1 :: (Monad m) => (a -> Maybe a) -> Pipe (Message a) (Message a) m r
filter1 func = do
    message <- await
    case func $ body message of
        Just m  -> yield message {body = m} >> filter1 func
        Nothing -> filter1 func


filter :: Monad m => (a -> Bool) -> Pipe (Message a) (Message a) m r
filter func  = do
    message <- await
    when (func $ body message) (yield message)
    filter func
