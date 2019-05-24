module Patterns.Aggregator where

import Pipes

import Data.Map.Strict

import Patterns.Aggregatable
import Patterns.Message


aggregator :: (Ord b, Monad m) => (a -> b) -> (a -> a -> a) -> (a -> Bool) -> Pipe (Message a) (Message a) m r
aggregator getKey join isComplete = aggregator' empty
    where aggregator' map = do
            message <- await
            let content = body message 
            let key = getKey content
            let newMap = insertWith join key content map
            let current = newMap ! key
            if isComplete current
            then do
                yield message {body = current}
                aggregator' (delete key newMap)
            else
                aggregator' newMap


aggregator2 :: (Aggregatable a, Monad m) => Pipe (Message a) (Message a) m r
aggregator2 = aggregator2' empty
    where aggregator2' map = do
            message <- await
            let content = body message 
            let key = getKey content
            let newMap = insertWith (<>) key content map
            let current = newMap ! key
            if isComplete current
            then do
                yield message {body = current}
                aggregator2' (delete key newMap)
            else
                aggregator2' newMap

