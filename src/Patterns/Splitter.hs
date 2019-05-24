module Patterns.Splitter where

import Pipes

import Data.Foldable (traverse_)

import Patterns.Message


splitter :: (Monad m, Foldable t) => (a -> t b) -> Pipe (Message a) (Message b) m r
splitter func = do
    message <- await
    traverse_ (yield . (Message (header message))) (func $ body message)
    splitter func

