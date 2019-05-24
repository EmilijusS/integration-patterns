{-# LANGUAGE TypeFamilies #-}
module Demo.AggMessage where

import Patterns.Aggregatable


newtype AggMessage = AggMessage { getAggMessage :: String }

instance Semigroup AggMessage where
    (<>) (AggMessage fst) (AggMessage snd) = AggMessage (fst ++ snd)

instance Aggregatable AggMessage where
    type KeyType AggMessage = Char
    getKey (AggMessage msg) = head msg
    isComplete (AggMessage msg) = length msg >= 5
    