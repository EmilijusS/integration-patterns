{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Patterns.Aggregatable where


-- Semigroup for aggregation
-- getKey to determine who to aggregate with
-- isComplete to know when to send further
class (Semigroup a, Ord (KeyType a)) => Aggregatable a where
    type KeyType a :: *
    getKey :: a -> KeyType a
    isComplete :: a -> Bool
    