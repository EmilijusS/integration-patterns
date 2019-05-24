module Demo.Composite where

import Pipes
import Pipes.Concurrent

import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.Async (async)

import Patterns.Aggregator
import Patterns.Router
import Patterns.Splitter
import Patterns.Translator
import Demo.AggMessage
import Demo.Utilities (demoConsumerLock, demoConsumer, demoProducer, isFirstSymbol)


-- splits every string message to letters
-- then routes 'a', 'b' and other letters to 3 different pipes
-- in those pipes aggregates equal letters until length = 5 and then prints them
compositeDemo1 :: IO ()
compositeDemo1 = do
    lock <- newMVar ()

    let agg = translator AggMessage >-> aggregator2 >-> translator getAggMessage
    let aConsumer = agg >-> demoConsumerLock lock "a:"
    let bConsumer = agg >-> demoConsumerLock lock "b:"
    let elseConsumer = agg >-> demoConsumerLock lock "else:"

    runEffect $ demoProducer [] >->
                splitter (fmap (\x -> [x])) >->
                router (isFirstSymbol 'a') aConsumer >->
                router (isFirstSymbol 'b') bConsumer >->
                elseConsumer


-- I wasn't sure, if mailbox inputs can be combined (not shown in examples)
compositeDemo2 :: IO ()
compositeDemo2 = do
    lock <- newMVar ()

    (o1, i1) <- spawn unbounded
    (o2, i2) <- spawn unbounded
    (o3, i3) <- spawn unbounded

    let agg = translator AggMessage >-> aggregator2 >-> translator getAggMessage
    let aConsumer = agg >-> toOutput o1
    let bConsumer = agg >-> toOutput o2
    let elseConsumer = agg >-> toOutput o3

    async $ runEffect $ fromInput (i1 <> i2 <> i3) >-> demoConsumer "all:"

    runEffect $ demoProducer [] >->
                splitter (fmap (\x -> [x])) >->
                router (isFirstSymbol 'a') aConsumer >->
                router (isFirstSymbol 'b') bConsumer >->
                elseConsumer

