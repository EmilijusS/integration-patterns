module Demo.Router where

import Pipes
import Pipes.Concurrent

import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.Async (async, wait)

import Patterns.Router
import Demo.Utilities (demoProducer, demoConsumerLock, isFirstSymbol)


router1Demo :: IO ()
router1Demo = do
    lock <- newMVar ()
    (o1, i1, s1) <- spawn' unbounded
    (o2, i2, s2) <- spawn' unbounded
    (o3, i3, s3) <- spawn' unbounded

    a1 <- async $ runEffect $ demoProducer [s1, s2, s3] >-> 
                              toOutput (o1 <> o2 <> o3)

    a2 <- async $ runEffect $ fromInput i1 >->
                              router1 (isFirstSymbol 'a') >->
                              demoConsumerLock lock "a:"
    a3 <- async $ runEffect $ fromInput i2 >->
                              router1 (isFirstSymbol 'b') >->
                              demoConsumerLock lock "b:"
    a4 <- async $ runEffect $ fromInput i3 >->
                              router1 (\m -> not $ isFirstSymbol 'a' m || isFirstSymbol 'b' m) >->
                              demoConsumerLock lock "else:"
    
    mapM_ wait [a1, a2, a3, a4]


router2Demo :: IO ()
router2Demo = do
    lock <- newMVar ()
    (o1, i1, s1) <- spawn' unbounded
    (o2, i2, s2) <- spawn' unbounded

    a1 <- async $ runEffect $ demoProducer [s1, s2] >->
                              router2 (isFirstSymbol 'a') o1 >->
                              router2 (isFirstSymbol 'b') o2 >->
                              demoConsumerLock lock "else:"

    a2 <- async $ runEffect $ fromInput i1 >-> demoConsumerLock lock "a:"
    a3 <- async $ runEffect $ fromInput i2 >-> demoConsumerLock lock "b:"

    mapM_ wait [a1, a2, a3]


routerDemo :: IO ()
routerDemo = do
    lock <- newMVar ()

    runEffect $ demoProducer [] >-> 
                router (isFirstSymbol 'a') (demoConsumerLock lock "a:") >->
                router (isFirstSymbol 'b') (demoConsumerLock lock "b:") >->
                demoConsumerLock lock "else:"


