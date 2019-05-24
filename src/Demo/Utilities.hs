module Demo.Utilities where

import Pipes 
import Pipes.Concurrent

import Control.Concurrent.MVar (MVar, withMVar)

import Patterns.Message


demoProducerStatic :: [STM ()] -> Producer (Message String) IO ()
demoProducerStatic seals = do 
    yield $ createMessage "aab"
    yield $ createMessage "baa"
    yield $ createMessage "caa"
    yield $ createMessage "ab"
    lift $ atomically $ sequence_ seals


demoProducer :: [STM ()] -> Producer (Message String) IO ()
demoProducer seals = do
    line <- lift getLine
    if line == "" 
        then lift $ atomically $ sequence_ seals
    else do
        yield $ createMessage line
        demoProducer seals


demoConsumer :: String -> Consumer (Message String) IO ()
demoConsumer prefix = do
    message <- await
    lift $ putStrLn $ prefix ++ " " ++ show (body message)
    demoConsumer prefix


demoConsumerLock :: MVar () -> String -> Consumer (Message String) IO ()
demoConsumerLock lock prefix = do
    message <- await
    lift $ printWithLock prefix lock $ body message
    demoConsumerLock lock prefix


printWithLock :: String -> MVar () -> String -> IO ()
printWithLock prefix lock message = withMVar lock $ \_ -> putStrLn $ prefix ++ " " ++ show message


isFirstSymbol :: Char -> String -> Bool
isFirstSymbol c (x:xs) = x == c
isFirstSymbol _ _      = False

