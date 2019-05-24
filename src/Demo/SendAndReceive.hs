module Demo.SendAndReceive where

import Pipes
import Pipes.Concurrent
import Control.Monad (unless)
import Control.Concurrent.Async (async, wait)

import Patterns.Message


sendAndReceive :: IO ()
sendAndReceive = do
    (o, i) <- spawn unbounded
    isSent <- atomically $ send o "Test"
    unless isSent $ print "Failed to send"
    mbMsg <- atomically $ recv i
    case mbMsg of
        Just msg -> print $ "Received: " ++ msg
        Nothing -> print "Failed to receive"


sendAndReceive2 :: IO ()
sendAndReceive2 = do
    (o1, i1) <- spawn unbounded
    (o2, i2) <- spawn unbounded
    async $ runEffect $ fromInput i1 >-> toOutput o2
    sendMessage o1 "Test"
    msg <- receiveMessage i2
    print msg 


-- sendAndReceive3 :: IO ()
-- sendAndReceive3 = do
--     (o1, i1) <- spawn unbounded
--     (o2, i2) <- spawn unbounded
--     async $ runEffect $ fromInput i1 >-> toOutput o2
--     asyncAction <- async $ sendRequestMessage o1 1 "Request"
--     (replyO, req) <- receiveRequestMessage i2
--     print req 
--     sendMessage replyO "Reply"
--     reply <- wait asyncAction
--     print reply
