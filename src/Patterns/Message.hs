module Patterns.Message where

import Pipes.Concurrent

import Control.Exception
import Control.Monad (unless)
import System.Time.Extra (timeout, Seconds)


-- data Header a = Header {returnAddress :: Maybe (Output (Message a))}

data Header = Header {returnAddress :: Maybe String}

-- data Message a = Message {header :: Header a, body :: a}

data Message a = Message {header :: Header, body :: a}

instance (Show a) => Show (Message a) where
    show (Message _ body) = show body



data MessagingSystemException   = SendFailure String            | 
                                  ReceiveFailure                | 
                                  ReceivedRequestMessage String | 
                                  NoReturnAddressHeader String  |
                                  RequestReplyTimeout String
                                  deriving (Show)

instance Exception MessagingSystemException



emptyHeader = Header Nothing

createMessage :: a -> Message a
createMessage body = Message {body = body, header = emptyHeader}

-- setReturnAddress :: Output (Message a) -> Message a -> Message a
-- setReturnAddress add msg = let newHeader = (header msg) {returnAddress = Just add}
--                             in msg {header = newHeader}



sendMessage' :: (Show a) => Output (Message a) -> Message a -> IO ()
sendMessage' o msg = do
    isSent <- atomically $ send o msg
    unless isSent $ throw $ SendFailure $ show msg

sendMessage :: (Show a) => Output (Message a) -> a -> IO ()
sendMessage o msg = sendMessage' o $ createMessage msg

-- sendRequestMessage :: (Show a) => Output (Message a) -> Seconds -> a -> IO a
-- sendRequestMessage out secs msg = do
--     (o, i) <- spawn unbounded
--     sendMessage' out $ setReturnAddress o $ createMessage msg
--     reply <- timeout secs $ receiveMessage i
--     case reply of
--         Just a -> return a
--         Nothing -> throw $ RequestReplyTimeout $ show msg



receiveMessage' :: Input (Message a) -> IO (Message a)
receiveMessage' i = do
    message <- atomically $ recv i
    case message of
        Nothing  -> throw ReceiveFailure
        Just msg -> return msg

receiveMessage :: (Show a) => Input (Message a) -> IO a
receiveMessage i = do
    msg <- receiveMessage' i
    case returnAddress $ header msg of
            Nothing   -> return $ body msg
            otherwise -> throw $ ReceivedRequestMessage $ show msg
            
-- receiveRequestMessage :: (Show a) => Input (Message a) -> IO (Output (Message a), a)
-- receiveRequestMessage i = do
--     msg <- receiveMessage' i
--     case returnAddress $ header msg of
--             Just o    -> return (o, body msg)
--             Nothing   -> throw $ NoReturnAddressHeader $ show msg

