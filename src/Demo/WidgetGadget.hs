module Demo.WidgetGadget where

import Pipes
import Pipes.Concurrent

import System.Random (randomIO)
import Control.Monad (when)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (MVar, withMVar, newMVar)
import Data.Char (toLower)

import Patterns

type CustomerId = String
type Item = String 

data Order = Order { orderId :: Int
                   , customerId :: CustomerId
                   , items :: [Item]
                   , isValid :: Bool
                   , vldBill :: Bool
                   , vldInv :: Bool
                   } deriving (Show)


startIntegration :: IO ()
startIntegration = do
    lock <- newMVar ()
    (o1, i1) <- spawn unbounded
    (o2, i2) <- spawn unbounded
    (o3, i3) <- spawn unbounded
    (o4, i4) <- spawn unbounded

    async $ integration i1 o2 o3 o4
    async $ shipping i2 lock
    async $ billing i3 lock
    async $ errorCh i4 lock

    getOrders o1 lock
    where
        getOrders out lock = do
            order <- createOrder lock
            sendMessage out order
            getOrders out lock


integration ::  Input (Message Order) -> 
                Output (Message Order) -> 
                Output (Message Order) -> 
                Output (Message Order) -> 
                IO ()
integration input shipOut billOut errorOut = do
    (o1, i1) <- spawn unbounded
    (o2, i2) <- spawn unbounded
    (o3, i3) <- spawn unbounded
    (o4, i4) <- spawn unbounded

    async $ billingValidation i1 o3
    async $ inventoryValidation i2 o4

    let aggStrat = (\a b -> a {isValid = isValid a && isValid b
                              , vldBill = vldBill a || vldBill b
                              , vldInv = vldInv a || vldInv b})
    let complete = (\a -> vldBill a && vldInv a)

    async $ runEffect $ fromInput input >-> toOutput (o1 <> o2)
    runEffect $ fromInput (i3 <> i4) >->
                aggregator orderId aggStrat complete >->
                router isValid (toOutput (shipOut <> billOut)) >->
                toOutput errorOut


billingValidation :: Input (Message Order) -> Output (Message Order) -> IO ()
billingValidation inp out = do
    order <- receiveMessage inp
    let validatedOrder = order { isValid = length (customerId order) == 10
                               , vldBill = True }
    sendMessage out validatedOrder
    billingValidation inp out


inventoryValidation :: Input (Message Order) -> Output (Message Order) -> IO ()
inventoryValidation inp out = do
    order <- receiveMessage inp
    let lowercaseItems = [map toLower x | x <- items order]
    let validatedItems = map (\x -> x == "widget" || x == "gadget") lowercaseItems
    let validatedOrder = order { isValid = and validatedItems
                               , vldInv = True }
    sendMessage out validatedOrder
    inventoryValidation inp out


processOrder :: String -> Input (Message Order) -> MVar () -> IO ()
processOrder pref inp lock = do
    order <- receiveMessage inp
    withMVar lock $ \_ -> putStrLn $ pref ++ " " ++ show order
    processOrder pref inp lock 

billing :: Input (Message Order) -> MVar () -> IO ()
billing = processOrder "Billing received order:" 

shipping :: Input (Message Order) -> MVar () -> IO ()
shipping = processOrder "Shipping received order:" 

errorCh :: Input (Message Order) -> MVar () -> IO ()
errorCh = processOrder "Invalid order:" 


createOrder :: MVar () -> IO Order
createOrder lock = do
    withMVar lock $ \_ -> putStrLn "Enter Customer ID: "
    id <- getLine
    withMVar lock $ \_ -> putStrLn "Enter items to order (stop by entering a blank line): "
    items <- getItems
    orderNum <- randomIO
    return $ Order orderNum id items False False False
    where
        getItems = do
            item <- getLine
            if item == "" then return []
            else do
                items <- getItems
                return $ item : items
