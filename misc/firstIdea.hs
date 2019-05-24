import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

data MessagingSystem = MessagingSystem { endpoints :: Set.Set String
                                       , connections :: Map.Map String (MessagingSystem -> MessagingSystem) 
                                       , messageQueues :: Map.Map String (Seq.Seq String)
                                       , message :: String }

newMessagingSystem :: MessagingSystem
newMessagingSystem = MessagingSystem Set.empty Map.empty Map.empty "" 

dequeue :: Seq.Seq a -> Maybe (a, Seq.Seq a)
dequeue (s Seq.:|> el) = Just (el, s)
dequeue _ = Nothing

-- TODO case when endpoint already exists
addEndpoint :: MessagingSystem -> String -> MessagingSystem
addEndpoint sys name = sys { endpoints = Set.insert name $ endpoints sys
                           , messageQueues = Map.insert name Seq.empty $ messageQueues sys } 

addConnection :: String -> (MessagingSystem -> MessagingSystem) -> MessagingSystem -> MessagingSystem
addConnection name conn sys = sys { connections = Map.insert name conn $ connections sys }

to :: String -> MessagingSystem -> MessagingSystem
to name sys = sys { messageQueues = Map.insert name ( (message sys) Seq.<| queue ) ( messageQueues sys ) }
    where queue = case Map.lookup name $ messageQueues sys of
                    Just a -> a
                    Nothing -> error "TODO add better error processing"

whenTo :: (String -> Bool) -> String -> (MessagingSystem -> MessagingSystem) -> MessagingSystem -> MessagingSystem
whenTo func endp continue  = if func endp then to endp
                             else continue

sendMessage :: String -> String -> MessagingSystem -> MessagingSystem
sendMessage endp mess sys = conn sys { message = mess }
    where conn = case Map.lookup endp $ connections sys of
                    Just a -> a
                    Nothing -> error "TODO add better error processing"

receiveMessage :: String -> MessagingSystem -> (MessagingSystem, Maybe String)
receiveMessage endp sys = (sys {messageQueues = Map.insert endp newQueue $ messageQueues sys}, mess)
        where (mess, newQueue) = case dequeue queue of
                Just (m, q) -> (Just m, q)
                Nothing -> (Nothing::Maybe String, queue)
              queue = case Map.lookup endp $ messageQueues sys of
                Just a -> a
                Nothing -> error "TODO add better error processing"

-- Example:
-- t = newMessagingSystem
-- t = addEndPoint t "a"
-- t = addEndPoint t "b"
-- t = addEndPoint t "c"
-- t = addEndPoint t "d"
-- cond1 = \s -> head s == 'a'
-- cond2 = \s -> head s == 'b'
-- t = addConnection "a" (whenTo cond1 "b" $ whenTo cond2 "c" $ to "d")
-- t = sendMessage "a" "asd" t
-- t = sendMessage "a" "csd" t
-- (t, m) = receiveMessage "b" t     m == Just "asd"
-- (t, m) = receiveMessage "c" t     m == Nothing
-- (t, m) = receiveMessage "d" t     m == Just "csd
