-- PIPES

-- PRODUCERS

-- Producer sends messages (values)
-- a - type being sent
-- m - base monad
-- r - returned type
Producer a m r

-- sends given value
yield :: Monad m => a -> Producer a m ()

-- substitutes yields in producer with given body 
for :: Monad m => Producer a m r -> (a -> Effect m ()) -> Effect m r
for :: Monad m => Producer a m r -> (a -> Producer b m ()) -> Producer b m r

-- Effect is just a wrapper around action. They are runnable
Effect m r
runEffect :: Monad m => Effect m r -> m r

-- EXAMPLE:
--         +--------+-- A 'Producer' that yields 'String's
--         |        |
--         |        |      +-- Every monad transformer has a base monad.
--         |        |      |   This time the base monad is 'IO'.
--         |        |      |  
--         |        |      |  +-- Every monadic action has a return value.
--         |        |      |  |   This action returns '()' when finished
--         v        v      v  v
stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String'
        stdinLn              -- Loop

-- just prints every line. lift is needed to lift IO to Effect Monad
main = runEffect $ for stdinLn (lift . putStrLn)

-- Takes a list (actually a foldable) of values and makes a Producer yielding those values
each :: Monad m => [a] -> Producer a m ()

-- ~> is "into" operator
-- basically f must be function a -> Producer and g can be either b -> Producer or b -> Effect
-- And this operator combines them
(f ~> g) x = for (f x) g

-- CONSUMERS

-- Externally iterate over Producer. Returns Left if producer is done, or Right with next value and remaining Producer.
next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))

-- Consumer gets messages
-- a - message type
-- m - base monad
-- b - return type
Consumer a m b

-- To get a message
await :: Monad m => Consumer a m a

-- 'feed' operator (kind of an opposite to 'into' operator)
-- draw >~ consumer loops over consumer, substituting each await with draw
(>~) :: Monad m => Effect m b -> Consumer b m c -> Effect m c
(>~) :: Monad m => Consumer a m b -> Consumer b m c -> Consumer a m c

-- PIPES

-- Pipe - connects producer with consumer
-- It is pull based, control flow begins from consumer
-- Producer and Consumer share the return value, because whichever terminates first provides it
(>->) :: Monad m => Producer a m r -> Consumer a m r -> Effect m r
(>->) :: Monad m => Producer a m r -> Pipe   a b m r -> Producer b m r
(>->) :: Monad m => Pipe   a b m r -> Consumer b m r -> Consumer a m r
(>->) :: Monad m => Pipe   a b m r -> Pipe   b c m r -> Pipe   a c m r


type Effect             = Proxy X  () () X
type Producer         b = Proxy X  () () b
type Consumer    a      = Proxy () a  () X
type Pipe        a    b = Proxy () a  () b
