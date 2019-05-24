module Demo.Splitter where

import Pipes
import qualified Pipes.Prelude as P

import Patterns.Splitter
import Demo.Utilities (demoProducer, demoConsumer)


splitterDemo :: IO ()
splitterDemo = runEffect $ demoProducer [] >->
                           splitter (fmap (\x -> [x])) >->
                           demoConsumer "Received:"
-- splitterDemo = runEffect $ (lift $ getLine) >~ (splitter ~> (lift . putStrLn)) (fmap (\x -> [x])) 

