module Demo.Aggregator where

import Pipes
import qualified Pipes.Prelude as P

import Patterns.Aggregator
import Patterns.Translator
import Demo.AggMessage
import Demo.Utilities (demoProducer, demoConsumer)

aggregatorDemo :: IO ()
aggregatorDemo = runEffect $ demoProducer [] >->
                             aggregator head (++) (\x -> length x >= 5) >->
                             demoConsumer "Received:"


aggregator2Demo :: IO ()
aggregator2Demo = runEffect $ demoProducer [] >->
                             translator AggMessage >->
                             aggregator2 >->
                             translator getAggMessage >->
                             demoConsumer "Received:"

