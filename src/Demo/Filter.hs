module Demo.Filter where

import Pipes
import qualified Pipes.Prelude as P

import Prelude hiding (filter)

import Patterns.Filter
import Demo.Utilities (demoProducer, demoConsumer, isFirstSymbol)


filterDemo :: IO ()
filterDemo = runEffect $ demoProducer [] >->
                         filter1 (\m -> if isFirstSymbol 'a' m then Nothing else Just m ) >->
                         demoConsumer "Received:"
