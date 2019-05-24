module Demo.Wiretap where

import Pipes

import Patterns.Wiretap
import Demo.Utilities (demoProducer, demoConsumer)


wiretapDemo :: IO ()
wiretapDemo = runEffect $ demoProducer [] >->
                          wiretap (await >>= \m -> lift $ print m) >->
                          demoConsumer "Received:"