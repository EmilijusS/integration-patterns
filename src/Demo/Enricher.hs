module Demo.Enricher where

import Pipes

import Patterns.Enricher
import Demo.Utilities (demoProducer, demoConsumer)


enricherDemo :: IO ()
enricherDemo = runEffect $ demoProducer [] >->
                           enricher (\_ -> getLine) >->
                           demoConsumer "Received:"