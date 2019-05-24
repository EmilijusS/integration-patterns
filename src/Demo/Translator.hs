module Demo.Translator where

import Pipes
import qualified Pipes.Prelude as P

import Patterns.Translator
import Demo.Utilities (demoProducer, demoConsumer)


translatorDemo :: IO ()
translatorDemo = runEffect $ demoProducer [] >->
                             translator (fmap (\x -> succ x)) >->
                             demoConsumer "Received:"
