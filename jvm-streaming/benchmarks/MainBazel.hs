module Main where

import qualified Bench
import Criterion.Main as Criterion
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.Java


main :: IO ()
main = do
    let classpath = "jvm-batching/libjar.jar"
    withJVM [Text.encodeUtf8 $ Text.pack $ "-Djava.class.path=" ++ classpath] $
      Criterion.defaultMain [Bench.benchBatching]
