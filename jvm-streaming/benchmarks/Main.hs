{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Bench
import Criterion.Main as Criterion
import Data.List (intersperse)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.Java
import Language.Java.Streaming.Jars (getJars)


main :: IO ()
main = do
    jars <- getJars
    let classpath = concat $ intersperse ":" jars
    withJVM [Text.encodeUtf8 $ Text.pack $ "-Djava.class.path=" ++ classpath] $
      Criterion.defaultMain [Bench.benchBatching]
