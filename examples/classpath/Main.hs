{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Exception (handle)
import Data.String (fromString)
import Control.Monad ((>=>))
import qualified Bazel.Runfiles as Runfiles
import qualified Data.Text.IO as Text
import Foreign.JNI (showException, withJVM)
import Language.Java.Inline


main :: IO ()
main = do
    r <- Runfiles.create
    let jarPath = Runfiles.rlocation r "io_tweag_inline_java/examples/classpath/jar_deploy.jar"
        jvmArgs = [ "-Djava.class.path=" <> fromString jarPath ]
    withJVM jvmArgs $ handle (showException >=> Text.putStrLn) [java| {
      org.apache.commons.collections4.OrderedMap map =
        new org.apache.commons.collections4.map.LinkedMap();
      map.put("FIVE", "5");
      map.put("SIX", "6");
      map.put("SEVEN", "7");
      System.out.println(map.firstKey());
      System.out.println(map.nextKey("FIVE"));
      System.out.println(map.nextKey("SIX"));
      }
   |]
