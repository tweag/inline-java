{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Exception (handle)
import Data.String (fromString)
import Control.Monad ((>=>))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Text.IO as Text
import Foreign.JNI (showException, withJVM)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Java.Inline
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)


main :: IO ()
main = do
    let -- We use the classpath provided at build time.
        jvmArgs = case $(do
            cps <- TH.runIO $ do
              Just cp <- lookupEnv "CLASSPATH"
              -- We canonicalize the paths because the jars
              -- are located under symbolic links that do not
              -- survive the compilation.
              mapM canonicalizePath $ splitOn ":" cp
            TH.lift (intercalate ":" cps)
          ) of
          cp -> [ "-Djava.class.path=" <> fromString cp ]
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
