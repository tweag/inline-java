{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.String (fromString)
import Foreign.JNI (withJVM)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Java.Inline
import System.Environment (lookupEnv)

main :: IO ()
main = do
    let -- We use the classpath provided at build time.
        jvmArgs = case $(TH.lift =<< TH.runIO (lookupEnv "CLASSPATH")) of
          Just cp -> [ fromString ("-Djava.class.path=" ++ cp) ]
    withJVM jvmArgs [java| {
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
