{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Int (Int32)
import Foreign.JNI (withJVM)
import Language.Java.Inline
import Language.Java.Inline.Cabal (gradleHooks)

main :: IO Int32
main = withJVM [] $ [java| {
    org.apache.commons.collections4.OrderedMap map =
      new org.apache.commons.collections4.map.LinkedMap();
    map.put("FIVE", "5");
    map.put("SIX", "6");
    map.put("SEVEN", "7");
    System.out.println(map.firstKey());
    System.out.println(map.nextKey("FIVE"));
    System.out.println(map.nextKey("SIX"));
    return 0;
    }
 |]
