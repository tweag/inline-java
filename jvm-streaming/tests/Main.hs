{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Java (withJVM)
import qualified Spec
import Test.Hspec

main :: IO ()
main =
    withJVM ["-Djava.class.path=../jvm-batching/build/libs/jvm-batching.jar"] $
      hspec Spec.spec
