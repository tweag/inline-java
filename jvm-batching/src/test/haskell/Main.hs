{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent (runInBoundThread)
import Foreign.JNI
import Language.Java.Inline (loadJavaWrappers)
import qualified Spec
import Test.Hspec

main :: IO ()
main = withJVM ["-Djava.class.path=build/libs/jvm-batching.jar"] $ do
    loadJavaWrappers -- causes classes in jvm-batching.jar to be loaded
                     -- since they are used in java quotations
    hspec $ around_ (runInBoundThread . runInAttachedThread) Spec.spec
