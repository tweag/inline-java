{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent (runInBoundThread)
import Control.Exception (handle, throwIO)
import qualified Data.Text.IO as Text
import Foreign.JNI
import Language.Java.Inline (loadJavaWrappers)
import qualified Spec
import Test.Hspec
import System.IO (stderr)

main :: IO ()
main = withJVM ["-Djava.class.path=build/libs/jvm-batching.jar:jvm-batching/libjar.jar"] $ do
    loadJavaWrappers -- causes classes in jvm-batching.jar to be loaded
                     -- since they are used in java quotations
    hspec $ around_ (runInBoundThread . runInAttachedThread . printExceptions) Spec.spec
  where
    printExceptions = handle $ \e ->
      showException e >>= Text.hPutStrLn stderr >> throwIO e
