{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (intersperse)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.JNI
import Language.Java.Inline (loadJavaWrappers)
import Language.Java.Streaming.Jars (getJars)
import qualified Spec
import Test.Hspec


main :: IO ()
main = do
    jars <- getJars
    let classpath = concat $ intersperse ":" jars
    withJVM [Text.encodeUtf8 $ Text.pack $ "-Djava.class.path=" ++ classpath] $ do
      loadJavaWrappers -- causes classes in jvm-batching.jar to be loaded
                       -- since they are used in java quotations
      hspec $ around_ (runInBoundThread . runInAttachedThread) Spec.spec
