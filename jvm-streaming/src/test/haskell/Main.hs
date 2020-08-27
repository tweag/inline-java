{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List (intersperse)
import Data.Singletons (SomeSing(..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.JNI
import Language.Java
import Language.Java.Streaming.Jars (getJars)
import qualified Spec
import Test.Hspec

main :: IO ()
main = do
    jars <- getJars
    let classpath = concat $ intersperse ":" jars
    withJVM [Text.encodeUtf8 $ Text.pack $ "-Djava.class.path=" ++ classpath] $ do
      thr <- callStatic "java.lang.Thread" "currentThread"
      localLoader <- call (thr :: J ('Class "java.lang.Thread")) "getContextClassLoader"
      loader <- newGlobalRef localLoader
      deleteLocalRef localLoader
      deleteLocalRef thr
      setGetClass loader

      hspec Spec.spec
  where
    -- We need to load classes using the context ClassLoader. Here we
    -- tell jvm to use it.
    setGetClass :: J ('Class "java.lang.ClassLoader") -> IO ()
    setGetClass loader = do
      localThreadClass <- findClass (referenceTypeName (sing @('Class "java.lang.Thread")))
      threadClass <- newGlobalRef localThreadClass
      deleteLocalRef localThreadClass
      currentThreadMethodID <- getStaticMethodID threadClass "currentThread" $
                     methodSignature [] (sing @('Class "java.lang.Thread"))
      setContextClassLoaderMethodID <- getMethodID threadClass "setContextClassLoader" $
                     methodSignature
                       [ SomeSing (sing @('Class "java.lang.ClassLoader"))
                       ]
                       (sing @('Void))

      setGetClassFunction $ \s -> do
        thr <- callStaticObjectMethod threadClass currentThreadMethodID []
        () <- callVoidMethod thr setContextClassLoaderMethodID [coerce loader]
        deleteLocalRef thr
        findClass (referenceTypeName s)
