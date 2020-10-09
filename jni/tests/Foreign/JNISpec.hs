{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Foreign.JNISpec where

import Control.Concurrent (runInBoundThread)
import Data.Singletons
import Foreign.JNI.Types
import Foreign.JNI.Unsafe
import Foreign.JNI.Unsafe.Internal.Introspection
import Test.Hspec

spec :: Spec
spec = do
    describe "runInAttachedThread" $ do
      it "can run jni calls in another thread" $
        runInBoundThread $ runInAttachedThread $ do
          jclass <- findClass $
            referenceTypeName (sing :: Sing ('Class "java.lang.Long"))
          deleteLocalRef jclass

      it "is needed to run jni calls in another thread" $
        runInBoundThread $ do
          findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Long")))
            `shouldThrow` \ThreadNotAttached -> True

    around_ (runInBoundThread . runInAttachedThread) $
      describe "isInstanceOf" $ do
        it "identifies a class name as a String" $ do
          klong <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Long")))
          name <- callObjectMethod klong classGetNameMethod []
          kstring <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.String")))
          isInstanceOf name kstring `shouldReturn` True
        it "identifies a class name as an Object" $ do
          klong <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Long")))
          name <- callObjectMethod klong classGetNameMethod []
          kobject <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Object")))
          isInstanceOf name kobject `shouldReturn` True
        it "doesn't identify a class name as a Long" $ do
          klong <- findClass (referenceTypeName (sing :: Sing ('Class "java.lang.Long")))
          name <- callObjectMethod klong classGetNameMethod []
          isInstanceOf name klong `shouldReturn` False
