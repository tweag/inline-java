{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Foreign.JNISpec where

import Control.Concurrent (runInBoundThread)
import Data.Singletons
import Foreign.JNI.Types
import Foreign.JNI.Unsafe
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
