{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main as Criterion
import Control.Monad (replicateM, replicateM_, void)
import qualified Data.Coerce as Coerce
import Data.Int
import Data.IORef
import Data.Singletons (SomeSing(..))
import qualified Foreign.Concurrent as Concurrent
import qualified Foreign.ForeignPtr as ForeignPtr
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.Types as JNI
import Foreign.Marshal.Alloc (mallocBytes, finalizerFree, free)
import Foreign.Marshal.Array (callocArray, mallocArray)
import Foreign.Ptr
import GHC.Stable
import Language.Java
import Language.Java.Inline

newtype Box a = Box { unBox :: a }

-- Not much sense in deepseq'ing foreign pointers. But needed for call to 'env'
-- below.
instance NFData (Box a) where rnf (Box a) = seq a ()

benchCallbacks :: Benchmark
benchCallbacks =
    env ini $ \ ~(Box (fun, obj)) ->
      bgroup "Callbacks"
      [ bgroup "inline-java"
        [ bench "return" $ withLocalFrame 1 $ do
            _ <- [java| $fun.apply($obj) |] :: IO JObject
            return ()
        , bench "no-callback" $ withLocalFrame 1 $ do
            [java| $obj |] :: IO JObject
            return  ()
        , bench "loadJavaWrappers" $ nfIO $ loadJavaWrappers
        ]
      , bgroup "jvm"
        [ bench "return" $ withLocalFrame 1 $ do
            _ <- call fun "apply" [coerce obj] :: IO JObject
            return ()
        , bench "no-callback" $ withLocalFrame 1 $ do
            _ <- call obj "toString" [] :: IO JString
            return ()
        ]
      ]
  where
    ini = do
      loadJavaWrappers
      Box <$> ((,) <$> newCallbackFunction return
                   <*> ([java| new Object() |] :: IO JObject)
              )
    withLocalFrame refsPerRun action =
      perBatchEnvWithCleanup
        (JNI.pushLocalFrame . (refsPerRun*) . fromIntegral)
        (\_ _ -> void (JNI.popLocalFrame jnull)) $
        \() -> action

type JNIFun = JNIEnv -> Ptr JObject -> IO (Ptr JObject)

foreign import ccall "wrapper" wrapObjectFun
  :: JNIFun -> IO (FunPtr JNIFun)

-- Export only to get a FunPtr.
foreign export ccall "inline_java_callbacks_benchmark_freeIterator" freeIterator
  :: JNIEnv -> Ptr JObject -> Int64 -> IO ()
foreign import ccall "&inline_java_callbacks_benchmark_freeIterator" freeIteratorPtr
  :: FunPtr (JNIEnv -> Ptr JObject -> Int64 -> IO ())

data FunPtrTable = FunPtrTable
  { callbackPtr :: FunPtr JNIFun
  }

freeIterator :: JNIEnv -> Ptr JObject -> Int64 -> IO ()
freeIterator _ _ ptr = do
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ptr
    FunPtrTable{..} <- deRefStablePtr sptr
    freeHaskellFunPtr callbackPtr
    freeStablePtr sptr

-- | Reflects a stream with no batching.
newCallbackFunction
  :: (JObject -> IO JObject)
  -> IO (J ('Iface "java.util.function.Function"))
newCallbackFunction f = do
    callbackPtr <- wrapObjectFun $ \_ jthis ->
      unsafeForeignPtrToPtr <$> Coerce.coerce <$> (JNI.objectFromPtr jthis >>= f)
    -- Keep FunPtr's in a table that can be referenced from the Java side, so
    -- that they can be freed.
    tblPtr :: Int64 <- fromIntegral . ptrToIntPtr . castStablePtrToPtr <$> newStablePtr FunPtrTable{..}
    fun <-
      [java| new java.util.function.Function() {
          @Override
          public native Object apply(Object o);

          private native void hsFinalize(long tblPtr);

          @Override
          public void finalize() { hsFinalize($tblPtr); }
        } |]
    klass <- JNI.getObjectClass fun >>= JNI.newGlobalRef
    JNI.registerNatives klass
      [ JNI.JNINativeMethod
          "apply"
          (methodSignature [SomeSing (sing :: Sing ('Class "java.lang.Object"))] (sing :: Sing ('Class "java.lang.Object")))
          callbackPtr
      , JNI.JNINativeMethod
          "hsFinalize"
          (methodSignature [SomeSing (sing :: Sing ('Prim "long"))] (sing :: Sing 'Void))
          freeIteratorPtr
      ]
    return fun


main :: IO ()
main = withJVM [] $ do
    Criterion.defaultMain
      [ benchCallbacks ]
