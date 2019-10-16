{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main as Criterion
import Control.Monad (void)
import qualified Data.Coerce as Coerce
import Data.Int
import Data.Singletons (SomeSing(..))
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.Types as JNI
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
    env ini $ \ ~(Box (fun, obj, jstr, jstr2, jstr3, jstr4)) ->
      bgroup "Callbacks"
      [ bgroup "inline-java"
        [ bench "return-x100" $ withLocalFrame 1 $
            void @_ @JObject [java| {
                String s = null;
                for(int i=0;i<100;++i) {
                  s = ((java.util.function.BiFunction<String,String,String>) $fun).apply($jstr, $jstr2);          };
                return s.concat($jstr);
              } |]
        , bench "no-callback" $ nfIO $ do
            [java| { } |] :: IO ()
            return ()
        , bench "no-callback-4-args" $ withLocalFrame 1 $
            void @_ @JObject [java| { if ($jstr3 == $jstr4){}; return $jstr.concat($jstr2); } |]
        , bench "loadJavaWrappers" $ nfIO $ loadJavaWrappers
        ]
      , bgroup "jvm"
        [ bench "return" $ withLocalFrame 1 $
            void @_ @JObject $ call fun "apply" [coerce obj, coerce (JNI.upcast jstr)]
        , bench "no-callback" $ withLocalFrame 1 $
            void @_ @JString $ call jstr "concat" [coerce jstr2]
        ]
      ]
  where
    ini = do
      loadJavaWrappers
      Box <$> ((,,,,,)
                <$> newCallbackFunction (const . return)
                <*> ([java| new Object() |] :: IO JObject)
                <*> ([java| "Hello, " |] :: IO JString)
                <*> ([java| "World!" |] :: IO JString)
                <*> ([java| "Hello again, " |] :: IO JString)
                <*> ([java| "The World." |] :: IO JString)
              )
    withLocalFrame refsPerRun action =
      perBatchEnvWithCleanup
        (JNI.pushLocalFrame . (refsPerRun*) . fromIntegral)
        (\_ _ -> void (JNI.popLocalFrame jnull)) $
        \() -> action

type JNIFun = JNIEnv -> Ptr JObject -> Ptr JObject -> Ptr JObject -> IO (Ptr JObject)

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
  :: (JObject -> JObject -> IO JObject)
  -> IO (J ('Iface "java.util.function.BiFunction"))
newCallbackFunction f = do
    callbackPtr <- wrapObjectFun $ \_ _jthis jarg1 jarg2 ->
      unsafeForeignPtrToPtr <$> Coerce.coerce <$> do
        a1 <- JNI.objectFromPtr jarg1
        a2 <- JNI.objectFromPtr jarg2
        f a1 a2
    -- Keep FunPtr's in a table that can be referenced from the Java side, so
    -- that they can be freed.
    tblPtr :: Int64 <- fromIntegral . ptrToIntPtr . castStablePtrToPtr <$> newStablePtr FunPtrTable{..}
    fun <-
      [java| new java.util.function.BiFunction() {
          @Override
          public native Object apply(Object o1, Object o2);

          private native void hsFinalize(long tblPtr);

          @Override
          public void finalize() { hsFinalize($tblPtr); }
        } |]
    klass <- JNI.getObjectClass fun >>= JNI.newGlobalRef
    JNI.registerNatives klass
      [ JNI.JNINativeMethod
          "apply"
          (methodSignature
            [ SomeSing (sing :: Sing ('Class "java.lang.Object"))
            , SomeSing (sing :: Sing ('Class "java.lang.Object"))
            ]
            (sing :: Sing ('Class "java.lang.Object")))
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
