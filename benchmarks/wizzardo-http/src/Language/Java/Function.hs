{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.Java.Function
  ( createBiFunction
  ) where

import Control.Exception (SomeException, catch)
import qualified Control.Monad
import qualified Control.Monad.IO.Class.Linear as Linear
import qualified Control.Monad.Linear.Builder as Linear
import Data.Int
import Data.Singletons
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Foreign.JNI as JNI
import Foreign.JNI.Safe
import qualified Foreign.JNI.Types as NonLinear
import Foreign.Ptr
import GHC.Stable
import Language.Java.Inline.Safe
import Language.Java.Safe
import Prelude
import Prelude.Linear (Unrestricted(..))
import System.IO.Unsafe (unsafePerformIO)


type JNIApplyFun
    = NonLinear.JNIEnv
    -> Ptr NonLinear.JObject
    -> StablePtrHandle
    -> Ptr NonLinear.JObject
    -> Ptr NonLinear.JObject
    -> IO (Ptr NonLinear.JObject)

newtype StablePtrHandle = StablePtrHandle Int64
  deriving Coercible

foreign import ccall "wrapper" wrapObjectFun
  :: JNIApplyFun -> IO (FunPtr JNIApplyFun)

-- Export only to get a FunPtr.
foreign export ccall "wizzardo_http_handler_freeIterator" freeIterator
  :: NonLinear.JNIEnv -> Ptr JObject -> StablePtrHandle -> IO ()
foreign import ccall "&wizzardo_http_handler_freeIterator" freeIteratorPtr
  :: FunPtr (NonLinear.JNIEnv -> Ptr JObject -> StablePtrHandle -> IO ())

freeIterator :: NonLinear.JNIEnv -> Ptr JObject -> StablePtrHandle -> IO ()
freeIterator _ _ (StablePtrHandle ptr) = do
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ptr
    handlePtr <- deRefStablePtr sptr
    freeHaskellFunPtr handlePtr
    freeStablePtr sptr

-- | Creates a BiFunction from a Haskell function.
--
-- The Haskell function must return jnull or a local reference.
--
-- TODO Maybe move this to a package to deal with function callbacks.
createBiFunction
  :: ( IsReferenceType a
     , IsReferenceType b
     , IsReferenceType c
     , SingI a
     , SingI b
     , SingI c
     , Linear.MonadIO m
     )
  => (NonLinear.J a -> NonLinear.J b -> IO (NonLinear.J c))
  -> m (J ('Class "java.util.function.BiFunction" <> [a, b, c]))
createBiFunction f =
    let Linear.Builder{..} = Linear.monadBuilder in do
    Unrestricted longFunctionPtr <- Linear.liftIOU (createStablePtrHandle f)
    jFunction <-
      [java| new java.util.function.BiFunction() {
          @Override
          public Object apply(Object t, Object u) {
            return hsApply($longFunctionPtr, t, u);
          }

          private native void hsFinalize(long functionPtr);
          private native Object hsApply(long functionPtr, Object t, Object u);

          @Override
          public void finalize() { hsFinalize($longFunctionPtr); }
        } |]
    (jFunction, Unrestricted klass) <- getObjectClass jFunction
    Linear.liftIO  (registerNativesForBiFunction klass)
    Linear.liftIO (JNI.deleteLocalRef klass)
    return (unsafeGeneric jFunction)

-- Keep this function at the top level to ensure that no callback-specific state
-- leaks into the functions to register as native methods for all the instances
-- of the inner class.
registerNativesForBiFunction :: NonLinear.JClass -> IO ()
registerNativesForBiFunction = do
    let {-# NOINLINE applyPtr #-}
        applyPtr :: FunPtr JNIApplyFun
        applyPtr = unsafePerformIO $ wrapObjectFun $ \_jenv _jthis h reqPtr respPtr ->
          withJNICallbackHandle h nullPtr $ \handleFun -> do
            NonLinear.unsafeObjectToPtr <$> Control.Monad.join
              ((handleFun
                :: NonLinear.JObject
                -> NonLinear.JObject
                -> IO NonLinear.JObject)
                <$> NonLinear.objectFromPtr reqPtr
                <*> NonLinear.objectFromPtr respPtr
              )
    registerNativesForCallback $ JNI.JNINativeMethod
          "hsApply"
          (methodSignature
            [ SomeSing (sing :: Sing ('Prim "long"))
            , SomeSing (sing :: Sing ('Class "java.lang.Object"))
            , SomeSing (sing :: Sing ('Class "java.lang.Object"))
            ]
            (sing :: Sing ('Class "java.lang.Object"))
          )
          applyPtr

withJNICallbackHandle :: StablePtrHandle -> a -> (f -> IO a) -> IO a
withJNICallbackHandle h valueOnException m =
    (withStablePtr h >>= m) `catch` \(e :: SomeException) ->
    fmap (const valueOnException) $ withLocalFrame_ $
    let Linear.Builder{..} = Linear.monadBuilder in do
    jmsg <- reflect (Text.pack $ show e)
    e <- [java| new RuntimeException($jmsg) |]
    throw_ (e :: J ('Class "java.lang.RuntimeException"))
  where
    withStablePtr :: StablePtrHandle -> IO a
    withStablePtr (StablePtrHandle h) = do
      let hPtr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral h
      deRefStablePtr hPtr

createStablePtrHandle :: a -> IO StablePtrHandle
createStablePtrHandle a =
    StablePtrHandle . fromIntegral . ptrToIntPtr . castStablePtrToPtr <$>
    newStablePtr a

registerNativesForCallback :: JNI.JNINativeMethod -> NonLinear.JClass -> IO ()
registerNativesForCallback jniNativeMethod klass = do
    JNI.registerNatives klass
      [ jniNativeMethod
      , JNI.JNINativeMethod
          "hsFinalize"
          (methodSignature [SomeSing (sing :: Sing ('Prim "long"))] (sing :: Sing 'Void))
          freeIteratorPtr
      ]
