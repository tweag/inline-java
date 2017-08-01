-- | Expose Java iterators as streams from the
-- <http://hackage.haskell.org/package/streaming streaming> package.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

module Language.Java.Streaming () where

import Control.Distributed.Closure.TH
import qualified Data.Coerce as Coerce
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Singletons (SomeSing(..))
import Data.Word (Word8)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, intPtrToPtr, ptrToIntPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import qualified Foreign.JNI as JNI
import Foreign.JNI.Types (jnull)
import GHC.Stable
  ( castPtrToStablePtr
  , castStablePtrToPtr
  , deRefStablePtr
  , freeStablePtr
  , newStablePtr
  )
import Language.Java
import Language.Java.Inline
import Streaming (Stream, Of)
import qualified Streaming.Prelude as Streaming

isPoppableStream :: IORef (Stream (Of a) IO ()) -> IO Word8
isPoppableStream ref = do
    readIORef ref >>= Streaming.uncons >>= \case
      Nothing -> return $ fromIntegral $ fromEnum False
      Just (x, stream) -> do
        writeIORef ref (Streaming.cons x stream)
        return $ fromIntegral $ fromEnum True

popStream :: Reflect a ty => IORef (Stream (Of a) IO ()) -> IO (J ty)
popStream ref = do
    stream <- readIORef ref
    Streaming.uncons stream >>= \case
      Nothing -> do
        exc :: J ('Class "java.util.NoSuchElementException") <- new []
        JNI.throw exc
        return jnull
      Just (x, stream') -> do
        writeIORef ref stream'
        reflect x

type JNIFun a = JNIEnv -> Ptr JObject -> IO a

foreign import ccall "wrapper" wrapObjectFun
  :: JNIFun (Ptr (J ty)) -> IO (FunPtr (JNIFun (Ptr (J ty))))
foreign import ccall "wrapper" wrapBooleanFun
  :: JNIFun Word8 -> IO (FunPtr (JNIFun Word8))

-- Export only to get a FunPtr.
foreign export ccall "jvm_streaming_freeIterator" freeIterator
  :: JNIEnv -> Ptr JObject -> Int64 -> IO ()
foreign import ccall "&jvm_streaming_freeIterator" freeIteratorPtr
  :: FunPtr (JNIEnv -> Ptr JObject -> Int64 -> IO ())

data FunPtrTable = forall ty. FunPtrTable
  { hasNextPtr :: FunPtr (JNIFun Word8)
  , nextPtr :: FunPtr (JNIFun (Ptr (J ty)))
  }

freeIterator :: JNIEnv -> Ptr JObject -> Int64 -> IO ()
freeIterator _ _ ptr = do
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ptr
    FunPtrTable{..} <- deRefStablePtr sptr
    freeHaskellFunPtr hasNextPtr
    freeHaskellFunPtr nextPtr
    freeStablePtr sptr

newIterator
  :: Reflect a ty
  => Stream (Of a) IO ()
  -> IO (J ('Iface "java.util.Iterator"))
newIterator stream = do
    ref <- newIORef stream
    hasNextPtr <- wrapBooleanFun $ \_ _ -> isPoppableStream ref
    nextPtr <- wrapObjectFun $ \_ _ ->
      -- Conversion is safe, because result is always a reflected object.
      unsafeForeignPtrToPtr <$> Coerce.coerce <$> popStream ref
    -- Keep FunPtr's in a table that can be referenced from the Java side, so
    -- that they can be freed.
    tblPtr :: Int64 <- fromIntegral . ptrToIntPtr . castStablePtrToPtr <$> newStablePtr FunPtrTable{..}
    iterator <-
      [java| new java.util.Iterator() {
                @Override
                public native boolean hasNext();

                @Override
                public native Object next();

                @Override
                public void remove() {
                    throw new UnsupportedOperationException();
                }

                private native void hsFinalize(long tblPtr);

                @Override
                public void finalize() {
                    hsFinalize($tblPtr);
                }
             } |]
    klass <- JNI.getObjectClass iterator
    JNI.registerNatives klass
      [ JNI.JNINativeMethod
          "hasNext"
          (methodSignature [] (sing :: Sing ('Prim "boolean")))
          hasNextPtr
      , JNI.JNINativeMethod
          "next"
          (methodSignature [] (sing :: Sing ('Class "java.lang.Object")))
          nextPtr
      , JNI.JNINativeMethod
          "hsFinalize"
          (methodSignature [SomeSing (sing :: Sing ('Prim "long"))] (sing :: Sing 'Void))
          freeIteratorPtr
      ]
    return iterator

withStatic [d|
  type instance Interp (Stream (Of a) m r) = 'Iface "java.util.Iterator"

  instance Reify a ty => Reify (Stream (Of a) IO ()) ('Iface "java.util.Iterator") where
    reify itLocal = do
      -- We make sure the iterator remains valid while we reference it.
      it <- JNI.newGlobalRef itLocal
      return $ Streaming.untilRight $ do
        call it "hasNext" [] >>= \case
          False -> return (Right ())
          True -> do
            obj <- call it "next" []
            Left <$> reify (unsafeCast (obj :: JObject))

  instance Reflect a ty => Reflect (Stream (Of a) IO ()) ('Iface "java.util.Iterator") where
    reflect x = newIterator x
  |]
