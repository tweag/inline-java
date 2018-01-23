-- | Expose Java iterators as streams from the
-- <http://hackage.haskell.org/package/streaming streaming> package.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
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
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Singletons (SomeSing(..))
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, intPtrToPtr, ptrToIntPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.Types as JNI
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

imports "java.util.Iterator"

type JNIFun a = JNIEnv -> Ptr JObject -> IO a

foreign import ccall "wrapper" wrapObjectFun
  :: JNIFun (Ptr (J ty)) -> IO (FunPtr (JNIFun (Ptr (J ty))))

-- Export only to get a FunPtr.
foreign export ccall "jvm_streaming_freeIterator" freeIterator
  :: JNIEnv -> Ptr JObject -> Int64 -> IO ()
foreign import ccall "&jvm_streaming_freeIterator" freeIteratorPtr
  :: FunPtr (JNIEnv -> Ptr JObject -> Int64 -> IO ())

data FunPtrTable = forall ty. FunPtrTable
  { nextPtr :: FunPtr (JNIFun (Ptr (J ty)))
  }

freeIterator :: JNIEnv -> Ptr JObject -> Int64 -> IO ()
freeIterator _ _ ptr = do
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ptr
    FunPtrTable{..} <- deRefStablePtr sptr
    freeHaskellFunPtr nextPtr
    freeStablePtr sptr

newIterator
  :: forall a. Reflect a
  => Stream (Of a) IO ()
  -> IO (J ('Iface "java.util.Iterator"))
newIterator stream0 = mdo
    ref <- newIORef stream0
    nextPtr <- wrapObjectFun $ \_ jthis ->
      -- Conversion is safe, because result is always a reflected object.
      unsafeForeignPtrToPtr <$> Coerce.coerce <$> popStream jthis
    -- Keep FunPtr's in a table that can be referenced from the Java side, so
    -- that they can be freed.
    tblPtr :: Int64 <- fromIntegral . ptrToIntPtr . castStablePtrToPtr <$> newStablePtr FunPtrTable{..}
    iterator <-
      [java| new Iterator() {
          /// A field that the Haskell side sets to true when it reaches the end.
          private boolean end = false;
          /// Lookahead element - it always points to a valid element unless
          /// end is true. There is no constructor, so in order to initialize
          // it, next() must be invoked once.
          private Object lookahead;
          @Override
          public boolean hasNext() { return !end; }

          @Override
          public Object next() {
            if (hasNext()) {
              final Object temp = lookahead;
              lookahead = hsNext();
              return temp;
            } else
              throw new java.util.NoSuchElementException();
          }

          @Override
          public void remove() { throw new UnsupportedOperationException(); }

          private native void hsFinalize(long tblPtr);

          private native Object hsNext();

          @Override
          public void finalize() { hsFinalize($tblPtr); }
        } |]
    klass <- JNI.getObjectClass iterator >>= JNI.newGlobalRef
    fieldEndId <- JNI.getFieldID klass "end"
                    (JNI.signature (sing :: Sing ('Prim "boolean")))
    let popStream :: Ptr JObject -> IO (J (Interp a))
        popStream ptrThis = do
          stream <- readIORef ref
          Streaming.uncons stream >>= \case
            Nothing -> do
              jthis <- JNI.objectFromPtr ptrThis
              -- When the stream ends, set the end field to True
              -- so the Iterator knows not to call hsNext again.
              JNI.setBooleanField jthis fieldEndId 1
              return jnull
            Just (x, stream') -> do
              writeIORef ref stream'
              reflect x
    JNI.registerNatives klass
      [ JNI.JNINativeMethod
          "hsNext"
          (methodSignature [] (sing :: Sing ('Class "java.lang.Object")))
          nextPtr
      , JNI.JNINativeMethod
          "hsFinalize"
          (methodSignature [SomeSing (sing :: Sing ('Prim "long"))] (sing :: Sing 'Void))
          freeIteratorPtr
      ]
    -- Call next once to initialize the iterator.
    () <- [java| { $iterator.next(); } |]
    return iterator

withStatic [d|
  instance Interpretation (Stream (Of a) m r) where
    type Interp (Stream (Of a) m r) = 'Iface "java.util.Iterator"

  instance Reify a => Reify (Stream (Of a) IO ()) where
    reify itLocal = do
      -- We make sure the iterator remains valid while we reference it.
      it <- JNI.newGlobalRef itLocal
      return $ Streaming.untilRight $ do
        call it "hasNext" [] >>= \case
          False -> JNI.deleteGlobalRef it >> return (Right ())
          True -> do
            obj <- [java| $it.next() |]
            Left <$> reify (unsafeCast (obj :: JObject) :: J (Interp a))

  instance Reflect a => Reflect (Stream (Of a) IO ()) where
    reflect x = newIterator x
  |]
