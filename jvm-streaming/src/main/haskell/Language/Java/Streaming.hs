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
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Language.Java.Streaming
  ( reifyStreamWithBatching
  , reflectStreamWithBatching
  ) where

import Control.Distributed.Closure.TH
import Control.Monad.IO.Class (liftIO)
import qualified Data.Coerce as Coerce
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import Data.Proxy
import qualified Data.Vector as V
import Data.Singletons (SomeSing(..))
import Foreign.Ptr (FunPtr, Ptr, intPtrToPtr, ptrToIntPtr)
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
import Language.Java.Batching
import Language.Java.Inline
import Streaming (Bifunctor(first), Stream, Of)
import qualified Streaming as Streaming
import qualified Streaming.Prelude as Streaming
import System.IO.Unsafe (unsafePerformIO)

imports "io.tweag.jvm.batching.*"
imports "java.util.Iterator"

type JNIFun a = JNIEnv -> Ptr JObject -> Int64 -> IO a

foreign import ccall "wrapper" wrapObjectFun
  :: JNIFun (Ptr (J ty)) -> IO (FunPtr (JNIFun (Ptr (J ty))))

-- Export only to get a FunPtr.
foreign export ccall "jvm_streaming_freeIterator" freeIterator
  :: JNIEnv -> Ptr JObject -> Int64 -> IO ()
foreign import ccall "&jvm_streaming_freeIterator" freeIteratorPtr
  :: FunPtr (JNIEnv -> Ptr JObject -> Int64 -> IO ())

data FunPtrTable = FunPtrTable
  { refPtr :: Int64
  }

freeIterator :: JNIEnv -> Ptr JObject -> Int64 -> IO ()
freeIterator _ _ ptr = do
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ptr
    FunPtrTable{..} <- deRefStablePtr sptr
    freeStablePtr $ castPtrToStablePtr $ intPtrToPtr $ fromIntegral refPtr
    freeStablePtr sptr

-- | Reflects a stream with no batching.
newIterator
  :: forall ty. Stream (Of (J ty)) IO ()
  -> IO (J ('Iface "java.util.Iterator" <> '[ty]))
newIterator stream0 = do
    ioStreamRef <- newIORef stream0
    refPtr :: Int64 <- fromIntegral . ptrToIntPtr . castStablePtrToPtr <$>
      newStablePtr ioStreamRef
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
              lookahead = hsNext($refPtr);
              return temp;
            } else
              throw new java.util.NoSuchElementException();
          }

          @Override
          public void remove() { throw new UnsupportedOperationException(); }

          private native void hsFinalize(long tblPtr);

          private native Object hsNext(long refPtr);

          @Override
          public void finalize() { hsFinalize($tblPtr); }
        } |]
    runOnce $ do
      klass <- JNI.getObjectClass iterator
      registerNativesForIterator klass
       <* JNI.deleteLocalRef klass
    -- Call next once to initialize the iterator.
    () <- [java| { $iterator.next(); } |]
    return $ generic iterator
  where
    -- Given that we always register natives on the same class,
    -- there is no point in registering natives more than once.
    runOnce :: IO a -> IO a
    runOnce action = do
      let {-# NOINLINE ref #-}
          ref = unsafePerformIO $ newIORef Nothing
      readIORef ref >>= \case
        Nothing -> do
          a <- action
          writeIORef ref (Just a)
          return a
        Just a ->
          return a

-- | Registers functions for the native methods of the inner class created in
-- 'newIterator'.
--
-- We keep this helper as a top-level function to ensure that no state tied
-- to a particular iterator leaks in the registered functions. The methods
-- registered here affect all the instances of the inner class.
registerNativesForIterator :: JClass -> IO ()
registerNativesForIterator klass = do
    fieldEndId <- JNI.getFieldID klass "end"
                    (JNI.signature (sing :: Sing ('Prim "boolean")))
    nextPtr <- wrapObjectFun $ \_ jthis streamRef ->
      -- Conversion is safe, because result is always a reflected object.
      unsafeForeignPtrToPtr . Coerce.coerce <$>
        popStream fieldEndId jthis streamRef
    JNI.registerNatives klass
      [ JNI.JNINativeMethod
          "hsNext"
          (methodSignature
            [SomeSing (sing :: Sing ('Prim "long"))]
            (sing :: Sing ('Class "java.lang.Object"))
          )
          nextPtr
      , JNI.JNINativeMethod
          "hsFinalize"
          (methodSignature [SomeSing (sing :: Sing ('Prim "long"))] (sing :: Sing 'Void))
          freeIteratorPtr
      ]
  where
    popStream :: JFieldID -> Ptr JObject -> Int64 -> IO (J ty)
    popStream fieldEndId ptrThis streamRef = do
      let stableRef = castPtrToStablePtr $ intPtrToPtr $ fromIntegral streamRef
      ref <- deRefStablePtr stableRef
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
          return x

-- | Reifies streams from iterators in batches of the given size.
reifyStreamWithBatching
  :: forall a. BatchReify a
  => Int32  -- ^ The batch size
  -> J ('Iface "java.util.Iterator" <> '[Interp a])
  -> IO (Stream (Of a) IO ())
reifyStreamWithBatching batchSize jiterator0 = do
    let jiterator1 = unsafeUngeneric jiterator0
    jbatcher <- unsafeUngeneric <$> newBatchWriter (Proxy :: Proxy a)
    jiterator <- [java| new Iterator() {
        private final int batchSize = $batchSize;
        private final Iterator it = $jiterator1;
        private final BatchWriter batcher = $jbatcher;
        public int count = 0;

        @Override
        public boolean hasNext() { return it.hasNext(); }

        @Override
        public Object next() {
          int i = 0;
          batcher.start(batchSize);
          while (it.hasNext() && i < batchSize) {
            batcher.set(i, it.next());
            i++;
          }
          count = i;
          return batcher.getBatch();
        }

        @Override
        public void remove() {
          throw new UnsupportedOperationException();
        }
      } |]
      >>= JNI.newGlobalRef
      :: IO (J ('Iface "java.util.Iterator"))
    cls <- JNI.getObjectClass jiterator >>= JNI.newGlobalRef
    fieldId <- JNI.getFieldID cls "count"
                 (JNI.signature (sing :: Sing ('Prim "int")))

    let go :: Int        -- next element to return from the batch
           -> V.Vector a -- current batch of elements
           -> Stream (Of a) IO ()
        go i v =
          if V.length v == i then do
            hasNext <- liftIO [java| $jiterator.hasNext() |]
            if hasNext then do
              v' <- liftIO $
                [java| $jiterator.next() |] `withLocalRef` \jbatch ->
                  JNI.getIntField jiterator fieldId
                  >>= reifyBatch (unsafeCast (jbatch :: JObject) :: J (Batch a))
              go 0 v'
            else
              liftIO $ do
                JNI.deleteGlobalRef jiterator
                JNI.deleteGlobalRef cls
          else do
            Streaming.yield $ v V.! i
            go (i + 1) v

    return $ go 0 V.empty

-- | Reflects streams to iterators in batches of the given size.
reflectStreamWithBatching
  :: forall a. BatchReflect a
  => Int  -- ^ The batch size
  -> Stream (Of a) IO ()
  -> IO (J ('Iface "java.util.Iterator" <> '[Interp a]))
reflectStreamWithBatching batchSize s0 = do
    jiterator <- unsafeUngeneric <$>
      (reflectStream $ Streaming.mapsM
                        (\s -> first V.fromList <$> Streaming.toList s)
                     $ Streaming.chunksOf batchSize s0
      )
    jbatchReader <- unsafeUngeneric <$> newBatchReader (Proxy :: Proxy a)
    generic <$> [java| new Iterator() {
        private final Iterator it = $jiterator;
        private final BatchReader batchReader = $jbatchReader;
        private int count = 0;

        @Override
        public boolean hasNext() {
          return count < batchReader.getSize() || it.hasNext();
        }
        @Override
        public Object next() {
          if (count == batchReader.getSize()) {
            batchReader.setBatch(it.next());
            count = 0;
          }
          Object o = batchReader.get(count);
          count++;
          return o;
        }
        @Override
        public void remove() {
          throw new UnsupportedOperationException();
        }
      } |]
  where
    reflectStream :: Stream (Of (V.Vector a)) IO ()
                  -> IO (J ('Iface "java.util.Iterator" <> '[Batch a]))
    reflectStream = newIterator . Streaming.mapM reflectBatch

withStatic [d|
  instance Interpretation (Stream (Of a) m r) where
    type Interp (Stream (Of a) m r) = 'Iface "java.util.Iterator"

  instance BatchReify a => Reify (Stream (Of a) IO ()) where
    reify = reifyStreamWithBatching 1024 . generic

  instance BatchReflect a => Reflect (Stream (Of a) IO ()) where
    reflect = fmap unsafeUngeneric . reflectStreamWithBatching 1024
  |]
