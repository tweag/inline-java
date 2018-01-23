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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

module Language.Java.Streaming () where

import Control.Distributed.Closure.TH
import Control.Monad.IO.Class (liftIO)
import qualified Data.Coerce as Coerce
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Proxy
import qualified Data.Vector as V
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
import Language.Java.Batching
import Language.Java.Inline
import Streaming (Bifunctor(first), Stream, Of)
import qualified Streaming as Streaming
import qualified Streaming.Prelude as Streaming

imports "io.tweag.jvm.batching.*"
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

-- | Reflects a stream with no batching.
newIterator
  :: forall ty. Stream (Of (J ty)) IO ()
  -> IO (J ('Iface "java.util.Iterator" <> '[ty]))
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
    let popStream :: Ptr JObject -> IO (J ty)
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
              return x
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
    return $ generic iterator

-- | Reifies streams.
reifyStreamWithBatching
  :: forall a. ReifyBatcher a
  => J ('Iface "java.util.Iterator" <> '[Interp a])
  -> IO (Stream (Of a) IO ())
reifyStreamWithBatching jiterator0 = do
    let jiterator1 = unsafeUngeneric jiterator0
    jbatcher <- unsafeUngeneric <$> newReifyBatcher (Proxy :: Proxy a)
    jiterator <- [java| new Iterator() {
        private final int batchSize = 1024;
        private final Iterator it = $jiterator1;
        private final Batcher batcher = $jbatcher;
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

-- | Reflects streams.
reflectStreamWithBatching
  :: forall a. ReflectBatchReader a
  => Stream (Of a) IO ()
  -> IO (J ('Iface "java.util.Iterator" <> '[Interp a]))
reflectStreamWithBatching s0 = do
    jiterator <- unsafeUngeneric <$>
      (reflectStream $ Streaming.mapsM
                        (\s -> first V.fromList <$> Streaming.toList s)
                     $ Streaming.chunksOf 1024 s0
      )
    jbatchReader <- unsafeUngeneric <$> newReflectBatchReader (Proxy :: Proxy a)
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

  instance ReifyBatcher a => Reify (Stream (Of a) IO ()) where
    reify = reifyStreamWithBatching . generic

  instance ReflectBatchReader a => Reflect (Stream (Of a) IO ()) where
    reflect = fmap unsafeUngeneric . reflectStreamWithBatching
  |]
