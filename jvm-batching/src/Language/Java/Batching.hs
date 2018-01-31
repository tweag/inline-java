{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}

-- | This module provides composable batched marshalling.
--
-- === Batching
--
-- Calls to Java methods via JNI are slow in general. Marshalling an array of
-- primitive values can be as slow as marshalling a single value.
--
-- Because of this, reifying an iterator or a container is best done by
-- accumulates multiple elements on the java side before passing them to the
-- Haskell side. And conversely, when reflecting an iterator or container,
-- multiple Haskell values are put together before marshalling to the Java
-- side.
--
-- Some Haskell values can be batched trivially into arrays of primitive values.
-- 'Int32' can be batched in a java @int[]@, 'Double' can be batched in a java
-- @double[]@, etc. However, other types like @Tuple2 Int32 Double@ would
-- require more primitive arrays. Values of type @Tuple2 Int32 Double@ are
-- batched in a pair of java arrays of type @int[]@ and @double[]@.
--
-- > data Tuple2 a b = Tuple2 a b
--
-- More generally, the design aims to provide composable batchers. If one knows
-- how to batch types @a@ and @b@, one can also batch @Tuple2 a b@, @[a]@,
-- @Vector a@, etc.
--
-- A reference to a batch of values in Java has the type @J (Batch a)@, where
-- @a@ is the Haskell type of the elements in the batch. e.g.
--
-- > type instance Batch Int32 = 'Array ('Prim "int")
-- > type instance Batch Double = 'Array ('Prim "double")
-- > type instance Batch (Tuple2 a b) =
-- >                 'Class "scala.Tuple2" <> '[Batch a, Batch b]
--
-- When defining batching for a new type, one needs to tell how batches are
-- represented in Java by adding a type instance to the type family @Batch@.
-- In addition, procedures for adding and extracting values from the batch
-- need to be specified on both the Haskell and the Java side.
--
-- On the Java side, batches are built using the interface
-- @io.tweag.jvm.batching.BatchWriter@. On the Haskell side, these
-- batches are read using @reifyBatch@.
--
-- > class ( ... ) => BatchReify a where
-- >   newBatchWriter
-- >     :: proxy a
-- >     -> IO (J ('Iface "io.tweag.jvm.batching.BatchWriter"
-- >                  <> [Interp a, Batch a]
-- >              )
-- >           )
-- >   reifyBatch :: J (Batch a) -> Int32 -> IO (V.Vector a)
--
-- @newReifyBatched@ produces a java object implementing the @BatchWriter@
-- interface, and @reifyBatch@ allows to read a batch created in this fashion.
--
-- Conversely, batches can be read on the Java side using the interface
-- @io.tweag.jvm.batching.BatchReader@. And on the Haskell side, these
-- batches can be created with @reflectBatch@.
--
-- > class ( ... ) => BatchReflect a where
-- >  newBatchReader
-- >    :: proxy a
-- >    -> IO (J ('Iface "io.tweag.jvm.batching.BatchReader"
-- >                 <> [Batch a, Interp a]
-- >              )
-- >          )
-- >  reflectBatch :: V.Vector a -> IO (J (Batch a))
--
-- @newBatchReader@ produces a java object implementing the @BatchReader@
-- interface, and @reflectBatch@ allows to create these batches from vectors of
-- Haskell values.
--
-- The methods of @BatchReify@ and @BatchReflect@ offer default
-- implementations which marshal elements in the batch one at a time. Taking
-- advantage of batching requires defining the methods explicitly. The default
-- implementations are useful for cases where speed is not important, for
-- instance when the iterators to reflect or reify contain a single element or
-- just a very few.
--
-- 'Vector's and 'ByteString's are batched with the follow scheme.
--
-- > type instance Batch BS.ByteString
-- >   = 'Class "io.tweag.jvm.batching.Tuple2" <>
-- >        '[ 'Array ('Prim "byte")
-- >         , 'Array ('Prim "int")
-- >         ]
--
-- We use two arrays. One of the arrays contains the result of appending all of
-- the 'ByteString's in the batch. The other array contains the offset of each
-- vector in the resulting array. See 'ArrayBatch'.
--
module Language.Java.Batching
  ( Batchable(..)
  , BatchReify(..)
  , BatchReflect(..)
    -- * Array batching
  , ArrayBatch
  ) where

import Control.Distributed.Closure.TH
import Control.Exception (bracket)
import Control.Monad (forM_, foldM)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Unsafe      as BS
import Data.Int
import Data.Singletons (SingI, Proxy(..))
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign.C.Types (CChar)
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr)
import Foreign.JNI
import Foreign.Ptr
import Foreign.Storable
import Language.Java
import Language.Java.Inline

imports "io.tweag.jvm.batching.*"

-- | A class of types whose values can be marshaled in batches.
class (Interpretation a, SingI (Batch a)) => Batchable (a :: k) where
  -- | The type of java batches for reifying and reflecting values of type @a@.
  type family Batch a :: JType

-- | A class for batching reification of values.
--
-- It has a method to create a batcher that creates batches in Java, and
-- another method that refies a batch into a vector of haskell values.
--
-- The type of the batch used to appear as a class parameter but we run into
-- https://ghc.haskell.org/trac/ghc/ticket/13582
--
class Batchable a => BatchReify a where
  -- | Produces a batcher that aggregates elements of type @ty@ (such as @int@)
  -- and produces collections of type @Batch a@ (such as @int[]@).
  newBatchWriter
    :: proxy a
    -> IO (J ('Iface "io.tweag.jvm.batching.BatchWriter"
                 <> [Interp a, Batch a]
             )
          )

  -- The default implementation makes calls to the JVM for each element in the
  -- batch.
  default newBatchWriter
    :: (Batch a ~ 'Array (Interp a))
    => proxy a
    -> IO (J ('Iface "io.tweag.jvm.batching.BatchWriter"
                 <> [Interp a, Batch a]
             )
          )
  newBatchWriter _ = generic <$> [java| new BatchWriters.ObjectBatchWriter() |]

  -- | Reifies the values in a batch of type @Batch a@.
  -- Gets the batch and the amount of elements it contains.
  reifyBatch :: J (Batch a) -> Int32 -> IO (V.Vector a)

  -- The default implementation makes calls to the JVM for each element in the
  -- batch.
  default reifyBatch
    :: (Reify a, Batch a ~ 'Array (Interp a))
    => J (Batch a) -> Int32 -> IO (V.Vector a)
  reifyBatch jxs size =
      V.generateM (fromIntegral size) $ \i ->
      getObjectArrayElement jxs (fromIntegral i) >>= reify . unsafeCast

-- | Helper for reifying batches of primitive types
reifyPrimitiveBatch
  :: Storable a
  => (J ('Array ty) -> IO (Ptr a))
  -> (J ('Array ty) -> Ptr a -> IO ())
  -> J ('Array ty) -> Int32 -> IO (V.Vector a)
reifyPrimitiveBatch getArrayElements releaseArrayElements jxs size = do
    bracket (getArrayElements jxs) (releaseArrayElements jxs)
      $ V.generateM (fromIntegral size) . peekElemOff

-- | Batches of arrays of variable length
--
-- The first component is an array or batch containing the actual elements,
-- and the second component is an @int[]@ where the ith position has the
-- offset of the first position after the ith array.
type ArrayBatch ty =
    'Class "io.tweag.jvm.batching.Tuple2" <>
       '[ ty
        , 'Array ('Prim "int")
        ]

-- | Helper for reifying batches of vectors
--
-- Arrays are batched with two arrays. One of the arrays contains the result
-- of appending all of the vectors in the batch. The other array contains the
-- offset of each vector in the resulting array.
--
reifyArrayBatch
  :: forall a b ty.
     (Int32 -> J ty -> IO a) -- ^ reify the array/batch of values
                             -- (takes the amount of elements in the array)
  -> (Int -> Int -> a -> IO b) -- ^ slice at a given offset of given length of some value
  -> J (ArrayBatch ty)
  -> Int32
  -> IO (V.Vector b)
reifyArrayBatch reifyB slice batch0 batchSize = do
    result <- VM.new (fromIntegral batchSize)
    let batch = unsafeUngeneric batch0
    vecEnds <- [java| (int[])$batch._2 |] >>= reify
    let count = if VS.null vecEnds then 0 else VS.last vecEnds
    vecValues <- [java| $batch._1 |] >>= reifyB count . fromObject
    _ <- foldM (writeVector result vecEnds vecValues) 0 [0 .. batchSize - 1]
    V.unsafeFreeze result
  where
    fromObject :: JObject -> J x
    fromObject = unsafeCast

    writeVector :: VM.IOVector b -> VS.Vector Int32 -> a -> Int32 -> Int32 -> IO Int32
    writeVector mv ends values offset i32 = do
        let i = fromIntegral i32
        slice (fromIntegral offset)
              (fromIntegral $ ends VS.! i - offset) values
          >>= VM.unsafeWrite mv i
        return $ ends VS.! i

-- | A class for batching reflection of values.
--
-- It has a method to create a batch reader that reads batches in Java, and
-- another method that reflects a vector of haskell values into a batch.
--
-- We considered having the type of the batch appear as a class parameter but
-- we run into https://ghc.haskell.org/trac/ghc/ticket/13582
--
class Batchable a => BatchReflect a where
  -- | Produces a batch reader that receives collections of type @ty1@
  -- (such as @int[]@) and produces values of type @ty2@ (such as @int@).
  newBatchReader
    :: proxy a
    -> IO (J ('Iface "io.tweag.jvm.batching.BatchReader"
                 <> [Batch a, Interp a]
             )
          )

  -- The default implementation makes calls to the JVM for each element in the
  -- batch.
  default newBatchReader
    :: (Batch a ~ 'Array (Interp a))
    => proxy a
    -> IO (J ('Iface "io.tweag.jvm.batching.BatchReader"
                       <> [Batch a, Interp a]
             )
          )
  newBatchReader _ =
      generic <$> [java| new BatchReaders.ObjectBatchReader() |]

  -- | Reflects the values in a vector to a batch of type @ty@.
  reflectBatch :: V.Vector a -> IO (J (Batch a))
  -- The default implementation makes calls to the JVM for each element in the
  -- batch.
  default reflectBatch
    :: (Reflect a, Batch a ~ 'Array (Interp a))
    => V.Vector a -> IO (J (Batch a))
  reflectBatch v = do
      jxs <- newArray $ fromIntegral (V.length v)
      forM_ [0 .. V.length v - 1] $ \i ->
        withLocalRef (reflect (v V.! i))
                     (setObjectArrayElement jxs (fromIntegral i))
      return jxs

-- | Helper for reflecting batches of primitive types
reflectPrimitiveBatch
  :: forall a ty. (Storable a, IsPrimitiveType ty)
  => (J ('Array ty) -> Int32 -> Int32 -> Ptr a -> IO ())
  -> V.Vector a -> IO (J ('Array ty))
reflectPrimitiveBatch setArrayRegion v = do
    let (fptr, offset, len) = VS.unsafeToForeignPtr (V.convert v)
    withForeignPtr fptr $ \ptr -> do
      jxs <- newArray (fromIntegral len)
      let aOffset = offset * sizeOf (undefined :: a)
      setArrayRegion jxs 0 (fromIntegral len) (plusPtr ptr aOffset)
      return jxs

-- | Helper for reflecting batches of vectors
--
-- The vector type is a, and vectors are manipulated exclusively with the
-- polymorphic functions given as arguments.
--
-- Vectors are batched with two arrays. One of the arrays contains the result
-- of appending all of the vectors in the batch. The other array contains the
-- offset of each vector in the resulting array.
--
reflectArrayBatch
  :: forall a b ty.
     (b -> IO (J ty))
  -> (a -> Int) -- ^ get length
  -> ([a] -> IO b) -- ^ concat
  -> V.Vector a
  -> IO (J (ArrayBatch ty))
reflectArrayBatch reflectB getLength concatenate vecs = do
    let ends = V.convert $ V.postscanl' (+) 0 $
                V.map (fromIntegral . getLength) vecs
                  :: VS.Vector Int32
    bigvec <- concatenate $ V.toList vecs
    jvec <- reflectB bigvec
    jends <- reflect ends
    generic <$> Language.Java.new [ coerce (upcast jvec)
                                  , coerce (upcast jends)
                                  ]

withStatic [d|
  instance Batchable Bool where
    type Batch Bool = 'Array ('Prim "boolean")

  instance BatchReify Bool where
    newBatchWriter _ = [java| new BatchWriters.BooleanBatchWriter() |]
    reifyBatch jxs size = do
        let toBool w = if w == 0 then False else True
        bracket (getBooleanArrayElements jxs)
                (releaseBooleanArrayElements jxs) $
          \arr -> V.generateM (fromIntegral size)
                              ((toBool <$>) . peekElemOff arr)

  instance Batchable CChar where
    type Batch CChar = 'Array ('Prim "byte")

  instance BatchReify CChar where
    newBatchWriter _ = [java| new BatchWriters.ByteBatchWriter() |]
    reifyBatch =
      reifyPrimitiveBatch getByteArrayElements releaseByteArrayElements

  instance Batchable Word16 where
    type Batch Word16 = 'Array ('Prim "char")

  instance BatchReify Word16 where
    newBatchWriter _ = [java| new BatchWriters.CharacterBatchWriter() |]
    reifyBatch =
      reifyPrimitiveBatch getCharArrayElements releaseCharArrayElements

  instance Batchable Int16 where
    type Batch Int16 = 'Array ('Prim "short")

  instance BatchReify Int16 where
    newBatchWriter _ = [java| new BatchWriters.ShortBatchWriter() |]
    reifyBatch =
      reifyPrimitiveBatch getShortArrayElements releaseShortArrayElements

  instance Batchable Int32 where
    type Batch Int32 = 'Array ('Prim "int")

  instance BatchReify Int32 where
    newBatchWriter _ = [java| new BatchWriters.IntegerBatchWriter() |]
    reifyBatch =
      reifyPrimitiveBatch getIntArrayElements releaseIntArrayElements

  instance Batchable Int64 where
    type Batch Int64 = 'Array ('Prim "long")

  instance BatchReify Int64 where
    newBatchWriter _ = [java| new BatchWriters.LongBatchWriter() |]
    reifyBatch =
      reifyPrimitiveBatch getLongArrayElements releaseLongArrayElements

  instance Batchable Float where
    type Batch Float = 'Array ('Prim "float")

  instance BatchReify Float where
    newBatchWriter _ = [java| new BatchWriters.FloatBatchWriter() |]
    reifyBatch =
      reifyPrimitiveBatch getFloatArrayElements releaseFloatArrayElements

  instance Batchable Double where
    type Batch Double = 'Array ('Prim "double")

  instance BatchReify Double where
    newBatchWriter _ = [java| new BatchWriters.DoubleBatchWriter() |]
    reifyBatch =
      reifyPrimitiveBatch getDoubleArrayElements releaseDoubleArrayElements

  instance BatchReflect Bool where
    newBatchReader _ = [java| new BatchReaders.BooleanBatchReader() |]
    reflectBatch = reflectPrimitiveBatch setBooleanArrayRegion
                 . V.map (\w -> if w then 1 else 0)

  instance BatchReflect CChar where
    newBatchReader _ = [java| new BatchReaders.ByteBatchReader() |]
    reflectBatch = reflectPrimitiveBatch setByteArrayRegion

  instance BatchReflect Word16 where
    newBatchReader _ = [java| new BatchReaders.CharacterBatchReader() |]
    reflectBatch = reflectPrimitiveBatch setCharArrayRegion

  instance BatchReflect Int16 where
    newBatchReader _ = [java| new BatchReaders.ShortBatchReader() |]
    reflectBatch = reflectPrimitiveBatch setShortArrayRegion

  instance BatchReflect Int32 where
    newBatchReader _ = [java| new BatchReaders.IntegerBatchReader() |]
    reflectBatch = reflectPrimitiveBatch setIntArrayRegion

  instance BatchReflect Int64 where
    newBatchReader _ = [java| new BatchReaders.LongBatchReader() |]
    reflectBatch = reflectPrimitiveBatch setLongArrayRegion

  instance BatchReflect Float where
    newBatchReader _ = [java| new BatchReaders.FloatBatchReader() |]
    reflectBatch = reflectPrimitiveBatch setFloatArrayRegion

  instance BatchReflect Double where
    newBatchReader _ = [java| new BatchReaders.DoubleBatchReader() |]
    reflectBatch = reflectPrimitiveBatch setDoubleArrayRegion

#if ! (__GLASGOW_HASKELL__ == 800 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1)
  instance Batchable BS.ByteString where
    type Batch BS.ByteString
      = 'Class "io.tweag.jvm.batching.Tuple2" <>
         '[ 'Array ('Prim "byte")
          , 'Array ('Prim "int")
          ]

  instance Batchable (VS.Vector Word16) where
    type Batch (VS.Vector Word16)
      = 'Class "io.tweag.jvm.batching.Tuple2" <>
         '[ 'Array ('Prim "char")
          , 'Array ('Prim "int")
          ]

  instance Batchable (VS.Vector Int16) where
    type Batch (VS.Vector Int16)
      = 'Class "io.tweag.jvm.batching.Tuple2" <>
         '[ 'Array ('Prim "short")
          , 'Array ('Prim "int")
          ]

  instance Batchable (VS.Vector Int32) where
    type Batch (VS.Vector Int32)
      = 'Class "io.tweag.jvm.batching.Tuple2" <>
         '[ 'Array ('Prim "int")
          , 'Array ('Prim "int")
          ]

  instance Batchable (VS.Vector Int64) where
    type Batch (VS.Vector Int64)
      = 'Class "io.tweag.jvm.batching.Tuple2" <>
         '[ 'Array ('Prim "long")
          , 'Array ('Prim "int")
          ]

  instance Batchable (VS.Vector Float) where
    type Batch (VS.Vector Float)
      = 'Class "io.tweag.jvm.batching.Tuple2" <>
         '[ 'Array ('Prim "float")
          , 'Array ('Prim "int")
          ]

  instance Batchable (VS.Vector Double) where
    type Batch (VS.Vector Double)
      = 'Class "io.tweag.jvm.batching.Tuple2" <>
         '[ 'Array ('Prim "double")
          , 'Array ('Prim "int")
          ]

  instance Batchable Text.Text where
    type Batch Text.Text
      = 'Class "io.tweag.jvm.batching.Tuple2" <>
         '[ 'Array ('Prim "char")
          , 'Array ('Prim "int")
          ]

  instance BatchReify BS.ByteString where
    newBatchWriter _ = [java| new BatchWriters.ByteArrayBatchWriter() |]
    reifyBatch = reifyArrayBatch (const reify) bsUnsafeSlice
      where
        bsUnsafeSlice :: Int -> Int -> BS.ByteString -> IO BS.ByteString
        bsUnsafeSlice offset sz = return . BS.unsafeTake sz . BS.unsafeDrop offset

  instance BatchReify (VS.Vector Word16) where
    newBatchWriter _ = [java| new BatchWriters.CharArrayBatchWriter() |]
    reifyBatch =
        reifyArrayBatch (const reify) (fmap (fmap return) . VS.unsafeSlice)

  instance BatchReify (VS.Vector Int16) where
    newBatchWriter _ = [java| new BatchWriters.ShortArrayBatchWriter() |]
    reifyBatch =
        reifyArrayBatch (const reify) (fmap (fmap return) . VS.unsafeSlice)

  instance BatchReify (VS.Vector Int32) where
    newBatchWriter _ = [java| new BatchWriters.IntArrayBatchWriter() |]
    reifyBatch =
        reifyArrayBatch (const reify) (fmap (fmap return) . VS.unsafeSlice)

  instance BatchReify (VS.Vector Int64) where
    newBatchWriter _ = [java| new BatchWriters.LongArrayBatchWriter() |]
    reifyBatch =
        reifyArrayBatch (const reify) (fmap (fmap return) . VS.unsafeSlice)

  instance BatchReify (VS.Vector Float) where
    newBatchWriter _ = [java| new BatchWriters.FloatArrayBatchWriter() |]
    reifyBatch =
        reifyArrayBatch (const reify) (fmap (fmap return) . VS.unsafeSlice)

  instance BatchReify (VS.Vector Double) where
    newBatchWriter _ = [java| new BatchWriters.DoubleArrayBatchWriter() |]
    reifyBatch =
        reifyArrayBatch (const reify) (fmap (fmap return) . VS.unsafeSlice)

  instance BatchReify Text.Text where
    newBatchWriter _ = [java| new BatchWriters.StringArrayBatchWriter() |]
    reifyBatch = reifyArrayBatch (const reify) $ \o n vs ->
                  (VS.unsafeWith (VS.unsafeSlice o n vs) $ \ptr ->
                      Text.fromPtr ptr (fromIntegral n)
                  )

  instance BatchReflect BS.ByteString where
    newBatchReader _ = [java| new BatchReaders.ByteArrayBatchReader() |]
    reflectBatch = reflectArrayBatch reflect BS.length (return . BS.concat)

  instance BatchReflect (VS.Vector Word16) where
    newBatchReader _ = [java| new BatchReaders.CharArrayBatchReader() |]
    reflectBatch = reflectArrayBatch reflect VS.length (return . VS.concat)

  instance BatchReflect (VS.Vector Int16) where
    newBatchReader _ = [java| new BatchReaders.ShortArrayBatchReader() |]
    reflectBatch = reflectArrayBatch reflect VS.length (return . VS.concat)

  instance BatchReflect (VS.Vector Int32) where
    newBatchReader _ = [java| new BatchReaders.IntArrayBatchReader() |]
    reflectBatch = reflectArrayBatch reflect VS.length (return . VS.concat)

  instance BatchReflect (VS.Vector Int64) where
    newBatchReader _ = [java| new BatchReaders.LongArrayBatchReader() |]
    reflectBatch = reflectArrayBatch reflect VS.length (return . VS.concat)

  instance BatchReflect (VS.Vector Float) where
    newBatchReader _ = [java| new BatchReaders.FloatArrayBatchReader() |]
    reflectBatch = reflectArrayBatch reflect VS.length (return . VS.concat)

  instance BatchReflect (VS.Vector Double) where
    newBatchReader _ = [java| new BatchReaders.DoubleArrayBatchReader() |]
    reflectBatch = reflectArrayBatch reflect VS.length (return . VS.concat)

  instance BatchReflect Text.Text where
    newBatchReader _ = [java| new BatchReaders.StringArrayBatchReader() |]
    reflectBatch = reflectArrayBatch reflect Text.length $ \ts ->
                     Text.useAsPtr (Text.concat ts) $ \ptr len ->
                       (`VS.unsafeFromForeignPtr0` fromIntegral len)
                         <$> newForeignPtr_ ptr
#endif

  instance Interpretation a => Interpretation (V.Vector a) where
    type Interp (V.Vector a) = 'Array (Interp a)

  -- TODO: Fix GHC so it doesn't complain that variables used exclusively in
  -- quasiquotes are unused. Thus we can stop prepending variable names with
  -- '_'.

  instance (Interpretation a, BatchReify a)
           => Reify (V.Vector a) where
    reify jv = do
        _batcher <- unsafeUngeneric <$> newBatchWriter (Proxy :: Proxy a)
        n <- getArrayLength jv
        let _jvo = arrayUpcast jv
        batch <- [java| {
          $_batcher.start($n);
          for(int i=0;i<$_jvo.length;i++)
             $_batcher.set(i, $_jvo[i]);
          return $_batcher.getBatch();
          } |]
        reifyBatch (fromObject batch) n
      where
        fromObject :: JObject -> J x
        fromObject = unsafeCast

  instance (Interpretation a, BatchReflect a)
           => Reflect (V.Vector a) where
    reflect v = do
        _batch <- upcast <$> reflectBatch v
        _batchReader <-
          unsafeUngeneric <$> newBatchReader (Proxy :: Proxy a)
        jv <- [java| {
          $_batchReader.setBatch($_batch);
          return $_batchReader.getSize();
          } |] >>= newArray :: IO (J ('Array (Interp a)))
        let _jvo = arrayUpcast jv
        () <- [java| {
          for(int i=0;i<$_jvo.length;i++)
            $_jvo[i] = $_batchReader.get(i);
          } |]
        return jv
 |]

-- TODO: Fix distributed-closure so these instances can be put in a
-- 'withStatic' block.

instance Batchable a => Batchable (V.Vector a) where
  type Batch (V.Vector a) = ArrayBatch (Batch a)

instance (SingI (Interp a), SingI (Batch a), BatchReify a)
         => BatchReify (V.Vector a) where
  newBatchWriter _ = do
      b <- unsafeUngeneric <$> newBatchWriter (Proxy :: Proxy a)
      generic <$> [java| new BatchWriters.ObjectArrayBatchWriter($b) |]
  reifyBatch =
    reifyArrayBatch (flip reifyBatch) (fmap (fmap return) . V.unsafeSlice)

instance (SingI (Interp a), SingI (Batch a), BatchReflect a)
         => BatchReflect (V.Vector a) where
  newBatchReader _ = do
      b <- unsafeUngeneric <$> newBatchReader (Proxy :: Proxy a)
      generic <$> [java| new BatchReaders.ObjectArrayBatchReader($b) |]
  reflectBatch =
      reflectArrayBatch reflectBatch V.length (return . V.concat)
