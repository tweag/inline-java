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

-- | This module provides composable batched marshalling.
--
-- It provides reify and reflect instances for vectors, which marshal
-- values in batches, which is more efficient than marshalling values
-- one at a time.
--
-- > instance (Interpretation a, BatchReify a) => Reify (V.Vector a) where
-- > ...
--
-- > instance (Interpretation a, BatchReflect a) => Reflect (V.Vector a) where
-- > ...
--
-- === Batching
--
-- Calls to Java methods via JNI are slow in general. Marshalling an array of
-- primitive values can be as slow as marshalling a single value.
--
-- Because of this, reifying an iterator or a container is best done by
-- accumulating multiple elements on the java side before passing them to the
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
-- @newBatchWriter@ produces a java object implementing the @BatchWriter@
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
-- just very few.
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
  --
  -- Gets the batch, the amount of elements it contains and a predicate that
  -- indicates which positions of the batch to reify. The value at position i
  -- in the returned vector holds bottom if @p i@ is False.
  --
  -- The predicate is used by instances of BatchReify to control which
  -- positions are to be reified in the batches of the components of a
  -- composite type whose values are allowed to be null.
  reifyBatch :: J (Batch a) -> Int32 -> (Int -> Bool) -> IO (V.Vector a)

  -- The default implementation makes calls to the JVM for each element in the
  -- batch.
  default reifyBatch
    :: (Reify a, Batch a ~ 'Array (Interp a))
    => J (Batch a) -> Int32 -> (Int -> Bool) -> IO (V.Vector a)
  reifyBatch jxs size p =
    V.generateM (fromIntegral size) $ \i ->
      if p i then
        getObjectArrayElement jxs (fromIntegral i) >>= reify . unsafeCast
      else
        return $ error "default reifyBatch: reification skipped."

-- | Helper for reifying batches of primitive types
reifyPrimitiveBatch
  :: Storable a
  => (J ('Array ty) -> IO (Ptr a))
  -> (J ('Array ty) -> Ptr a -> IO ())
  -> J ('Array ty)
  -> Int32
  -> (Int -> Bool)
  -> IO (V.Vector a)
reifyPrimitiveBatch getArrayElements releaseArrayElements jxs size p = do
    bracket (getArrayElements jxs) (releaseArrayElements jxs) $ \arr ->
      V.generateM (fromIntegral size) $ \i ->
        if p i then peekElemOff arr i
        else return $ error "default reifyBatch: reification skipped."

-- | Batches of arrays of variable length
--
-- The first component is an array or batch B containing the elements
-- of all the arrays in the batch. The second component is an array of
-- offsets F. The ith position in the offset array is the first position
-- in B after the ith array of the batch.
--
-- Thus, the first array of the batch can be found in B between the
-- indices 0 and F[0], the second array of the batch is between the
-- indices F[0] and F[1], and so on.
--
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
  -> (Int -> Int -> a -> IO b) -- ^ slice at a given offset of given length of some array a
  -> J (ArrayBatch ty)
  -> Int32
  -> (Int -> Bool)
  -> IO (V.Vector b)
reifyArrayBatch reifyB slice batch0 batchSize p = do
    let batch = unsafeUngeneric batch0
    arrayEnds <- reifyArrayOffsets batch
    arrayValues <- reifyArrayValues arrayEnds batch
    reifySlices arrayEnds arrayValues
  where
    fromObject :: JObject -> J x
    fromObject = unsafeCast

    reifyArrayOffsets
      :: J ('Class "io.tweag.jvm.batching.Tuple2") -> IO (VS.Vector Int32)
    reifyArrayOffsets batch = [java| (int[])$batch._2 |] >>= reify

    reifyArrayValues arrayEnds batch = do
      let count = if VS.null arrayEnds then 0 else VS.last arrayEnds
      [java| $batch._1 |] >>= reifyB count . fromObject

    reifySlices arrayEnds arrayValues = do
      result <- VM.new (fromIntegral batchSize)
      _ <- foldM
           (writeSliceToVector result arrayEnds arrayValues)
           0
           [0 .. fromIntegral batchSize - 1]
      V.unsafeFreeze result

    writeSliceToVector
      :: VM.IOVector b   -- ^ output vector to write to
      -> VS.Vector Int32 -- ^ ends[i] holds the offset of the (i+1)th slice
      -> a               -- ^ input vector to read slices from
      -> Int32           -- ^ offset of the slice to read from the input vector
      -> Int             -- ^ index of the position to write in the output vector
      -> IO Int32        -- ^ offset of the next slice to read
    writeSliceToVector output arrayEnds arrayValues offset i = do
        -- Account for the posibility of @arrayEnds ! i@ to be 0
        -- in case the position in the batch has been left uninitialized.
        let nextOffset = max (arrayEnds VS.! i) offset
        if p i then do
          slice (fromIntegral offset)
                (fromIntegral $ nextOffset - offset) arrayValues
            >>= VM.unsafeWrite output i
          return nextOffset
        else do
          VM.unsafeWrite output i $
            error "reifyPrimitiveArrayBatch: reification skipped."
          return offset

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
    generic <$> Language.Java.new (upcast jvec) (upcast jends)

withStatic [d|
  instance Batchable Bool where
    type Batch Bool = 'Array ('Prim "boolean")

  instance BatchReify Bool where
    newBatchWriter _ = [java| new BatchWriters.BooleanBatchWriter() |]
    reifyBatch jxs size p = do
        let toBool w = if w == 0 then False else True
        bracket (getBooleanArrayElements jxs)
                (releaseBooleanArrayElements jxs) $
          \arr -> V.generateM (fromIntegral size) $ \i ->
            if p i then
              toBool <$> peekElemOff arr i
            else
              return $ error "reifyBatch Bool: reification skipped."

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
    reify jv =
        withLocalRef (unsafeUngeneric <$> newBatchWriter (Proxy :: Proxy a))
        $ \_batcher -> do
          n <- getArrayLength jv
          let _jvo = arrayUpcast jv
          withLocalRef [java| {
            $_batcher.start($n);
            for(int i=0;i<$_jvo.length;i++)
               $_batcher.set(i, $_jvo[i]);
            return $_batcher.getBatch();
            } |]
            $ \batch ->
              reifyBatch (fromObject batch) n (const True)
      where
        fromObject :: JObject -> J x
        fromObject = unsafeCast

  instance (Interpretation a, BatchReflect a)
           => Reflect (V.Vector a) where
    reflect v =
        withLocalRef (upcast <$> reflectBatch v) $ \_batch ->
        withLocalRef (unsafeUngeneric <$> newBatchReader (Proxy :: Proxy a))
        $ \_batchReader -> do
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

  instance Batchable a => Batchable (V.Vector a) where
    type Batch (V.Vector a) = ArrayBatch (Batch a)

  instance BatchReify a => BatchReify (V.Vector a) where
    newBatchWriter _ =
        withLocalRef
          (unsafeUngeneric <$> newBatchWriter (Proxy :: Proxy a))
          $ \_b ->
            generic <$> [java| new BatchWriters.ObjectArrayBatchWriter($_b) |]
    reifyBatch =
        reifyArrayBatch
          -- skipped values do not leave holes in a batch of arrays, so it
          -- is safe to reify all of the values
          (\sz j -> reifyBatch j sz (const True))
          (fmap (fmap return) . V.unsafeSlice)

  instance BatchReflect a => BatchReflect (V.Vector a) where
    newBatchReader _ =
        withLocalRef
          (unsafeUngeneric <$> newBatchReader (Proxy :: Proxy a))
          $ \_b ->
            generic <$> [java| new BatchReaders.ObjectArrayBatchReader($_b) |]
    reflectBatch =
        reflectArrayBatch reflectBatch V.length (return . V.concat)

  instance Batchable a => Batchable (Nullable a) where
    type Batch (Nullable a) =
      'Class "io.tweag.jvm.batching.Tuple2" <>
        '[ 'Array ('Prim "boolean")
         , ArrayBatch (Batch a)
         ]

  instance BatchReify a => BatchReify (Nullable a) where
    newBatchWriter _ =
      withLocalRef
        (unsafeUngeneric <$> newBatchWriter (Proxy :: Proxy a))
        $ \_baseBatcher ->
          generic <$> [java| new BatchWriters.NullableBatchWriter($_baseBatcher) |]

    reifyBatch jxs n p = do
      let _batch = unsafeUngeneric jxs
      isnull <- withLocalRef [java| $_batch._1 |] $ \j ->
        V.convert <$> (reify (unsafeCast (j :: JObject)) :: IO (VS.Vector W8Bool))
      let p' i = p i && isnull V.! i == 0
      b <- withLocalRef [java| $_batch._2 |] $ \j0 ->
         (\j -> reifyBatch j n p') (unsafeCast (j0 :: JObject) :: J (Batch a))
      return $ V.zipWith toNullable isnull b
      where
        toNullable :: W8Bool -> a -> Nullable a
        toNullable 0 a = NotNull a
        toNullable _ _ = Null
 |]
