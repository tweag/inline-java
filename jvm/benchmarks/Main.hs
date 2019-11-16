{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main as Criterion
import Control.Monad (replicateM, replicateM_, void)
import Data.Int
import Data.IORef
import Data.Singletons (SomeSing(..))
import Data.Text (Text)
import qualified Foreign.Concurrent as Concurrent
import qualified Foreign.ForeignPtr as ForeignPtr
import Foreign.JNI
import Foreign.Marshal.Alloc (mallocBytes, finalizerFree, free)
import Foreign.Marshal.Array (callocArray, mallocArray)
import Foreign.Ptr (castPtr)
import Language.Java

newtype Box a = Box { unBox :: a }

-- Not much sense in deepseq'ing foreign pointers. But needed for call to 'env'
-- below.
instance NFData (Box a) where rnf (Box a) = seq a ()

jabs :: Int32 -> IO Int32
jabs x = callStatic "java.lang.Math" "abs" With1Args x

jniAbs :: JClass -> JMethodID -> Int32 -> IO Int32
jniAbs klass method x = callStaticIntMethod klass method [coerce x]

intValue :: Int32 -> IO Int32
intValue x = do
    jx <- reflect x
    call jx "intValue" With0Args

compareTo :: Int32 -> Int32 -> IO Int32
compareTo x y = do
    jx <- reflect x
    jy <- reflect y
    call jx "compareTo" With1Args jy

incrHaskell :: Int32 -> IO Int32
incrHaskell x = return (x + 1)

foreign import ccall unsafe getpid :: IO Int

benchCalls :: Benchmark
benchCalls =
    env ini $ \ ~(Box klass, method) ->
      bgroup "Calls"
      [ bgroup "Java calls"
        [ bench "static method call: unboxed single arg / unboxed return" $ nfIO $ jabs 1
        , bench "static method call: boxed single arg / boxed return" $
          perBatchEnvWithCleanup
            (\batchSize -> do
               pushLocalFrame (2 * fromIntegral batchSize)
               Box <$> reflect ("123" :: Text)
            )
            (\_ _ -> void (popLocalFrame jnull)) $
            \(Box jStringInteger) -> do
              _ <- callStatic "java.lang.Integer" "valueOf" With1Args jStringInteger
                 :: IO (J ('Class "java.lang.Integer"))
              return ()
        , bench "jni static method call: unboxed single arg / unboxed return" $ nfIO $ jniAbs klass method 1
        , bench "method call: no args / unboxed return" $ nfIO $ intValue 1
        , bench "method call: boxed single arg / unboxed return" $ nfIO $ compareTo 1 1
        , bench "getClass" $
          perBatchEnvWithCleanup
            (pushLocalFrame . (2*) . fromIntegral)
            (\_ _ -> void (popLocalFrame jnull)) $
            \() -> do
              _ <- getClass (SClass "java/lang/Math")
              return ()
        , bench "getStaticMethodID" $
          perBatchEnvWithCleanup
            (\_ -> Box <$> getClass (SClass "java/lang/Math"))
            (\_ (Box c) -> deleteLocalRef c) $
            \ ~(Box c) -> do
              _ <- getStaticMethodID c "abs" absSignature
              return ()
        , bench "getMethodID" $
          perBatchEnvWithCleanup
            (\_ -> Box <$> getClass (SClass "java/lang/Integer"))
            (\_ (Box c) -> deleteLocalRef c) $
            \ ~(Box c) -> do
              _ <- getMethodID c "intValue" (methodSignature [] (SPrim "int"))
              return ()
        ]
      , bgroup "Haskell calls"
        [ bench "incr haskell" $ nfIO $ incrHaskell 1
        , bench "ffi haskell" $ nfIO $ getpid
        ]
      ]
  where
    absSignature = methodSignature [SomeSing (sing :: Sing ('Prim "int"))] (SPrim "int")
    ini = do
      klass <- findClass (referenceTypeName (SClass "java/lang/Math"))
      method <- getStaticMethodID klass "abs" absSignature
      return (Box klass, method)

benchRefs :: Benchmark
benchRefs =
    env (Box <$> new With0Args) $ \ ~(Box (jobj :: JObject)) ->
    bgroup "References"
    [ bench "local reference" $ nfIO $ do
        _ <- newLocalRef jobj
        return ()
    , bench "global reference" $ nfIO $ do
        _ <- newGlobalRef jobj
        return ()
    ,  bench "global reference (no finalizer)" $ nfIO $ do
        _ <- newGlobalRefNonFinalized jobj
        return ()
    , bench "Foreign.Concurrent.newForeignPtr" $ nfIO $ do
        _ <- Concurrent.newForeignPtr (unsafeObjectToPtr jobj) (return ())
        return ()
    , bench "Foreign.ForeignPtr.newForeignPtr" $ nfIO $ do
        -- Approximate cost of malloc: 50ns. Ideally would move out of benchmark
        -- but finalizer not idempotent.
        ptr <- mallocBytes 4
        _ <- ForeignPtr.newForeignPtr finalizerFree ptr
        return ()
    , bench "local frame / 1 reference" $ nfIO $ do
        pushLocalFrame 30
        _ <- newLocalRef jobj
        _ <- popLocalFrame jnull
        return ()
    , bench "delete 1 local ref" $ nfIO $
        newLocalRef jobj >>= deleteLocalRef
    , bench "local frame / 30 references" $ nfIO $ do
        pushLocalFrame 30
        replicateM_ 30 $ newLocalRef jobj
        _ <- popLocalFrame jnull
        return ()
    , bench "delete 30 local refs" $ nfIO $
        replicateM_ 30 $ newLocalRef jobj >>= deleteLocalRef
    ]

benchNew :: Benchmark
benchNew =
    bgroup "new"
    [ bench "Integer" $
      perBatchEnvWithCleanup
        (pushLocalFrame . (2*) . fromIntegral)
        (\_ _ -> void (popLocalFrame jnull)) $
        \() -> do
          _ <- new With1Args (2 :: Int32) :: IO (J ('Class "java.lang.Integer"))
          return ()
    , bench "Integer.valueOf" $
      perBatchEnvWithCleanup
        (pushLocalFrame . (2*) . fromIntegral)
        (\_ _ -> void (popLocalFrame jnull)) $
        \() -> do
          _ <- callStatic "java.lang.Integer" "valueOf" With1Args (2 :: Int32)
                 :: IO (J ('Class "java.lang.Integer"))
          return ()
    , envWithCleanup allocTextPtr freeTextPtr $ \ ~(Box (ptr, len)) ->
      bench "newString" $
      perBatchEnvWithCleanup
        (pushLocalFrame . (2*) . fromIntegral)
        (\_ _ -> void (popLocalFrame jnull)) $
        \() ->
          void $ newString ptr (fromIntegral len)
    , envWithCleanup allocTextPtr freeTextPtr $ \ ~(Box (_ptr, len)) ->
      bench "newArray" $
      perBatchEnvWithCleanup
        (pushLocalFrame . (2*) . fromIntegral)
        (\_ _ -> void (popLocalFrame jnull)) $
        \() ->
          void $ newByteArray (fromIntegral len)
    , envWithCleanup allocTextPtr freeTextPtr $ \ ~(Box (ptr, len)) ->
      bench "newDirectByteBuffer" $
      perBatchEnvWithCleanup
        (pushLocalFrame . (2*) . fromIntegral)
        (\_ _ -> void (popLocalFrame jnull)) $
        \() ->
          void $ newDirectByteBuffer (castPtr ptr) (2 * len)
    ]
  where
    allocTextPtr = do
      let len = 128
      dst <- callocArray len
      return $ Box (dst, fromIntegral len)
    freeTextPtr (Box (p, _)) = free p

benchArrays :: Benchmark
benchArrays =
    bgroup "Arrays" $ (++ otherBenchmarks) $ (`map` [128, 256, 512]) $ \arraySize ->
    let n :: Num b => b
        n = fromIntegral (arraySize :: Int) in
    env (Box <$> mallocArray n) $ \ ~(Box bytes) ->
    env (Box <$> newArray n) $ \ ~(Box jbytes) ->
    bgroup (show (n :: Int))
    [ bench "getByteArrayElements" $
      perBatchEnvWithCleanup (\_ -> newIORef []) (const cleanArrays) $
      \ref -> do
        p <- getByteArrayElements jbytes
        modifyIORef ref ((jbytes, p) :)
    , bench "releaseByteArrayElements" $
      perBatchEnv
        (\batchSize ->
           replicateM (fromIntegral batchSize) (getByteArrayElements jbytes)
           >>= newIORef
        ) $
      \ref -> do
         arrays <- readIORef ref
         case arrays of
           x : xs -> do
             releaseByteArrayElements jbytes x
             writeIORef ref xs
           _ -> error "not enough arrays"
    , bench "getByteArrayRegion" $ nfIO $
         getByteArrayRegion jbytes 0 n bytes
    , bench "setByteArrayRegion" $ nfIO $
         setByteArrayRegion jbytes 0 n bytes
    ]
  where
    otherBenchmarks =
     [ bench "getObjectArrayElement" $
       perBatchEnvWithCleanup
         (\batchSize -> do
            pushLocalFrame (2 * fromIntegral batchSize)
            Box <$> newArray 100
         )
         (\_ _ -> void (popLocalFrame jnull)) $
         \(Box jObjectArray) -> do
           _ <- getObjectArrayElement (jObjectArray :: JObjectArray) 40 :: IO JObject
           return ()
     ]

    cleanArrays ref =
      readIORef ref >>= mapM_ (uncurry releaseByteArrayElements)

benchDirectBuffers :: Benchmark
benchDirectBuffers =
    bgroup "DirectBuffers" $ (`map` [128, 256, 512]) $ \bufferSize ->
    let n :: Num b => b
        n = fromIntegral (bufferSize :: Int) in
    env (Box <$> mallocArray n) $ \ ~(Box bytes) ->
    bgroup (show bufferSize)
    [ bench "getDirectBufferAddress" $
      perBatchEnvWithCleanup
        (\_ -> Box <$> newDirectByteBuffer bytes n)
        (\_ -> deleteLocalRef . unBox) $
      void . getDirectBufferAddress . unBox
    , bench "getDirectBufferCapacity" $
      perBatchEnvWithCleanup
        (\_ -> Box <$> newDirectByteBuffer bytes n)
        (\_ -> deleteLocalRef . unBox) $
      void . getDirectBufferCapacity . unBox
    ]

main :: IO ()
main = withJVM [] $ do
    Criterion.defaultMain
      [ benchCalls
      , benchRefs
      , benchNew
      , benchArrays
      , benchDirectBuffers
      ]
