{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main as Criterion
import Control.Monad (replicateM_)
import Data.Int
import Data.Singletons (SomeSing(..))
import qualified Foreign.Concurrent as Concurrent
import qualified Foreign.ForeignPtr as ForeignPtr
import Foreign.JNI
import Foreign.Marshal.Alloc (mallocBytes, finalizerFree)
import Language.Java

newtype BoxObject = BoxObject JObject
newtype BoxClass = BoxClass JClass

-- Not much sense in deepseq'ing foreign pointers. But needed for call to 'env'
-- below.
instance NFData BoxObject where rnf (BoxObject (J fptr)) = fptr `seq` ()
instance NFData BoxClass where rnf (BoxClass (J fptr)) = fptr `seq` ()

jabs :: Int32 -> IO Int32
jabs x = callStatic "java.lang.Math" "abs" [coerce x]

jniAbs :: JClass -> JMethodID -> Int32 -> IO Int32
jniAbs klass method x = callStaticIntMethod klass method [coerce x]

intValue :: Int32 -> IO Int32
intValue x = do
    jx <- reflect x
    call jx "intValue" []

compareTo :: Int32 -> Int32 -> IO Int32
compareTo x y = do
    jx <- reflect x
    jy <- reflect y
    call jx "compareTo" [coerce jy]

incrHaskell :: Int32 -> IO Int32
incrHaskell x = return (x + 1)

foreign import ccall unsafe getpid :: IO Int

benchCalls :: Benchmark
benchCalls =
    env ini $ \ ~(BoxClass klass, method) ->
      bgroup "Calls"
      [ bgroup "Java calls"
        [ bench "static method call: unboxed single arg / unboxed return" $ nfIO $ jabs 1
        , bench "jni static method call: unboxed single arg / unboxed return" $ nfIO $ jniAbs klass method 1
        , bench "method call: no args / unboxed return" $ nfIO $ intValue 1
        , bench "method call: boxed single arg / unboxed return" $ nfIO $ compareTo 1 1
        , bench "local frame / 1 reference" $ nfIO $ do
            pushLocalFrame 30
            _ <- newLocalRef klass
            popLocalFrame jnull
            return ()
        , bench "delete 1 local ref" $ nfIO $
            newLocalRef klass >>= deleteLocalRef
        , bench "local frame / 30 references" $ nfIO $ do
            pushLocalFrame 30
            replicateM_ 30 $ newLocalRef klass
            popLocalFrame jnull
            return ()
        , bench "delete 30 local refs" $ nfIO $
            replicateM_ 30 $ newLocalRef klass >>= deleteLocalRef
        ]
      , bgroup "Haskell calls"
        [ bench "incr haskell" $ nfIO $ incrHaskell 1
        , bench "ffi haskell" $ nfIO $ getpid
        ]
      ]
  where
    ini = do
      klass <- findClass (referenceTypeName (SClass "java/lang/Math"))
      method <- getStaticMethodID klass "abs" (methodSignature [SomeSing (sing :: Sing ('Prim "int"))] (SPrim "int"))
      return (BoxClass klass, method)

benchRefs :: Benchmark
benchRefs =
    env (BoxObject <$> new []) $ \ ~(BoxObject jobj) ->
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
    ]

main :: IO ()
main = withJVM [] $ do
    Criterion.defaultMain [benchCalls, benchRefs]
