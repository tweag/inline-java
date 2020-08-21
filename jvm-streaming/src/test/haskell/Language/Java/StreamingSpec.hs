{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.StreamingSpec where

import Data.Int
import Data.IORef
import Foreign.JNI (runInAttachedThread)
import Language.Java
import Language.Java.Inline
import Language.Java.Streaming
import Test.Hspec
import Streaming (Stream, Of)
import qualified Streaming.Prelude as Streaming

spec :: Spec
spec = around_ runInAttachedThread $ do
    describe "iteration" $ do
      it "succeeds on empty lists" $ do
        vals <- reflect [1..0 :: Int32]
        iterator <- [java| java.util.Arrays.asList($vals).iterator() |]
        stream :: Stream (Of Int32) IO () <- reify iterator
        Streaming.toList_ stream `shouldReturn` [1..0 :: Int32]
      it "succeeds on singleton lists" $ do
        vals <- reflect [1 :: Int32]
        iterator <- [java| java.util.Arrays.asList($vals).iterator() |]
        stream :: Stream (Of Int32) IO () <- reify iterator
        Streaming.toList_ stream `shouldReturn` [1 :: Int32]
      it "succeeds on non-trivial lists" $ do
        vals <- reflect [1..10000 :: Int32]
        iterator <- [java| java.util.Arrays.asList($vals).iterator() |]
        stream :: Stream (Of Int32) IO () <- reify iterator
        Streaming.toList_ stream `shouldReturn` [1..10000 :: Int32]
    describe "streams" $ do
      it "have the property that reify . reflect == id" $ do
        iterator <- reflect (Streaming.each [1..10000] :: Stream (Of Int32) IO ())
        stream :: Stream (Of Int32) IO () <- reify iterator
        Streaming.toList_ stream `shouldReturn` [1..10000 :: Int32]
      it "don't redo effects" $ do
        ref <- newIORef (1 :: Int32)
        iterator <- reflect $
          Streaming.replicateM 10 $ atomicModifyIORef ref (\i -> (i + 1, i))
        stream :: Stream (Of Int32) IO () <- reify iterator
        Streaming.toList_ stream `shouldReturn` [1..10 :: Int32]
      it "are independent one from another" $ do
        ref <- newIORef (1 :: Int32)
        iterator <- reflectStreamWithBatching 1 $
          Streaming.replicateM 10 $ atomicModifyIORef ref (\i -> (i + 1, i))
        ref2 <- newIORef (21 :: Int32)
        iterator2 <- reflectStreamWithBatching 1 $
          Streaming.replicateM 10 $ atomicModifyIORef ref2 (\i -> (i + 1, i))
        stream :: Stream (Of Int32) IO () <- reifyStreamWithBatching 1 iterator
        stream2 :: Stream (Of Int32) IO () <- reifyStreamWithBatching 1 iterator2
        Streaming.toList_ stream `shouldReturn` [1..10 :: Int32]
        Streaming.toList_ stream2 `shouldReturn` [21..30 :: Int32]
