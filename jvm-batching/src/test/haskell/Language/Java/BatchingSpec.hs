{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Java.BatchingSpec where

import qualified Data.ByteString as BS
import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Language.Java
import Language.Java.Batching ()
import Language.Java.Inline
import Test.Hspec

spec :: Spec
spec = do
    let testrr :: (Eq a, Show a, Reify a, Reflect a) => a -> IO ()
        testrr x = (reflect x >>= reify) `shouldReturn` x
    describe "batching" $ do
      it "succeeds on empty vectors" $ do
        testrr (V.fromList [] :: V.Vector Text)
      it "succeeds on non-empty vectors" $ do
        testrr (V.fromList [1..10] :: V.Vector Int32)
      it "succeeds on vectors with nulls" $ do
        v <- [java| new Integer[] {1, null, 2, null } |]
        reify v `shouldReturn` (V.fromList [NotNull 1, Null, NotNull 2, Null] :: V.Vector (Nullable Int32))
      it "succeeds on vectors of vectors with nulls" $ do
        v <- [java| new Integer[][] {
            new Integer[] {},
            null,
            new Integer[] { 2, 3 },
            null,
            new Integer[] {}
          } |]
        reify v `shouldReturn` ((V.fromList
          [ NotNull V.empty
          , Null
          , NotNull (V.fromList [2, 3])
          , Null
          , NotNull V.empty
          ]) :: V.Vector (Nullable (V.Vector Int32)))
      it "succeeds on vectors of vectors" $ do
        let xs :: V.Vector (V.Vector Int32)
            xs = V.fromList (map V.fromList [ [i .. i + 4] | i <- [1,6..26] ])
        testrr xs
        testrr (V.empty `asTypeOf` xs)
        testrr $ V.map (V.map (Text.pack . show)) xs
        testrr $ V.map (V.map (BS.singleton . fromIntegral)) xs
        testrr (V.map V.convert xs :: V.Vector (VS.Vector Int32))
