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
      it "succeeds on vectors of vectors" $ do
        let xs :: V.Vector (V.Vector Int32)
            xs = V.fromList (map V.fromList [ [i .. i + 4] | i <- [1,6..26] ])
        testrr xs
        testrr (V.empty `asTypeOf` xs)
        testrr $ V.map (V.map (Text.pack . show)) xs
        testrr $ V.map (V.map (BS.singleton . fromIntegral)) xs
        testrr (V.map V.convert xs :: V.Vector (VS.Vector Int32))
