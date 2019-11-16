{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language.JavaSpec where

import Data.Int
import qualified Data.Text as Text
import Data.Text (Text)
import Foreign.JNI (getArrayLength)
import Language.Java
import Test.Hspec

spec :: Spec
spec = do
    describe "callStatic" $ do
      it "can call double-returning static functions" $ do
        jstr <- reflect ("1.2345" :: Text)
        callStatic "java.lang.Double" "parseDouble" With1Args jstr
          `shouldReturn` (1.2345 :: Double)

      it "can call int-returning static functions" $ do
        jstr <- reflect ("12345" :: Text)
        callStatic "java.lang.Integer" "parseInt" With1Args jstr
          `shouldReturn` (12345 :: Int32)

      it "can call String-returning static functions" $ do
        jstr <-
          callStatic
            "java.lang.Integer"
            "toString"
            With1Args
            (12345 :: Int32)
        reify jstr `shouldReturn` ("12345" :: Text)

      it "can get static fields" $ do
        getStaticField "java.lang.Math" "PI"
          `shouldReturn` (pi :: Double)

      it "can get enum values" $ do
        monday :: J ('Class "java.time.DayOfWeek") <-
          getStaticField "java.time.DayOfWeek" "MONDAY"
        call monday "getValue" With0Args
          `shouldReturn` (1 :: Int32)

      it "short doesn't under- or overflow" $ do
        maxshort <- reflect (Text.pack (show (maxBound :: Int16)))
        minshort <- reflect (Text.pack (show (minBound :: Int16)))
        callStatic "java.lang.Short" "parseShort" With1Args maxshort
          `shouldReturn` (maxBound :: Int16)
        callStatic "java.lang.Short" "parseShort" With1Args minshort
          `shouldReturn` (minBound :: Int16)

      it "int doesn't under- or overflow" $ do
        maxint <- reflect (Text.pack (show (maxBound :: Int32)))
        minint <- reflect (Text.pack (show (minBound :: Int32)))
        callStatic "java.lang.Integer" "parseInt" With1Args maxint
          `shouldReturn` (maxBound :: Int32)
        callStatic "java.lang.Integer" "parseInt" With1Args minint
          `shouldReturn` (minBound :: Int32)

      it "long doesn't under- or overflow" $ do
        maxlong <- reflect (Text.pack (show (maxBound :: Int64)))
        minlong <- reflect (Text.pack (show (minBound :: Int64)))
        callStatic "java.lang.Long" "parseLong" With1Args maxlong
          `shouldReturn` (maxBound :: Int64)
        callStatic "java.lang.Long" "parseLong" With1Args minlong
          `shouldReturn` (minBound :: Int64)

    describe "newArray" $ do
      it "Supports object arrays" $ do
        jxs <- newArray 10
        getArrayLength (jxs :: J ('Array ('Class "java.lang.Object")))
          `shouldReturn` 10

      it "supports generics" $ do
        jxs <- newArray 10
        getArrayLength (jxs ::
            J ('Array ('Class "java.util.List" <> '[ 'Class "java.lang.Long"]))
            )
          `shouldReturn` 10

    describe "reify" $ do
      -- Applications need extra conversions if the following doesn't hold.
      it "can get Integer when Long is expected" $ do
        let i = maxBound :: Int32
        j <- new With1Args i :: IO (J ('Class "java.lang.Integer"))
        reify (unsafeCast j) `shouldReturn` (fromIntegral i :: Int64)
