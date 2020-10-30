{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language.JavaSpec where

import Control.Concurrent (runInBoundThread)
import Control.Monad (join)
import Data.Int
import qualified Data.Text as Text
import Data.Text (Text, isInfixOf)
import Data.Text.Arbitrary ()
import Data.Text.Encoding (encodeUtf8)
import Foreign.JNI (getArrayLength, runInAttachedThread)
import qualified Foreign.JNI.String as JNI
import Language.Java
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = around_ (runInBoundThread . runInAttachedThread) $ do
    describe "callStatic" $ do
      it "can call double-returning static functions" $ do
        jstr <- reflect ("1.2345" :: Text)
        callStatic "java.lang.Double" "parseDouble" jstr
          `shouldReturn` (1.2345 :: Double)

      it "can call int-returning static functions" $ do
        jstr <- reflect ("12345" :: Text)
        callStatic "java.lang.Integer" "parseInt" jstr
          `shouldReturn` (12345 :: Int32)

      it "can call String-returning static functions" $ do
        jstr <-
          callStatic
            "java.lang.Integer"
            "toString"

            (12345 :: Int32)
        reify jstr `shouldReturn` ("12345" :: Text)

      it "can get static fields" $ do
        getStaticField "java.lang.Math" "PI"
          `shouldReturn` (pi :: Double)

      it "can get enum values" $ do
        monday :: J ('Class "java.time.DayOfWeek") <-
          getStaticField "java.time.DayOfWeek" "MONDAY"
        call monday "getValue"
          `shouldReturn` (1 :: Int32)

      it "short doesn't under- or overflow" $ do
        maxshort <- reflect (Text.pack (show (maxBound :: Int16)))
        minshort <- reflect (Text.pack (show (minBound :: Int16)))
        callStatic "java.lang.Short" "parseShort" maxshort
          `shouldReturn` (maxBound :: Int16)
        callStatic "java.lang.Short" "parseShort" minshort
          `shouldReturn` (minBound :: Int16)

      it "int doesn't under- or overflow" $ do
        maxint <- reflect (Text.pack (show (maxBound :: Int32)))
        minint <- reflect (Text.pack (show (minBound :: Int32)))
        callStatic "java.lang.Integer" "parseInt" maxint
          `shouldReturn` (maxBound :: Int32)
        callStatic "java.lang.Integer" "parseInt" minint
          `shouldReturn` (minBound :: Int32)

      it "long doesn't under- or overflow" $ do
        maxlong <- reflect (Text.pack (show (maxBound :: Int64)))
        minlong <- reflect (Text.pack (show (minBound :: Int64)))
        callStatic "java.lang.Long" "parseLong" maxlong
          `shouldReturn` (maxBound :: Int64)
        callStatic "java.lang.Long" "parseLong" minlong
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
        j <- new i :: IO (J ('Class "java.lang.Integer"))
        reify (unsafeCast j) `shouldReturn` (fromIntegral i :: Int64)

      it "correctly encodes characters outside ASCII range" $ do
        let incorrect :: Text = "आपकी उम्र लंबी हो और आप समृद्ध बने 👋"
        let correct = Text.replace "👋" "🖖" incorrect
        hello <- reflect ("👋" :: Text)
        spock <- reflect ("🖖" :: Text)
        jincorrect <- reflect incorrect
        jcorrect :: JString <- call jincorrect "replaceFirst" hello spock
        reify jcorrect `shouldReturn` correct

    describe "strings" $ do
      -- It is known that the current implementation of JNI.String does not support embedded NULL
      prop "correctly convert back and forth from Prelude.String to JNI.String" $
        \s -> (notElem '\NUL' s) ==> (s === (JNI.toChars . JNI.fromChars) s)
      prop "correctly convert back and forth from JNI.String to Prelude.String" $
        \t -> (not $ isInfixOf "\NUL" t) ==>
          let s = JNI.fromByteString $ encodeUtf8 t
          in s === (JNI.fromChars . JNI.toChars) s

      prop "correctly convert back and forth from Data.Text to java.lang.String" $
        \(textBefore :: Text) -> ioProperty $ do
          withLocalRef (reflect textBefore) $ \jTextBefore ->
            -- call substring() to force returning a new object
            withLocalRef (call jTextBefore "substring" (0 :: Int32)) $ \(jTextAfter :: JString) -> do
              textAfter :: Text <- reify jTextAfter
              return $ textBefore === textAfter

      prop "correctly convert back and forth from java.lang.String to Data.Text" $
        \(textBefore :: Text) -> ioProperty $ do
          withLocalRef (reflect textBefore) $ \jTextBefore ->
            withLocalRef (join $ (reflect . Text.copy) <$> reify jTextBefore) $ \jTextAfter -> do
              isEqual :: Bool <- call jTextBefore "equals" (upcast jTextAfter)
              return $ isEqual === True
