{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.TypesSpec where

import Data.Int
import Data.Time (Day, LocalTime(..), addDays, addLocalTime, fromGregorian, midnight)
import qualified Data.Vector as V
import Language.Java
import Language.Java.Inline
import Language.Java.SQL.Time ()
import Test.Hspec


spec :: Spec
spec = do
    describe "batched reify" $ do
      it "succeeds on days" $ do
        let n = 10 :: Int32
        j <- [java| {
           java.sql.Date dates[] = new java.sql.Date[$n];
           for(int i=0;i<$n;i++)
             dates[i] = new java.sql.Date(((long)i) * 1000 * 60 * 60 * 24);
           return dates;
         }|]
        reify (j :: J (Interp (V.Vector Day)))
          `shouldReturn`
             V.fromList [ addDays (fromIntegral i) (fromGregorian 1970 1 1) | i <- [0..n - 1] ]

      it "succeeds on localtime" $ do
        let n = 10 :: Int32
        j <- [java| {
           java.sql.Timestamp ts[] = new java.sql.Timestamp[$n];
           for(int i=0;i<$n;i++)
             ts[i] = new java.sql.Timestamp(((long)i) * 1000 * 60 * 60 * 24);
           return ts;
         }|]
        reify (j :: J (Interp (V.Vector LocalTime)))
          `shouldReturn`
             V.fromList
               [ addLocalTime
                   (fromIntegral i * 60 * 60 * 24)
                   (LocalTime (fromGregorian 1970 1 1) midnight)
               | i <- [0..n - 1]
               ]


