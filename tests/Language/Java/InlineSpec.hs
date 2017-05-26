{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- Test that inline-java produces code without warnings.
{-# OPTIONS_GHC -Wall -Werror #-}

module Language.Java.InlineSpec(spec) where

import Data.Int
import Foreign.JNI.Types (JObject, type (<>))
import Language.Java
import Language.Java.Inline
import Test.Hspec

type ObjectClass = 'Class "java.lang.Object"
type ListClass = 'Class "java.util.List"

type JJObject = JObject
type List a = J (ListClass <> '[a])

imports "java.util.*"

spec :: Spec
spec = do
    describe "Java quasiquoter" $ do
      it "Evaluates simple expressions" $ do
        ([java| 1 + 1 |] >>= reify) `shouldReturn` (2 :: Int32)

      it "Evaluates simple blocks" $ do
        ([java| {
             int x = 1;
             int y = 2;
             return x + y;
           } |] >>= reify) `shouldReturn` (3 :: Int32)

      it "Supports antiquotation variables" $ do
        let x = 1 :: Int32
        ([java| $x + 1 |] >>= reify) `shouldReturn` (2 :: Int32)

      describe "Type synonyms" $ do
        it "Supports top-level type synonym'ed antiquotation variables" $ do
          obj <- [java| new Object() {} |]
          let obj1 = obj :: JObject
          _ :: JObject <- [java| $obj1 |]
          return ()

        it "Supports inner type synonym'ed antiquotation variables" $ do
          obj <- [java| new Object() {} |]
          let obj1 = obj :: J ObjectClass
          _ :: J ObjectClass <- [java| $obj1 |]
          return ()

        it "Supports chained type synonym'ed antiquotation variables" $ do
          obj <- [java| new Object() {} |]
          let obj1 = obj :: JJObject
          _ :: JJObject <- [java| $obj1 |]
          return ()

        it "Supports parameterized type synonyms" $ do
          obj :: List ObjectClass <- [java| new Object() {} |]
          _ :: List ObjectClass <- [java| $obj |]
          return ()

      it "Supports multiple antiquotation variables" $ do
        let foo = 1 :: Int32
            bar = 2 :: Int32
        ([java| $foo + $bar |] >>= reify) `shouldReturn` (3 :: Int32)

      it "Supports repeated antiquotation variables" $ do
        obj :: JObject <- [java| new Object() {} |]
        ([java| $obj.equals($obj) |] >>= reify) `shouldReturn` True

      it "Supports antiquotation variables in blocks" $ do
        let z = 1 :: Int32
        ([java| { return $z + 1; } |] >>= reify) `shouldReturn` (2 :: Int32)

      it "Supports anonymous classes" $ do
        _ :: JObject <- [java| new Object() {} |]
        return ()

      it "Supports multiple anonymous classes" $ do
        ([java| new Object() {}.equals(new Object() {}) |] >>= reify) `shouldReturn` False

      it "Supports using antiquotation variables in inner classes" $ do
        let foo = 1 :: Int32
        ([java| { class Foo { int f() { return $foo; } }; return 1; } |]
          >>= reify) `shouldReturn` (1 :: Int32)

      it "Supports import declarations" $ do
        -- Arrays comes from the java.util package.
        _ <- [java| Arrays.asList() |] :: IO JObjectArray
        return ()
