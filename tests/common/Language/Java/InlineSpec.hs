{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- Test that inline-java produces code without warnings or errors.
{-# OPTIONS_GHC -dcore-lint -Wall -Werror #-}

module Language.Java.InlineSpec(spec) where

import Data.Int
import Foreign.JNI (JVMException)
import Language.Java
import Language.Java.Inline
import Test.Hspec
import System.Clock

type ObjectClass = 'Class "java.lang.Object"
type ListClass = 'Iface "java.util.List"

type JJObject = JObject
type List a = J (ListClass <> '[a])

imports "java.util.*"

spec :: Spec
spec = do
    describe "Java quasiquoter" $ do
      it "Can return ()" $ do
        t0 <- getTime Monotonic
        [java| { } |] :: IO ()
        t1 <- getTime Monotonic
        print (diffTimeSpec t1 t0)

      it "Evaluates simple expressions" $ do
        [java| 1 + 1 |] `shouldReturn` (2 :: Int32)

      it "Evaluates simple blocks" $ do
        [java| {
             int x = 1;
             int y = 2;
             return x + y;
           } |] `shouldReturn` (3 :: Int32)

      it "Supports antiquotation variables" $ do
        let x = 1 :: Int32
        [java| $x + 1 |] `shouldReturn` (2 :: Int32)

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
          obj :: List ObjectClass <- [java| new ArrayList() |]
          _ :: List ObjectClass <- [java| $obj |]
          return ()

      it "Supports multiple antiquotation variables" $ do
        let foo = 1 :: Int32
            bar = 2 :: Int32
        [java| $foo + $bar |] `shouldReturn` (3 :: Int32)

      it "Supports repeated antiquotation variables" $ do
        obj :: JObject <- [java| new Object() {} |]
        ([java| $obj.equals($obj) |] >>= reify) `shouldReturn` True

      it "Supports antiquotation variables in blocks" $ do
        let z = 1 :: Int32
        [java| { return $z + 1; } |] `shouldReturn` (2 :: Int32)

      it "Supports anonymous classes" $ do
        _ :: JObject <- [java| new Object() {} |]
        return ()

      it "Supports multiple anonymous classes" $ do
        [java| new Object() {}.equals(new Object() {}) |] `shouldReturn` False

      it "Supports using antiquotation variables in inner classes" $ do
        let foo = 1 :: Int32
        [java| { class Foo { int f() { return $foo; } }; } |] :: IO ()

      it "Supports import declarations" $ do
        -- Arrays comes from the java.util package.
        _ <- [java| Arrays.asList().toArray() |] :: IO JObjectArray
        return ()

      it "Supports anonymous functions" $ do
        [java| {
          List<Integer> xs = Arrays.asList(1, 2);
          Collections.sort(xs, (a, b) -> b.compareTo(a));
          return xs.get(0);
          } |] `shouldReturn` (2 :: Int32)

      it "Can be used inside brackets" $ do
        $([| let _x = 1 :: Int32 -- Named _x to avoid spurious "unused" warning
              in (+1) <$> [java| $_x |]
           |]) `shouldReturn` (2 :: Int32)

      it "Can throw checked exceptions" $ do
        ([java| { throw new InterruptedException(); } |] :: IO ())
          `shouldThrow` \(_ :: JVMException) -> True

      it "Type-checks generics" $ do
          obj :: List ('Class "java.lang.String") <-
            [java| new ArrayList<String>() |]
          _ :: List ('Class "java.lang.String") <- [java| $obj |]
          return ()

      it "Can access inner classes" $ do
          st :: J ('Class "java.lang.Thread$State") <-
            [java| Thread.State.NEW |]
          [java| $st == Thread.State.NEW |] `shouldReturn` True
