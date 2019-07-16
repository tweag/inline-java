{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- Test that inline-java produces code without warnings or errors.
{-# OPTIONS_GHC -dcore-lint -Wall -Werror #-}

module Language.Java.Inline.SafeSpec(spec) where

import Control.Monad.Linear
import Data.Int
import Foreign.JNI (JVMException)
import Foreign.JNI.Safe
import Language.Java.Safe
import Language.Java.Inline.Safe
import Prelude hiding ((>>=), (>>))
import Test.Hspec

type ObjectClass = 'Class "java.lang.Object"
type ListClass = 'Iface "java.util.List"

type JJObject = JObject
type List a = J (ListClass <> '[a])

imports "java.util.*"

spec :: Spec
spec = do
    describe "Linear Java quasiquoter" $ do
      it "Can return ()" $ do
        withLocalFrame_ [java| { } |]

      it "Evaluates simple expressions" $ do
        withLocalFrame [java| 1 + 1 |] `shouldReturn` (2 :: Int32)

      it "Evaluates simple blocks" $ do
        withLocalFrame [java| {
             int x = 1;
             int y = 2;
             return x + y;
           } |] `shouldReturn` (3 :: Int32)

      it "Supports antiquotation variables" $ do
        let x = 1 :: Int32
        withLocalFrame [java| $x + 1 |] `shouldReturn` (2 :: Int32)

      describe "Type synonyms" $ do
        it "Supports top-level type synonym'ed antiquotation variables" $
          withLocalFrame_ $ [java| new Object() {} |]
          >>= \(obj :: JObject) ->
            [java| $obj |] >>= \jobj1 ->
            deleteLocalRef (jobj1 :: JObject)

        it "Supports inner type synonym'ed antiquotation variables" $
          withLocalFrame_ $ [java| new Object() {} |]
          >>= \(obj :: J ObjectClass) ->
            [java| $obj |] >>= \(obj1 :: J ObjectClass) ->
            deleteLocalRef obj1

        it "Supports chained type synonym'ed antiquotation variables" $
          withLocalFrame_ $ [java| new Object() {} |]
          >>= \(obj :: JJObject) ->
            [java| $obj |] >>= \(obj1 :: JJObject) ->
            deleteLocalRef obj1

        it "Supports parameterized type synonyms" $
          withLocalFrame_ $ [java| new ArrayList() |]
          >>= \(obj :: List ObjectClass) ->
            [java| $obj |] >>= \(obj1 :: List ObjectClass) ->
            deleteLocalRef obj1

      it "Supports multiple antiquotation variables" $ do
        let foo = 1 :: Int32
            bar = 2 :: Int32
        withLocalFrame [java| $foo + $bar |] `shouldReturn` (3 :: Int32)

      it "Supports repeated antiquotation variables" $
        withLocalFrame
          ([java| new Object() {} |] >>= \(obj :: JObject) ->
             ([java| $obj.equals($obj) |] >>= reify_)
          )
          `shouldReturn` True

      it "Supports antiquotation variables in blocks" $ do
        let z = 1 :: Int32
        withLocalFrame [java| { return $z + 1; } |]
          `shouldReturn` (2 :: Int32)

      it "Supports anonymous classes" $ do
        withLocalFrame_ $
          [java| new Object() {} |] >>= \(obj :: JObject) ->
            deleteLocalRef obj

      it "Supports multiple anonymous classes" $ do
        withLocalFrame [java| new Object() {}.equals(new Object() {}) |]
          `shouldReturn` False

      it "Supports using antiquotation variables in inner classes" $ do
        let foo = 1 :: Int32
        withLocalFrame_
          [java| { class Foo { int f() { return $foo; } }; } |] :: IO ()

      it "Supports import declarations" $
        -- Arrays comes from the java.util package.
        withLocalFrame_ $
          [java| Arrays.asList().toArray() |] >>= \(obj :: JObjectArray) ->
          deleteLocalRef obj

      it "Supports anonymous functions" $ do
        withLocalFrame [java| {
          List<Integer> xs = Arrays.asList(1, 2);
          Collections.sort(xs, (a, b) -> b.compareTo(a));
          return xs.get(0);
          } |] `shouldReturn` (2 :: Int32)

      it "Can be used inside brackets" $
        $([| let _x = 1 :: Int32 -- Named _x to avoid spurious "unused" warning
              in (+1) Prelude.<$> withLocalFrame [java| $_x |]
          |]) `shouldReturn` (2 :: Int32)

      it "Can throw checked exceptions" $ do
        withLocalFrame_ [java| { throw new InterruptedException(); } |]
          `shouldThrow` \(_ :: JVMException) -> True

      it "Type-checks generics" $
          withLocalFrame_ $
            [java| new ArrayList<String>() |]
            >>= \(obj :: List ('Class "java.lang.String")) ->
            [java| $obj |] >>= \(obj1 :: List ('Class "java.lang.String")) ->
              deleteLocalRef obj1

      it "Can access inner classes" $
          withLocalFrame
            ([java| Thread.State.NEW |]
             >>= \(st :: J ('Class "java.lang.Thread$State")) ->
             [java| $st == Thread.State.NEW |]
            )
            `shouldReturn` True
