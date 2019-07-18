{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- Test that inline-java produces code without warnings or errors.
--
-- -O0 is necessary to workaround a bug in ghc-8.9.20190613
-- https://github.com/tweag/ghc/issues/384
{-# OPTIONS_GHC -dcore-lint -Wall -Werror -Wno-name-shadowing -O0 #-}

module Language.Java.Inline.SafeSpec(spec) where

import qualified Control.Monad.Linear.Builder as Linear
import Data.Int
import Foreign.JNI (JVMException)
import Foreign.JNI.Safe
import Language.Java.Safe
import Language.Java.Inline.Safe
import qualified Control.Monad.Builder as Prelude
import Prelude hiding ((>>=), (>>))
import Test.Hspec

type ObjectClass = 'Class "java.lang.Object"
type ListClass = 'Iface "java.util.List"

type JJObject = JObject
type List a = J (ListClass <> '[a])

imports "java.util.*"

spec :: Spec
spec =
    let Prelude.Builder{..} = Prelude.monadBuilder in do
    describe "Linear Java quasiquoter" $ do
      it "Can return ()" $
        withLocalFrame_ [java| { } |]

      it "Evaluates simple expressions" $
        withLocalFrame [java| 1 + 1 |] `shouldReturn` (2 :: Int32)

      it "Evaluates simple blocks" $
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
          withLocalFrame_ $
            let Linear.Builder{..} = Linear.monadBuilder in do
            obj :: JObject <- [java| new Object() {} |]
            jobj1 <- [java| $obj |]
            deleteLocalRef (jobj1 :: JObject)

        it "Supports inner type synonym'ed antiquotation variables" $
          withLocalFrame_ $
            let Linear.Builder{..} = Linear.monadBuilder in do
            obj :: J ObjectClass <- [java| new Object() {} |]
            obj1 :: J ObjectClass <- [java| $obj |]
            deleteLocalRef obj1

        it "Supports chained type synonym'ed antiquotation variables" $
          withLocalFrame_ $
            let Linear.Builder{..} = Linear.monadBuilder in do
            obj :: JJObject <- [java| new Object() {} |]
            obj1 :: JJObject <- [java| $obj |]
            deleteLocalRef obj1

        it "Supports parameterized type synonyms" $
          withLocalFrame_ $
            let Linear.Builder{..} = Linear.monadBuilder in do
            obj :: List ObjectClass <- [java| new ArrayList() |]
            obj1 :: List ObjectClass <- [java| $obj |]
            deleteLocalRef obj1

      it "Supports multiple antiquotation variables" $ do
        let foo = 1 :: Int32
            bar = 2 :: Int32
        withLocalFrame [java| $foo + $bar |] `shouldReturn` (3 :: Int32)

      it "Supports repeated antiquotation variables" $
        withLocalFrame (
          let Linear.Builder{..} = Linear.monadBuilder in do
          obj :: JObject <- [java| new Object() {} |]
          [java| $obj.equals($obj) |] >>= reify_
         ) `shouldReturn` True

      it "Supports antiquotation variables in blocks" $ do
        let z = 1 :: Int32
        withLocalFrame [java| { return $z + 1; } |]
          `shouldReturn` (2 :: Int32)

      it "Supports anonymous classes" $
        withLocalFrame_ $
          let Linear.Builder{..} = Linear.monadBuilder in do
          obj :: JObject <- [java| new Object() {} |]
          deleteLocalRef obj

      it "Supports multiple anonymous classes" $
        withLocalFrame [java| new Object() {}.equals(new Object() {}) |]
          `shouldReturn` False

      it "Supports using antiquotation variables in inner classes" $ do
        let foo = 1 :: Int32
        withLocalFrame_
          [java| { class Foo { int f() { return $foo; } }; } |] :: IO ()

      it "Supports import declarations" $
        -- Arrays comes from the java.util package.
        withLocalFrame_ $
          let Linear.Builder{..} = Linear.monadBuilder in do
          obj :: JObjectArray <- [java| Arrays.asList().toArray() |]
          deleteLocalRef obj

      it "Supports anonymous functions" $
        withLocalFrame [java| {
          List<Integer> xs = Arrays.asList(1, 2);
          Collections.sort(xs, (a, b) -> b.compareTo(a));
          return xs.get(0);
          } |] `shouldReturn` (2 :: Int32)

      it "Can be used inside brackets" $
        $([| let _x = 1 :: Int32 -- Named _x to avoid spurious "unused" warning
              in (+1) Prelude.<$> withLocalFrame [java| $_x |]
          |]) `shouldReturn` (2 :: Int32)

      it "Can throw checked exceptions" $
        withLocalFrame_ [java| { throw new InterruptedException(); } |]
          `shouldThrow` \(_ :: JVMException) -> True

      it "Type-checks generics" $
          withLocalFrame_ $
            let Linear.Builder{..} = Linear.monadBuilder in do
            obj :: List ('Class "java.lang.String") <- [java| new ArrayList<String>() |]
            obj1 :: List ('Class "java.lang.String") <- [java| $obj |]
            deleteLocalRef obj1

      it "Can access inner classes" $
          (withLocalFrame $
            let Linear.Builder{..} = Linear.monadBuilder in do
            st :: J ('Class "java.lang.Thread$State") <- [java| Thread.State.NEW |]
            [java| $st == Thread.State.NEW |]
           ) `shouldReturn` True
