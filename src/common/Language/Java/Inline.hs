-- | = Inline Java quasiquotation
--
-- See the
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation GHC manual>
-- for an introduction to quasiquotation. The quasiquoter exported in this
-- module allows embedding arbitrary Java expressions and blocks of statements
-- inside Haskell code. You can call any Java method and define arbitrary inline
-- code using Java syntax. No FFI required.
--
-- Here is the same example as in "Language.Java", but with inline Java calls:
--
-- @
-- {&#45;\# LANGUAGE DataKinds \#&#45;}
-- {&#45;\# LANGUAGE QuasiQuotes \#&#45;}
-- module Object where
--
-- import Language.Java as J
-- import Language.Java.Inline
--
-- newtype Object = Object ('J' (''Class' "java.lang.Object"))
-- instance 'Coercible' Object
--
-- clone :: Object -> IO Object
-- clone obj = [java| $obj.clone() |]
--
-- equals :: Object -> Object -> IO Bool
-- equals obj1 obj2 = [java| $obj1.equals($obj2) |]
--
-- ...
-- @

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Java.Inline
  ( java
  , imports
  , loadJavaWrappers
  ) where

import Language.Haskell.TH.Quote
import Language.Java
import Language.Java.Inline.Internal
import qualified Language.Java.Inline.QQMarker as QQMarker

-- | Java code quasiquoter. Example:
--
-- @
-- imports "javax.swing.JOptionPane"
--
-- hello :: IO ()
-- hello = do
--     message <- reflect ("Hello World!" :: Text)
--     [java| JOptionPane.showMessageDialog(null, $message) |]
-- @
--
-- A quasiquote is a snippet of Java code. The code is assumed to be a block
-- (sequence of statements) if the first non whitespace character is a @{@
-- (curly brace) character. Otherwise it's parsed as an expression. Variables
-- with an initial @$@ (dollar) sign are allowed. They have a special meaning:
-- they stand for antiqotation variables (think of them as format specifiers in
-- printf format string). An antiquotation variable @$foo@ is well-scoped if
-- there exists a variable with the name @foo@ in the Haskell context of the
-- quasiquote, whose type is 'Coercible' to a Java primitive or reference type.
--
java :: QuasiQuoter
java = javaWithConfig QQConfig
    { qqMarker = 'QQMarker.qqMarker
    , qqCallStatic = 'callStatic
    , qqCoerce = 'coerce
    , qqWrapMarker = \qExp -> [| loadJavaWrappers >> $qExp |]
    }
