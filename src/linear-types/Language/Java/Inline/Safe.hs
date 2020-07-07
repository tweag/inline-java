-- | = Inline Java quasiquotation with a linear interface
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
-- import Language.Java.Inline.Safe
-- import Language.Java.Safe
--
-- newtype Object = Object ('J' (''Class' "java.lang.Object"))
-- instance 'Coercible' Object
--
-- clone :: Object ->. IO Object
-- clone obj = [java| $obj.clone() |]
--
-- equals :: Object ->. Object ->. IO Bool
-- equals obj1 obj2 = [java| $obj1.equals($obj2) |]
--
-- ...
-- @

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Inline.Safe
  ( java
  , Java.imports
  , Java.loadJavaWrappers
  ) where

import qualified Control.Monad.IO.Class.Linear as Linear
import qualified Control.Monad.Linear as Linear
import Foreign.JNI.Safe (liftPreludeIO)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import qualified Language.Java.Inline.Internal as Java
import qualified Language.Java.Inline.Internal.QQMarker.Safe as Safe
import qualified Language.Java.Safe as Safe
import qualified Prelude.Linear as Linear

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
java = Java.javaWithConfig Java.QQConfig
    { Java.qqMarker = 'Safe.qqMarker
    , Java.qqCallStatic = \qargs ->
        let (args, tolist) = splitAt 2 qargs
         in -- XXX: We need to explicitly use the linear ($) so GHC is satisfied
            -- that the argument is going to be used linearly in the variadic
            -- function.
            TH.appE
              (foldl
                (flip TH.appE)
                (TH.appsE (TH.varE 'Safe.callStatic : args))
                (map (\q -> [| (Linear.$ $q) |]) tolist)
              )
              [| Safe.End |]
    , Java.qqWrapMarker = \qExp ->
        [| liftPreludeIO loadJavaWrappers Linear.>> $qExp |]
    }
