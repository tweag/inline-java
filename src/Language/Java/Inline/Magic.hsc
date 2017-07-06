-- | Internal module defining some magic, kept separate from the rest, that
-- depends on compiler internals.

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-ds #-}

module Language.Java.Inline.Magic
  ( JavaImport(..)
  , forEachDotClass
  , mangleClassName
  , qqMarker
  ) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Char (isAlphaNum)
import Data.Data
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (Nat, Symbol)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Java (Coercible)

#include "bctable.h"

-- | Produces a Java class name from a package and a module name.
mangleClassName :: String -> String -> String
mangleClassName pkgname modname = concat
    [ "Inline__"
    , filter isAlphaNum pkgname
    , "_"
    , map (\case '.' -> '_'; x -> x) modname
    ]

data JavaImport = JavaImport String Integer
  deriving (Typeable, Data, TH.Lift)

data LinkedList

foreign import capi unsafe "&inline_java_bctable" bctable :: Ptr (Ptr LinkedList)

-- | Runs the given function for every class in the bytecode table.
--
-- The function is given the name of the class and the bytecode.
--
forEachDotClass :: (String -> ByteString -> IO ()) -> IO ()
forEachDotClass f = peek bctable >>= go
  where
    go :: Ptr LinkedList -> IO ()
    go llnode | llnode == nullPtr = return ()
              | otherwise = do
      count <- #{peek struct inline_java_linked_list, ij_ll_dc_count} llnode
      pdc <- #{peek struct inline_java_linked_list, ij_ll_dc} llnode
      forM_ [ plusPtr pdc (#{size struct inline_java_dot_class} * i)
            | i <- [0 .. fromIntegral (count :: CInt) - 1]
            ] $ \pdci -> do
        name <- #{peek struct inline_java_dot_class, ij_dc_class_name} pdci
                 >>= peekCString
        bclen <- #{peek struct inline_java_dot_class, ij_dc_bc_count} pdci
        pbc <- #{peek struct inline_java_dot_class, ij_dc_class_bytecode} pdci
        bc <- unsafePackCStringLen (pbc, fromIntegral (bclen :: CInt))
        f name bc
      #{peek struct inline_java_linked_list, ij_ll_next} llnode >>= go

-- | A function to mark the occurrence of java quasiquotations
qqMarker
  :: forall
     -- k                -- the kind variable shows up in Core
     (args_tys :: k)     -- JType's of arguments
     tyres               -- JType of result
     (input :: Symbol)   -- input string of the quasiquoter
     (mname :: Symbol)   -- name of the method to generate
     (antiqs :: Symbol)  -- antiquoted variables as a comma-separated list
     (line :: Nat)       -- line number of the quasiquotation
     args_tuple          -- uncoerced argument types
     b.                  -- uncoerced result type
     (Coercibles args_tuple args_tys, Coercible b tyres, HasCallStack)
  => Proxy input
  -> Proxy mname
  -> Proxy antiqs
  -> Proxy line
  -> args_tuple
  -> Proxy args_tys
  -> IO b
  -> IO b
qqMarker _ _ _ _ _ = withFrozenCallStack $
    error $ "Please, pass -fplugin=Language.Java.Inline.Plugin"
            ++ " to ghc when building this module."

class Coercibles xs (tys :: k) | xs -> tys
instance Coercibles () ()
instance (Coercible x ty, Coercibles xs tys) => Coercibles (x, xs) '(ty, tys)
