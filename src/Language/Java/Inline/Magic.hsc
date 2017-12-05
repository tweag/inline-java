-- | Internal module defining some magic, kept separate from the rest, that
-- depends on compiler internals.

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( DotClass(..)
  , JavaImport(..)
  , forEachDotClass
  , mangleClassName
  , qqMarker
  ) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Char (isAlphaNum)
import Data.Data
import Foreign.C.String (peekCString)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (Nat, Symbol)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Java (Coercible, Ty)

#include "bctable.h"

-- | The bytecode corresponding to a java class
data DotClass = DotClass
  { className :: String
  , classBytecode :: ByteString
  }

data JavaImport = JavaImport String Integer
  deriving (Typeable, Data, TH.Lift)

-- | Produces a Java class name from a package and a module name.
mangleClassName :: String -> String -> String
mangleClassName pkgname modname = concat
    [ "Inline__"
    , filter isAlphaNum pkgname
    , "_"
    , map (\case '.' -> '_'; x -> x) modname
    ]

foreign import capi unsafe "&inline_java_bctable" bctable :: Ptr (Ptr DotClass)

peekDotClass :: Ptr DotClass -> IO DotClass
peekDotClass ptr = do
    sz <- #{peek struct inline_java_dot_class, bytecode_sz} ptr
    bc <- #{peek struct inline_java_dot_class, bytecode} ptr
    DotClass
      <$> (#{peek struct inline_java_dot_class, name} ptr >>= peekCString)
      <*> (BS.unsafePackCStringLen (bc, fromIntegral (sz :: CSize)))

-- | Runs the given function for every class in the bytecode table.
forEachDotClass :: (DotClass -> IO ()) -> IO ()
forEachDotClass f = peek bctable >>= go
  where
    go :: Ptr DotClass -> IO ()
    go tbl
      | tbl == nullPtr = return ()
      | otherwise = do
        dcs_ptr <- #{peek struct inline_java_pack, classes} tbl
        tbl_sz <- #{peek struct inline_java_pack, size} tbl
        forM_ [0..(tbl_sz-1)] $ \i -> do
          let dc_sz = #{size struct inline_java_dot_class}
          f =<< peekDotClass (dcs_ptr `plusPtr` (i * dc_sz))
        #{peek struct inline_java_pack, next} tbl >>= go

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
     (tyres ~ Ty b, Coercibles args_tuple args_tys, Coercible b, HasCallStack)
  => Proxy input
  -> Proxy mname
  -> Proxy antiqs
  -> Proxy line
  -> args_tuple
  -> Proxy args_tys
  -> IO b
  -> IO b
qqMarker _ _ _ _ _ = withFrozenCallStack $
    error $
      "Please pass -fplugin=Language.Java.Inline.Plugin"
      ++ " to ghc when building this module."

class Coercibles xs (tys :: k) | xs -> tys
instance Coercibles () ()
instance (ty ~ Ty x, Coercible x, Coercibles xs tys) => Coercibles (x, xs) '(ty, tys)
