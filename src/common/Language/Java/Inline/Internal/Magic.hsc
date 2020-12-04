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

module Language.Java.Inline.Internal.Magic
  ( DotClass(..)
  , JavaImport(..)
  , getDotClasses
  , mangleClassName
  ) where

import Control.Monad (forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Char (isAlphaNum)
import Data.Data
import Foreign.C.String (peekCString)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable
import qualified Language.Haskell.TH.Syntax as TH

#include "bctable.h"

-- | The bytecode corresponding to a java class
data DotClass = DotClass
  { className :: String
  , classBytecode :: ByteString
  }

data JavaImport = JavaImport String Integer
  deriving (Data, TH.Lift)

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

-- | Returns every class in the bytecode table
getDotClasses :: IO [DotClass]
getDotClasses = peek bctable >>= go
  where
    go :: Ptr DotClass -> IO [DotClass]
    go tbl
      | tbl == nullPtr = return []
      | otherwise = do
        dcs_ptr <- #{peek struct inline_java_pack, classes} tbl
        tbl_sz <- #{peek struct inline_java_pack, size} tbl
        head_dcs <- forM [0..(tbl_sz-1)] $ \i ->
          peekDotClass (dcs_ptr `plusPtr` (i * dc_sz))
        tail_dcs <- #{peek struct inline_java_pack, next} tbl >>= go
        return $ head_dcs ++ tail_dcs
    dc_sz :: Int
    dc_sz = #{size struct inline_java_dot_class}
