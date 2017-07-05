-- | Internal module defining some magic, kept separate from the rest, that
-- depends on compiler internals.

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Java.Inline.Magic
  ( DotClass(..)
  , forEachDotClass
  ) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Data
import Data.Word
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable
import qualified Language.Haskell.TH.Syntax as TH

#include "bctable.h"

data DotClass = DotClass
    { className :: String
    , classBytecode :: [Word8]
    }
  deriving (Typeable, Data)

instance TH.Lift DotClass where
  lift DotClass{..} =
      [| DotClass
           $(TH.lift className)
           $(TH.lift classBytecode)
       |]

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
