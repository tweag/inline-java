-- | Internal module defining some magic, kept separate from the rest, that
-- depends on compiler internals.

module Language.Java.Inline.Internal.Magic where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Foreign.C.String (peekCString)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable

#include "bctable.h"

-- | The bytecode corresponding to a java class
data DotClass = DotClass
  { className :: String
  , classBytecode :: ByteString
  }

peekDotClass :: Ptr DotClass -> IO DotClass
peekDotClass ptr = do
    sz <- #{peek struct inline_java_dot_class, bytecode_sz} ptr
    bc <- #{peek struct inline_java_dot_class, bytecode} ptr
    DotClass
      <$> (#{peek struct inline_java_dot_class, name} ptr >>= peekCString)
      <*> (BS.unsafePackCStringLen (bc, fromIntegral (sz :: CSize)))
