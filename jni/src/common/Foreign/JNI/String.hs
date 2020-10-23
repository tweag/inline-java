-- | JNI strings. Like C strings and unlike 'Data.ByteString.ByteString', these
-- are null-terminated. Unlike C strings, each character is (multi-byte) encoded
-- as UTF-8. Unlike UTF-8, embedded NULL characters are encoded as two bytes and
-- the four-byte UTF-8 format for characters is not recognized. A custom
-- encoding is used instead. See
-- <http://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#modified_utf_8_strings>
-- for more details.
--
-- /NOTE:/ the current implementation does not support embedded NULL's and
-- four-byte characters.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}

module Foreign.JNI.String
  ( String
  , toChars
  , fromChars
  , fromByteString
  , unsafeFromByteString
  , toByteString
  , withString
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Foreign.C.String (CString)
import qualified GHC.Foreign as GHC
import qualified GHC.IO.Encoding as GHC
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Prelude
import Prelude hiding (String)

-- A JNI string is represented as a NUL terminated bytestring in UTF-8 encoding.
newtype String = String ByteString
  deriving (Eq, Ord)

instance Show String where
  show str = show (toChars str)

instance IsString String where
  fromString str = fromChars str

fromChars :: Prelude.String -> String
{-# INLINE [0] fromChars #-}
fromChars str = unsafeDupablePerformIO $
    GHC.withCString GHC.utf8 str $ \cstr -> do
      -- we need to copy the trailing NUL
      len <- BS.c_strlen cstr
      String <$> BS.packCStringLen (cstr, fromIntegral len + 1)

toChars :: String -> Prelude.String
toChars (String bs) = unsafeDupablePerformIO $ BS.unsafeUseAsCString bs $ GHC.peekCString GHC.utf8

withString :: String -> (CString -> IO a) -> IO a
withString (String bs) f = BS.unsafeUseAsCString bs f

-- Discards the trailing NUL.
toByteString :: String -> ByteString
toByteString (String bs) = BS.init bs

-- | O(1) if the input is null-terminated. Otherwise the input is copied into
-- a null-terminated buffer first.
fromByteString :: ByteString -> String
fromByteString bs
  | BS.length bs > 0 && BS.last bs == 0 = String bs
  | otherwise = String (bs `BS.snoc` 0)

-- | Same as 'fromByteString', but doesn't check whether the input is
-- null-terminated or not.
unsafeFromByteString :: ByteString -> String
unsafeFromByteString = String
