{-# LANGUAGE LambdaCase #-}
module DirectBuffer where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import Control.Exception (bracket)
import Data.IORef
import Foreign.C.Types
import Foreign.JNI.Types
import Foreign.JNI.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr

newtype Buffer = Buffer (Ptr CChar)
newtype DirectBufferPool = DirectBufferPool (IORef [(JByteBuffer, Buffer)])

directBufferCapacity :: Int
directBufferCapacity = 1000

newDirectBufferPool :: IO DirectBufferPool
newDirectBufferPool = DirectBufferPool <$> newIORef []

withDirectBuffer :: DirectBufferPool -> (JByteBuffer -> Buffer -> IO a) -> IO a
withDirectBuffer (DirectBufferPool refPool) f =
    bracket getBuffer releaseBuffer (uncurry f)

  where

    getBuffer = do
      mbuffer <- atomicModifyIORef refPool $ \case
        [] -> ([], Nothing)
        x:xs -> (xs, Just x)
      maybe newBuffer return mbuffer

    newBuffer = do
      bufferPtr <- mallocBytes directBufferCapacity
      jbuffer <- newDirectByteBuffer bufferPtr (fromIntegral directBufferCapacity)
        >>= newGlobalRef
      return (jbuffer, Buffer bufferPtr)

    releaseBuffer buffer =
      atomicModifyIORef refPool $ \xs -> (buffer : xs, ())

writeBuffer :: Buffer -> ByteString -> IO ()
writeBuffer (Buffer buffer) bs = do
  let bsLen = ByteString.length bs
  if bsLen > directBufferCapacity then error "buffer too small"
  else ByteString.unsafeUseAsCString bs $ \ptr -> copyBytes buffer ptr bsLen
