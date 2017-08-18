-- | Binds OSThreads functions from the RTS.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Foreign.JNI.OSThreads (KernelThreadId(..), kernelThreadId) where

-- any may be required
import Data.Word
import Data.Int

#include <Rts.h>

newtype KernelThreadId = KernelThreadId #{type KernelThreadId}
  deriving (Eq, Ord, Show)

foreign import ccall unsafe kernelThreadId :: IO KernelThreadId
