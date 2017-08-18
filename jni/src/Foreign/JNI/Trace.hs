{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Foreign.JNI.Trace
  ( traceEventIO
  , traceNewLocalRef
  , traceDeleteLocalRef
  , tracePushLocalFrame
  , tracePushLocalFrameCS
  , tracePopLocalFrame
  , JNIEvent(..)
  , KernelThreadId(..)
  , StackFrameInfo(..)
  , extractStackFrameInfo
  , isTracingEnabled
  , isTracingLocalRefCallStacksEnabled
  ) where

import Data.Binary
import Data.Int (Int32)
import Data.List (isInfixOf)
import qualified Debug.Trace as Trace
import GHC.Generics (Generic)
import GHC.Exts (toList)
import GHC.Stack (callStack, HasCallStack, SrcLoc(..), CallStack)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.JNI.Types (J(..))
import Foreign.JNI.OSThreads (kernelThreadId, KernelThreadId(..))
import Foreign.Ptr (IntPtr, ptrToIntPtr)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Tells if JNI tracing is enabled.
{-# NOINLINE isTracingEnabled #-}
isTracingEnabled :: Bool
isTracingEnabled = maybe False (isInfixOf "localrefs") $
    unsafePerformIO $ lookupEnv "HASKELL_JNI_TRACE"

-- | Tells if tracing is enabled for the call stack where local references
-- are created.
{-# NOINLINE isTracingLocalRefCallStacksEnabled #-}
isTracingLocalRefCallStacksEnabled :: Bool
isTracingLocalRefCallStacksEnabled =
    maybe False (isInfixOf "localrefs.callstacks") $
      unsafePerformIO $ lookupEnv "HASKELL_JNI_TRACE"

traceEventIO :: JNIEvent -> IO ()
traceEventIO event | isTracingEnabled = do
   KernelThreadId kernelTid <- kernelThreadId
   Trace.traceEventIO $ unwords ["[jni]", show kernelTid, show event]
traceEventIO _ = return ()

-- Keeping the data constructor names short helps producing more compact
-- eventlogs since they are serialized with 'Show'.

-- | An event produced by JNI
data JNIEvent
    = NLR IntPtr (Maybe [StackFrameInfo]) -- NewLocalRef
    | DLR IntPtr -- DeleteLocalRef
    | PushLF (Maybe Int32) [StackFrameInfo]
    | PopLF IntPtr [StackFrameInfo]
  deriving (Show, Read)

-- | Identifying information of a stack frame
--
-- @SFI function file line column@
data StackFrameInfo = SFI String String Int Int
  deriving (Show, Read, Generic)

instance Binary StackFrameInfo

-- | Emits an event to the eventlog corresponding to the creation of a local
-- reference.
traceNewLocalRef :: HasCallStack => IntPtr -> IO ()
traceNewLocalRef ptr =
    traceEventIO $ NLR ptr $
      if isTracingLocalRefCallStacksEnabled
        then Just $ drop 1 $ extractStackFrameInfo callStack
        else Nothing

-- | Emits an event to the eventlog corresponding to the deletion of a local
-- reference.
traceDeleteLocalRef :: HasCallStack => J a -> IO ()
traceDeleteLocalRef obj = traceEventIO $ DLR (unsafeJToIntPtr obj)

-- | Emits an event to the eventlog corresponding to a call of 'pushLocalFrame'.
-- This is useful to signal entry into a Haskell function when invoked from
-- Java.
tracePushLocalFrame :: HasCallStack => Maybe Int32 -> IO ()
tracePushLocalFrame capacity =
    tracePushLocalFrameCS (extractStackFrameInfo callStack) capacity

-- | Like 'tracePushLocalFrame' but it takes the call stack explicitly.
tracePushLocalFrameCS :: [StackFrameInfo] -> Maybe Int32 -> IO ()
tracePushLocalFrameCS cs capacity = traceEventIO $ PushLF capacity cs

-- | Emits an event to the eventlog corresponding to a call of 'popLocalFrame'.
-- This is useful to signal exit into a Haskell function when invoked from Java.
tracePopLocalFrame :: HasCallStack => J ty -> IO ()
tracePopLocalFrame obj =
    traceEventIO $ PopLF (unsafeJToIntPtr obj) (extractStackFrameInfo callStack)

-- | Extracts 'StackFrameInfo's from a 'CallStack'.
extractStackFrameInfo :: CallStack -> [StackFrameInfo]
extractStackFrameInfo cs =
    [ SFI f srcLocFile srcLocStartLine srcLocStartCol
    | (f, SrcLoc {..}) <- dropCalls (toList cs)
    ]
  where
    dropCalls (_ : xs@((f, _) : _))
      | f `elem` ["pushLocalFrame", "popLocalFrame"] = xs
    dropCalls xs = xs

-- | Converts a Java reference to an 'IntPtr'.
unsafeJToIntPtr :: J a -> IntPtr
unsafeJToIntPtr (J fptr) = ptrToIntPtr $ unsafeForeignPtrToPtr fptr
