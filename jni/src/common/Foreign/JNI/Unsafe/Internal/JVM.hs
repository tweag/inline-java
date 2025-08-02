{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | JVM and thread management calls
module Foreign.JNI.Unsafe.Internal.JVM where

import Control.Concurrent
  (isCurrentThreadBound, rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception
  (Exception, SomeException, bracket, bracket_, catch, handle, throwIO)
import Control.Monad (unless)
import Data.Bits
import Data.Int
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign.C (CChar)
import Foreign.JNI.Internal.RWLock (RWLock)
import qualified Foreign.JNI.Internal.RWLock as RWLock
import Foreign.JNI.Internal.BackgroundWorker (BackgroundWorker)
import qualified Foreign.JNI.Internal.BackgroundWorker as BackgroundWorker
import Foreign.JNI.Types
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Stack (HasCallStack, callStack, getCallStack, prettySrcLoc)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import qualified System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (String)
import qualified Prelude

C.context (C.baseCtx <> C.bsCtx <> jniCtx)

C.include "<jni.h>"
C.include "<stdio.h>"
C.include "<errno.h>"
C.include "<stdlib.h>"

$(C.verbatim "static JavaVM* jniJVM; ")

-- A thread-local variable to cache the JNI environment. Accessing this variable
-- is faster than calling @jvm->GetEnv()@.
$(C.verbatim "static __thread JNIEnv* jniEnv; ")

-- | A JNI call is made from a thread not attached to the JVM.
data ThreadNotAttached = ThreadNotAttached
  deriving (Exception, Show)

-- | A JNI call is made from an unbound thread.
data ThreadNotBound = ThreadNotBound
  deriving (Exception, Show)

-- | Thrown when an JNI call is made from an unbound thread.
data JNIError = JNIError Prelude.String Int32
  deriving Show

instance Exception JNIError

-- | This lock is used to avoid the JVM from dying before any finalizers
-- deleting global references are finished.
--
-- Finalizers try to acquire read locks.
--
-- The JVM acquires a write lock before shutdown. Thence, finalizers fail to
-- acquire read locks and behave as noops.
globalJVMLock :: RWLock
globalJVMLock = unsafePerformIO RWLock.new
{-# NOINLINE globalJVMLock #-}

throwIfNotOK_ :: HasCallStack => IO Int32 -> IO ()
throwIfNotOK_ m = m >>= \case
  rc
    | rc == [CU.pure| jint { JNI_OK } |] -> return ()
    | rc == [CU.pure| jint { JNI_EDETACHED } |] -> throwIO ThreadNotAttached
    | otherwise ->
      throwIO $ JNIError locStr rc
  where
    locStr = case getCallStack callStack of
      (_, loc) : _ -> prettySrcLoc loc
      _ -> "no location"

attachCurrentThreadAsDaemon :: IO ()
attachCurrentThreadAsDaemon = do
    checkBoundness
    throwIfNotOK_
      [CU.exp| jint {
        (*$(JavaVM* jvmPtr))->AttachCurrentThreadAsDaemon($(JavaVM* jvmPtr), (void**)&jniEnv, NULL)
      } |]

detachCurrentThread :: IO ()
detachCurrentThread =
    throwIfNotOK_
    [CU.block| jint {
      int rc = (*$(JavaVM* jvmPtr))->DetachCurrentThread($(JavaVM* jvmPtr));
      if (rc == JNI_OK)
        jniEnv = NULL;
      return rc;
    } |]

-- | Tells whether the calling thread is attached to the JVM.
isCurrentThreadAttached :: IO Bool
isCurrentThreadAttached =
    catch (getJNIEnv >> return True) (\ThreadNotAttached -> return False)

-- | Attaches the calling thread to the JVM, runs the given IO action and
-- then detaches the thread.
--
-- If the thread is already attached no attaching and detaching is performed.
runInAttachedThread :: IO a -> IO a
runInAttachedThread io = do
    attached <- isCurrentThreadAttached
    if attached
    then io
    else bracket_
          attachCurrentThreadAsDaemon
          detachCurrentThread
          io

getVersion :: Ptr JNIEnv -> IO (Int16, Int16)
getVersion env = (\i -> (fromIntegral $ i .>>. 16, fromIntegral i)) <$> do
    [CU.exp| jint {
      (*$(JNIEnv *env))->GetVersion($(JNIEnv *env))
    } |]

getEnvJVM :: Ptr JNIEnv -> IO JVM
getEnvJVM env = JVM_ <$> do
    [C.block| JavaVM * {
      JavaVM *jvm;
      (*$(JNIEnv *env))->GetJavaVM($(JNIEnv *env), (JavaVM**)&jvm);
      return jvm;
    } |]

-- | Sets the current JVM
--
setJVM :: JVM -> IO ()
setJVM (JVM_ jvm) = do
    [CU.exp| void {
      jniJVM = $(JavaVM *jvm)
    } |]

-- | The current JVM
--
-- Assumes there's at most one JVM. The current JNI spec says only
-- one JVM per process is supported anyways.
-- https://docs.oracle.com/en/java/javase/11/docs/specs/jni/invocation.html#jni_getcreatedjavavms
{-# NOINLINE jvmPtr #-}
jvmPtr :: Ptr JVM
jvmPtr = unsafePerformIO $ [CU.exp| JavaVM* { jniJVM } |] >>= \case
    vm | vm == nullPtr ->
      fail "JVM has not been set yet. Call setJVM to set a JVM."
    vm -> return vm

-- | Yields the JNIEnv of the calling thread.
--
getJNIEnv :: IO (Ptr JNIEnv)
getJNIEnv = [CU.exp| JNIEnv* { jniEnv } |] >>= \case
    env | env == nullPtr -> do
      throwIfNotOK_
        [CU.exp| jint {
          (*$(JavaVM* jvmPtr))->GetEnv($(JavaVM* jvmPtr), (void**)&jniEnv, JNI_VERSION_1_6)
        }|]
      [CU.exp| JNIEnv* { jniEnv } |]
    env -> return env

-- | Run an action against the appropriate 'JNIEnv'.
--
-- Each OS thread has its own 'JNIEnv', which this function gives access to.
withJNIEnv :: (Ptr JNIEnv -> IO a) -> IO a
withJNIEnv f = getJNIEnv >>= f

useAsCStrings :: [ByteString] -> ([Ptr CChar] -> IO a) -> IO a
useAsCStrings strs m =
  foldr (\str k cstrs -> BS.useAsCString str $ \cstr -> k (cstr:cstrs)) m strs []

#if !defined(ANDROID)
-- | Create a new JVM, with the given arguments. /Can only be called once per
-- process due to limitations of the JNI implementation/ (see documentation
-- of JNI_CreateJavaVM and JNI_DestroyJavaVM). Best practice: use 'withJVM'
-- instead. Only useful for GHCi.
newJVM :: [ByteString] -> IO JVM
newJVM options = JVM_ <$> do
    checkBoundness
    startJVM <* startFinalizerThread
  where
    startJVM =
      useAsCStrings options $ \cstrs -> do
        withArray cstrs $ \(coptions :: Ptr (Ptr CChar)) -> do
          let n = fromIntegral (length cstrs) :: C.CInt

          [C.block| JavaVM * {
            JavaVMInitArgs vm_args;
            JavaVMOption *options = malloc(sizeof(JavaVMOption) * $(int n));
            for(int i = 0; i < $(int n); i++)
                options[i].optionString = $(char **coptions)[i];
            vm_args.version = JNI_VERSION_1_6;
            vm_args.nOptions = $(int n);
            vm_args.options = options;
            vm_args.ignoreUnrecognized = 0;
            JNI_CreateJavaVM(&jniJVM, (void**)&jniEnv, &vm_args);
            free(options);
            return jniJVM; } |]
#endif

checkBoundness :: IO ()
checkBoundness =
  if rtsSupportsBoundThreads then do
    bound <- isCurrentThreadBound
    unless bound (throwIO ThreadNotBound)
  else
    error $ unlines
      [ "jni won't work with a non-threaded runtime."
      , "Perhaps link your program with -threaded."
      ]

#if !defined(ANDROID)
-- | Deallocate a 'JVM' created using 'newJVM'.
destroyJVM :: JVM -> IO ()
destroyJVM (JVM_ jvm) = do
    stopFinalizerThread
    RWLock.acquireWriteLock globalJVMLock
    [C.block| void {
        (*$(JavaVM *jvm))->DestroyJavaVM($(JavaVM *jvm));
        jniEnv = NULL;
        jniJVM = NULL;
    } |]

-- | Create a new JVM, with the given arguments. Destroy it once the given
-- action completes. /Can only be called once per process due to
-- limitations of the JNI implementation/ (see documentation of
-- JNI_CreateJavaVM and JNI_DestroyJavaVM). Best practice: use it to wrap
-- your @main@ function.
withJVM :: [ByteString] -> IO a -> IO a
withJVM options action = bracket (newJVM options) destroyJVM (const action)
#endif

-- | A background thread for cleaning global references
finalizerThread :: IORef BackgroundWorker
{-# NOINLINE finalizerThread #-}
finalizerThread = unsafePerformIO $ newIORef uninitializedFinalizerThread

uninitializedFinalizerThread :: BackgroundWorker
uninitializedFinalizerThread = error "The finalizer thread is not initialized"

-- | Starts a background thread to delete global references.
--
-- Global references have GC finalizers responsible for deleting
-- them. Because the finalizers run in threads dettached from the
-- JVM, the finalizers send the references to this auxiliary
-- thread for deletion.
--
-- Call this function only once at the beginning of the program.
--
-- This call is not needed if 'newJVM' or 'startJVM' are invoked.
-- This call is only useful when JNI is used in an application where
-- the JVM has been initialized by other means.
--
-- Call 'stopFinalizerThread' to stop the thread.
startFinalizerThread :: IO ()
startFinalizerThread =
    BackgroundWorker.create
        (exitOnError . runInBoundThread . runInAttachedThread)
    >>= writeIORef finalizerThread
  where
    exitOnError = handle $ \(e :: SomeException) -> do
      System.Exit.die $ "Haskell jni package: error in finalizer thread: " ++ show e

-- | Stops the background thread that deletes global references.
--
-- Any global references waiting to be deletead are deleted
-- before stopping the thread. Any new references submitted for
-- deletion after this call won't be deleted.
--
-- Probably the safest way to call this function is to stop first
-- all threads using global references, and calling the GC in
-- advance to cause unreachable global references to be submitted
-- for deletion.
--
-- This call is not needed if 'withJVM' or 'destroyJVM' are called
-- instead. This call is only useful when JNI is used in an
-- application where the JVM is terminated by other means.
stopFinalizerThread :: IO ()
stopFinalizerThread = do
    readIORef finalizerThread >>= BackgroundWorker.stop
    writeIORef finalizerThread uninitializedFinalizerThread

-- | Runs a task in a long-living background thread attached to the
-- JVM. The thread is dedicated to release unused references to java
-- objects.
--
-- Useful to be called from GC finalizers, where attaching to the JVM would
-- be too expensive.
--
submitToFinalizerThread :: IO () -> IO ()
submitToFinalizerThread action = do
  worker <- readIORef finalizerThread
  BackgroundWorker.submitTask worker action
