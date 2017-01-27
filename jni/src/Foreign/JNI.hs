-- | Low-level bindings to the Java Native Interface (JNI).
--
-- Read the
-- <https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html JNI spec>
-- for authoritative documentation as to what each of the functions in
-- this module does. The names of the bindings in this module were chosen to
-- match the names of the functions in the JNI spec.
--
-- All bindings in this module access the JNI via a thread-local variable of
-- type @JNIEnv *@. If the current OS thread has not yet been "attached" to the
-- JVM, it is attached implicitly upon the first call to one of these bindings
-- in the current thread.
--
-- The 'String' type in this module is the type of JNI strings. See
-- "Foreign.JNI.String".

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Foreign.JNI
  ( -- * JNI functions
    -- ** VM creation
    withJVM
    -- ** Class loading
  , defineClass
  , JNINativeMethod(..)
  , registerNatives
    -- ** Exceptions
  , throw
  , throwNew
    -- ** Query functions
  , findClass
  , getFieldID
  , getStaticFieldID
  , getMethodID
  , getStaticMethodID
  , getObjectClass
    -- ** Reference manipulation
  , newGlobalRef
  , deleteGlobalRef
  , newLocalRef
  , deleteLocalRef
  , pushLocalFrame
  , popLocalFrame
    -- ** Field accessor functions
    -- *** Get fields
  , getObjectField
  , getBooleanField
  , getIntField
  , getLongField
  , getCharField
  , getShortField
  , getByteField
  , getDoubleField
  , getFloatField
    -- *** Get static fields
  , getStaticObjectField
  , getStaticBooleanField
  , getStaticIntField
  , getStaticLongField
  , getStaticCharField
  , getStaticShortField
  , getStaticByteField
  , getStaticDoubleField
  , getStaticFloatField
    -- *** Set fields
  , setObjectField
  , setBooleanField
  , setIntField
  , setLongField
  , setCharField
  , setShortField
  , setByteField
  , setDoubleField
  , setFloatField
    -- *** Set static fields
  , setStaticObjectField
  , setStaticBooleanField
  , setStaticIntField
  , setStaticLongField
  , setStaticCharField
  , setStaticShortField
  , setStaticByteField
  , setStaticDoubleField
  , setStaticFloatField
    -- ** Method invocation
  , callObjectMethod
  , callBooleanMethod
  , callIntMethod
  , callLongMethod
  , callCharMethod
  , callShortMethod
  , callByteMethod
  , callDoubleMethod
  , callFloatMethod
  , callVoidMethod
  , callStaticObjectMethod
  , callStaticVoidMethod
  , callStaticBooleanMethod
  , callStaticIntMethod
  , callStaticLongMethod
  , callStaticCharMethod
  , callStaticShortMethod
  , callStaticByteMethod
  , callStaticDoubleMethod
  , callStaticFloatMethod
    -- ** Object construction
  , newObject
  , newString
  , newObjectArray
  , newBooleanArray
  , newByteArray
  , newCharArray
  , newShortArray
  , newIntArray
  , newLongArray
  , newFloatArray
  , newDoubleArray
    -- ** Array manipulation
  , getArrayLength
  , getStringLength
  , getBooleanArrayElements
  , getByteArrayElements
  , getCharArrayElements
  , getShortArrayElements
  , getIntArrayElements
  , getLongArrayElements
  , getFloatArrayElements
  , getDoubleArrayElements
  , getStringChars
  , setBooleanArrayRegion
  , setByteArrayRegion
  , setCharArrayRegion
  , setShortArrayRegion
  , setIntArrayRegion
  , setLongArrayRegion
  , setFloatArrayRegion
  , setDoubleArrayRegion
  , releaseBooleanArrayElements
  , releaseByteArrayElements
  , releaseCharArrayElements
  , releaseShortArrayElements
  , releaseIntArrayElements
  , releaseLongArrayElements
  , releaseFloatArrayElements
  , releaseDoubleArrayElements
  , releaseStringChars
  , getObjectArrayElement
  , setObjectArrayElement
  ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, bracket, finally, throwIO)
import Control.Monad (join, unless, void, when)
import Data.Choice
import Data.Coerce
import Data.Int
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.TLS.PThread
import Foreign.C (CChar)
import Foreign.ForeignPtr
  ( finalizeForeignPtr
  , newForeignPtr_
  , withForeignPtr
  )
import Foreign.JNI.NativeMethod
import Foreign.JNI.Types
import qualified Foreign.JNI.String as JNI
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, nullPtr)
import GHC.ForeignPtr (newConcForeignPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (String)

C.context (C.baseCtx <> C.bsCtx <> jniCtx)

C.include "<jni.h>"
C.include "<errno.h>"
C.include "<stdlib.h>"

data JavaException = JavaException JThrowable
  deriving (Show, Typeable)

instance Exception JavaException

-- | Thrown when @Get<PrimitiveType>ArrayElements@ returns a null pointer,
-- because it wanted to copy the array contents but couldn't. In this case the
-- JVM doesn't throw OutOfMemory according to the JNI spec.
data ArrayCopyFailed = ArrayCopyFailed
  deriving (Show, Typeable)

instance Exception ArrayCopyFailed

objectFromPtr :: Ptr (J a) -> IO (J a)
objectFromPtr ptr = J <$> newForeignPtr_ ptr

-- | Map Java exceptions to Haskell exceptions.
throwIfException :: Ptr JNIEnv -> IO a -> IO a
throwIfException env m = m `finally` do
    excptr <- [CU.exp| jthrowable { (*$(JNIEnv *env))->ExceptionOccurred($(JNIEnv *env)) } |]
    unless (excptr == nullPtr) $ do
      [CU.exp| void { (*$(JNIEnv *env))->ExceptionDescribe($(JNIEnv *env)) } |]
      [CU.exp| void { (*$(JNIEnv *env))->ExceptionClear($(JNIEnv *env)) } |]
      throwIO . JavaException =<< newGlobalRef =<< objectFromPtr excptr

-- | Check whether a pointer is null.
throwIfNull :: IO (Ptr a) -> IO (Ptr a)
throwIfNull m = do
    ptr <- m
    if ptr == nullPtr
    then throwIO ArrayCopyFailed
    else return ptr

-- | A read-write lock
--
-- Concurrent readers are allowed, but only one writer is supported.
newtype RWLock =
    RWLock (IORef (Int, RWWantedState))
    -- ^ A count of the held read locks and the wanted state

-- | The wanted state of the RW
data RWWantedState
    = Reading            -- ^ There are no writers
    | Writing (MVar ())
       -- ^ A writer wants to write, grant no more read locks. The MVar is used
       -- to notify the writer when the currently held read locks are released.

-- | Creates a new read-write lock.
newRWLock :: IO RWLock
newRWLock = RWLock <$> newIORef (0, Reading)

-- | Tries to acquire a read lock. If this call returns `Do #read`, no writer
-- will be granted a lock before the read lock is released.
tryAcquireReadLock :: RWLock -> IO (Choice "read")
tryAcquireReadLock (RWLock ref) = do
    atomicModifyIORef ref $ \case
      (!readers,  Reading) -> ((readers + 1, Reading),    Do #read)
      st                   -> (                       st, Don't #read)

-- | Releases a read lock.
releaseReadLock :: RWLock -> IO ()
releaseReadLock (RWLock ref) = do
    st <- atomicModifyIORef ref $
            \st@(readers, aim) -> ((readers - 1, aim), st)
    case st of
      -- Notify the writer if I'm the last reader.
      (0, Writing mv) -> putMVar mv ()
      _               -> return ()

-- | Waits until the current read locks are released and grants a write lock.
acquireWriteLock :: RWLock -> IO ()
acquireWriteLock (RWLock ref) = do
    mv <- newEmptyMVar
    join $ atomicModifyIORef ref $ \case
      (0, _) -> ((0, Writing mv),   return ())
      st     -> (               st, takeMVar mv)

-- | This lock is used to avoid the JVM from dying before any finalizers
-- deleting global references are finished.
--
-- Finalizers try to acquire read locks.
--
-- The JVM acquires a write lock before shutdown. Thence, finalizers fail to
-- acquire read locks and behave as noops.
globalJVMLock :: RWLock
globalJVMLock = unsafePerformIO newRWLock
{-# NOINLINE globalJVMLock #-}

-- | A global mutable cell holding the TLS variable, whose content is set once
-- for each thread.
envTlsRef :: IORef (TLS (Ptr JNIEnv))
{-# NOINLINE envTlsRef #-}
envTlsRef = unsafePerformIO $ do
    -- It doesn't matter if this computation ends up running twice, say because
    -- of lazy blackholing.
    !tls <- mkTLS $ [C.block| JNIEnv* {
      jsize num_jvms;
      JavaVM *jvm;
      /* Assume there's at most one JVM. The current JNI spec (2016) says only
       * one JVM per process is supported anyways. */
      JNI_GetCreatedJavaVMs(&jvm, 1, &num_jvms);
      JNIEnv *env;

      if(!num_jvms) {
              fprintf(stderr, "No JVM has been initialized yet.\n");
              exit(EFAULT);
      }

      /* Attach as daemon to match GHC's usual semantics for threads, which are
       * daemonic.
       */
      (*jvm)->AttachCurrentThreadAsDaemon(jvm, (void**)&env, NULL);
      return env; } |]
    newIORef tls

-- | Run an action against the appropriate 'JNIEnv'.
--
-- Each OS thread has its own 'JNIEnv', which this function gives access to.
--
-- TODO check whether this call is only safe from a (bound) thread.
withJNIEnv :: (Ptr JNIEnv -> IO a) -> IO a
withJNIEnv f = f =<< getTLS =<< readIORef envTlsRef

useAsCStrings :: [ByteString] -> ([Ptr CChar] -> IO a) -> IO a
useAsCStrings strs m =
  foldr (\str k cstrs -> BS.useAsCString str $ \cstr -> k (cstr:cstrs)) m strs []

-- | Create a new JVM, with the given arguments. /Can only be called once/. Best
-- practice: use it to wrap your @main@ function.
withJVM :: [ByteString] -> IO a -> IO a
withJVM options action =
    bracket ini fini (const action)
  where
    ini = do
      useAsCStrings options $ \cstrs -> do
        withArray cstrs $ \(coptions :: Ptr (Ptr CChar)) -> do
          let n = fromIntegral (length cstrs) :: C.CInt
          [C.block| JavaVM * {
            JavaVM *jvm;
            JNIEnv *env;
            JavaVMInitArgs vm_args;
            JavaVMOption *options = malloc(sizeof(JavaVMOption) * $(int n));
            for(int i = 0; i < $(int n); i++)
                    options[i].optionString = $(char **coptions)[i];
            vm_args.version = JNI_VERSION_1_6;
            vm_args.nOptions = $(int n);
            vm_args.options = options;
            vm_args.ignoreUnrecognized = 0;
            JNI_CreateJavaVM(&jvm, (void**)&env, &vm_args);
            free(options);
            return jvm; } |]
    fini jvm = do
      acquireWriteLock globalJVMLock
      [C.block| void { (*$(JavaVM *jvm))->DestroyJavaVM($(JavaVM *jvm)); } |]

defineClass
  :: Coercible o (J ('Class "java.lang.ClassLoader"))
  => JNI.String -- ^ Class name
  -> o          -- ^ Loader
  -> ByteString -- ^ Bytecode buffer
  -> IO JClass
defineClass name (coerce -> upcast -> loader) buf = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString name $ \namep ->
    objectFromPtr =<<
    [CU.exp| jclass {
      (*$(JNIEnv *env))->DefineClass($(JNIEnv *env),
                                     $(char *namep),
                                     $fptr-ptr:(jobject loader),
                                     $bs-ptr:buf,
                                     $bs-len:buf) } |]
registerNatives
  :: JClass
  -> [JNINativeMethod]
  -> IO ()
registerNatives cls methods = withJNIEnv $ \env ->
    throwIfException env $
    withArray methods $ \cmethods -> do
      let numMethods = fromIntegral $ length methods
      _ <- [CU.exp| jint {
             (*$(JNIEnv *env))->RegisterNatives($(JNIEnv *env),
                                                $fptr-ptr:(jclass cls),
                                                $(JNINativeMethod *cmethods),
                                                $(int numMethods)) } |]
      return ()

throw :: Coercible o (J a) => o -> IO ()
throw (coerce -> upcast -> obj) = withJNIEnv $ \env -> void $ do
    [CU.exp| jint {
       (*$(JNIEnv *env))->Throw($(JNIEnv *env),
                                $fptr-ptr:(jobject obj)) } |]

throwNew :: JClass -> JNI.String -> IO ()
throwNew cls msg = withJNIEnv $ \env ->
    JNI.withString msg $ \msgp -> void $ do
    [CU.exp| jint {
       (*$(JNIEnv *env))->ThrowNew($(JNIEnv *env),
                                   $fptr-ptr:(jclass cls),
                                   $(char *msgp)) } |]

findClass
  :: JNI.String -- ^ Class name
  -> IO JClass
findClass name = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString name $ \namep ->
    objectFromPtr =<<
    [CU.exp| jclass { (*$(JNIEnv *env))->FindClass($(JNIEnv *env), $(char *namep)) } |]

newObject :: JClass -> JNI.String -> [JValue] -> IO JObject
newObject cls sig args = withJNIEnv $ \env ->
    throwIfException env $
    withJValues args $ \cargs -> do
      constr <- getMethodID cls "<init>" sig
      objectFromPtr =<< [CU.exp| jobject {
        (*$(JNIEnv *env))->NewObjectA($(JNIEnv *env),
                                      $fptr-ptr:(jclass cls),
                                      $(jmethodID constr),
                                      $(jvalue *cargs)) } |]

getFieldID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> JNI.String -- ^ JNI signature
  -> IO JFieldID
getFieldID cls fieldname sig = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString fieldname $ \fieldnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jfieldID {
      (*$(JNIEnv *env))->GetFieldID($(JNIEnv *env),
                                    $fptr-ptr:(jclass cls),
                                    $(char *fieldnamep),
                                    $(char *sigp)) } |]

getStaticFieldID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> JNI.String -- ^ JNI signature
  -> IO JFieldID
getStaticFieldID cls fieldname sig = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString fieldname $ \fieldnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jfieldID {
      (*$(JNIEnv *env))->GetStaticFieldID($(JNIEnv *env),
                                          $fptr-ptr:(jclass cls),
                                          $(char *fieldnamep),
                                          $(char *sigp)) } |]

#define GET_FIELD(name, hs_rettype, c_rettype) \
get/**/name/**/Field :: Coercible o (J a) => o -> JFieldID -> IO hs_rettype; \
get/**/name/**/Field (coerce -> upcast -> obj) field = withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| c_rettype { \
      (*$(JNIEnv *env))->Get/**/name/**/Field($(JNIEnv *env), \
                                              $fptr-ptr:(jobject obj), \
                                              $(jfieldID field)) } |]

getObjectField :: Coercible o (J a) => o -> JFieldID -> IO JObject
getObjectField x y =
    let GET_FIELD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< getObjectField x y
GET_FIELD(Boolean, Word8, jboolean)
GET_FIELD(Byte, CChar, jbyte)
GET_FIELD(Char, Word16, jchar)
GET_FIELD(Short, Int16, jshort)
GET_FIELD(Int, Int32, jint)
GET_FIELD(Long, Int64, jlong)
GET_FIELD(Float, Float, jfloat)
GET_FIELD(Double, Double, jdouble)

#define GET_STATIC_FIELD(name, hs_rettype, c_rettype) \
getStatic/**/name/**/Field :: JClass -> JFieldID -> IO hs_rettype; \
getStatic/**/name/**/Field klass field = withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| c_rettype { \
      (*$(JNIEnv *env))->GetStatic/**/name/**/Field($(JNIEnv *env), \
                                                    $fptr-ptr:(jclass klass), \
                                                    $(jfieldID field)) } |]

getStaticObjectField :: JClass -> JFieldID -> IO JObject
getStaticObjectField x y =
    let GET_STATIC_FIELD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< getStaticObjectField x y
GET_STATIC_FIELD(Boolean, Word8, jboolean)
GET_STATIC_FIELD(Byte, CChar, jbyte)
GET_STATIC_FIELD(Char, Word16, jchar)
GET_STATIC_FIELD(Short, Int16, jshort)
GET_STATIC_FIELD(Int, Int32, jint)
GET_STATIC_FIELD(Long, Int64, jlong)
GET_STATIC_FIELD(Float, Float, jfloat)
GET_STATIC_FIELD(Double, Double, jdouble)

#define SET_FIELD(name, hs_fieldtype, c_fieldtype) \
set/**/name/**/Field :: Coercible o (J a) => o -> JFieldID -> hs_fieldtype -> IO (); \
set/**/name/**/Field (coerce -> upcast -> obj) field x = \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.block| void { \
      (*$(JNIEnv *env))->Set/**/name/**/Field($(JNIEnv *env), \
                                              $fptr-ptr:(jobject obj), \
                                              $(jfieldID field), \
                                              $(c_fieldtype x)); } |]

setObjectField :: Coercible o (J a) => o -> JFieldID -> JObject -> IO ()
setObjectField x y z =
    let SET_FIELD(Object, (Ptr JObject), jobject)
    in withForeignPtr (coerce z) (setObjectField x y)
SET_FIELD(Boolean, Word8, jboolean)
SET_FIELD(Byte, CChar, jbyte)
SET_FIELD(Char, Word16, jchar)
SET_FIELD(Short, Int16, jshort)
SET_FIELD(Int, Int32, jint)
SET_FIELD(Long, Int64, jlong)
SET_FIELD(Float, Float, jfloat)
SET_FIELD(Double, Double, jdouble)

#define SET_STATIC_FIELD(name, hs_fieldtype, c_fieldtype) \
setStatic/**/name/**/Field :: JClass -> JFieldID -> hs_fieldtype -> IO (); \
setStatic/**/name/**/Field klass field x = \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.block| void { \
      (*$(JNIEnv *env))->SetStatic/**/name/**/Field($(JNIEnv *env), \
                                                    $fptr-ptr:(jclass klass), \
                                                    $(jfieldID field), \
                                                    $(c_fieldtype x)); } |]

setStaticObjectField :: JClass -> JFieldID -> JObject -> IO ()
setStaticObjectField x y z =
    let SET_STATIC_FIELD(Object, (Ptr JObject), jobject)
    in withForeignPtr (coerce z) (setStaticObjectField x y)
SET_STATIC_FIELD(Boolean, Word8, jboolean)
SET_STATIC_FIELD(Byte, CChar, jbyte)
SET_STATIC_FIELD(Char, Word16, jchar)
SET_STATIC_FIELD(Short, Int16, jshort)
SET_STATIC_FIELD(Int, Int32, jint)
SET_STATIC_FIELD(Long, Int64, jlong)
SET_STATIC_FIELD(Float, Float, jfloat)
SET_STATIC_FIELD(Double, Double, jdouble)

getMethodID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> JNI.String -- ^ JNI signature
  -> IO JMethodID
getMethodID cls methodname sig = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString methodname $ \methodnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jmethodID {
      (*$(JNIEnv *env))->GetMethodID($(JNIEnv *env),
                                     $fptr-ptr:(jclass cls),
                                     $(char *methodnamep),
                                     $(char *sigp)) } |]

getStaticMethodID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> JNI.String -- ^ JNI signature
  -> IO JMethodID
getStaticMethodID cls methodname sig = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString methodname $ \methodnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jmethodID {
      (*$(JNIEnv *env))->GetStaticMethodID($(JNIEnv *env),
                                           $fptr-ptr:(jclass cls),
                                           $(char *methodnamep),
                                           $(char *sigp)) } |]

getObjectClass :: Coercible o (J ty) => o -> IO JClass
getObjectClass (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    objectFromPtr =<<
    [CU.exp| jclass {
      (*$(JNIEnv *env))->GetObjectClass($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]

-- | Creates a global reference to the object referred to by
-- the given reference.
--
-- Arranges for a finalizer to call 'deleteGlobalRef' when the
-- global reference is no longer reachable on the Haskell side.
newGlobalRef :: Coercible o (J ty) => o -> IO o
newGlobalRef (coerce -> upcast -> obj) = withJNIEnv $ \env -> do
    gobj <-
      [CU.exp| jobject {
        (*$(JNIEnv *env))->NewGlobalRef($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]
    coerce <$> J <$> newConcForeignPtr gobj (finalize gobj)
  where
    finalize gobj = do
      bracket (tryAcquireReadLock globalJVMLock)
              (\doRead -> when (toBool doRead) $ releaseReadLock globalJVMLock)
              $ \doRead ->
        when (toBool doRead) $ withJNIEnv $ \env ->
          [CU.block| void { (*$(JNIEnv *env))->DeleteGlobalRef($(JNIEnv *env)
                                                              ,$(jobject gobj));
                          } |]

deleteGlobalRef :: Coercible o (J ty) => o -> IO ()
deleteGlobalRef (coerce -> J p) = finalizeForeignPtr p

-- NB: Cannot add a finalizer to local references because it may
-- run in a thread where the reference is not valid.
newLocalRef :: Coercible o (J ty) => o -> IO (J ty)
newLocalRef (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    unsafeCast <$> (objectFromPtr =<<)
    [CU.exp| jobject {
      (*$(JNIEnv *env))->NewLocalRef($(JNIEnv *env),
                                     $fptr-ptr:(jobject obj)) } |]

deleteLocalRef :: Coercible o (J ty) => o -> IO ()
deleteLocalRef (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    [CU.exp| void {
      (*$(JNIEnv *env))->DeleteLocalRef($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]

pushLocalFrame :: Int32 -> IO ()
pushLocalFrame (coerce -> capacity) = withJNIEnv $ \env ->
    -- We ignore the output as it is always 0 on success and throws an
    -- exception otherwise.
    throwIfException env $ void $
    [CU.block| jint {
      (*$(JNIEnv *env))->PushLocalFrame($(JNIEnv *env),
                                        $(jint capacity)); } |]

popLocalFrame :: Coercible o (J ty) => o -> IO o
popLocalFrame (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    coerce <$> (objectFromPtr =<<)
    [CU.exp| jobject {
      (*$(JNIEnv *env))->PopLocalFrame($(JNIEnv *env),
                                       $fptr-ptr:(jobject obj)) } |]

-- Modern CPP does have ## for concatenating strings, but we use the hacky /**/
-- comment syntax for string concatenation. This is because GHC passes
-- the -traditional flag to the preprocessor by default, which turns off several
-- modern CPP features.

#define CALL_METHOD(name, hs_rettype, c_rettype) \
call/**/name/**/Method :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO hs_rettype; \
call/**/name/**/Method (coerce -> upcast -> obj) method args = withJNIEnv $ \env -> \
    throwIfException env $ \
    withJValues args $ \cargs -> \
    [C.exp| c_rettype { \
      (*$(JNIEnv *env))->Call/**/name/**/MethodA($(JNIEnv *env), \
                                                 $fptr-ptr:(jobject obj), \
                                                 $(jmethodID method), \
                                                 $(jvalue *cargs)) } |]

CALL_METHOD(Void, (), void)
callObjectMethod :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO JObject
callObjectMethod x y z =
    let CALL_METHOD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< callObjectMethod x y z
callBooleanMethod :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO Bool
callBooleanMethod x y z =
    let CALL_METHOD(Boolean, Word8, jboolean)
    in toEnum . fromIntegral <$> callBooleanMethod x y z
CALL_METHOD(Byte, CChar, jbyte)
CALL_METHOD(Char, Word16, jchar)
CALL_METHOD(Short, Int16, jshort)
CALL_METHOD(Int, Int32, jint)
CALL_METHOD(Long, Int64, jlong)
CALL_METHOD(Float, Float, jfloat)
CALL_METHOD(Double, Double, jdouble)

#define CALL_STATIC_METHOD(name, hs_rettype, c_rettype) \
callStatic/**/name/**/Method :: JClass -> JMethodID -> [JValue] -> IO hs_rettype; \
callStatic/**/name/**/Method cls method args = withJNIEnv $ \env -> \
    throwIfException env $ \
    withJValues args $ \cargs -> \
    [C.exp| c_rettype { \
      (*$(JNIEnv *env))->CallStatic/**/name/**/MethodA($(JNIEnv *env), \
                                                       $fptr-ptr:(jclass cls), \
                                                       $(jmethodID method), \
                                                       $(jvalue *cargs)) } |]

CALL_STATIC_METHOD(Void, (), void)
callStaticObjectMethod :: JClass -> JMethodID -> [JValue] -> IO JObject
callStaticObjectMethod x y z =
    let CALL_STATIC_METHOD(Object, (Ptr JObject), jobject)
    in objectFromPtr =<< callStaticObjectMethod x y z
callStaticBooleanMethod :: JClass -> JMethodID -> [JValue] -> IO Bool
callStaticBooleanMethod x y z =
    let CALL_STATIC_METHOD(Boolean, Word8, jboolean)
    in toEnum . fromIntegral <$> callStaticBooleanMethod x y z
CALL_STATIC_METHOD(Byte, CChar, jbyte)
CALL_STATIC_METHOD(Char, Word16, jchar)
CALL_STATIC_METHOD(Short, Int16, jshort)
CALL_STATIC_METHOD(Int, Int32, jint)
CALL_STATIC_METHOD(Long, Int64, jlong)
CALL_STATIC_METHOD(Float, Float, jfloat)
CALL_STATIC_METHOD(Double, Double, jdouble)

newObjectArray :: Int32 -> JClass -> IO JObjectArray
newObjectArray sz cls = withJNIEnv $ \env ->
    throwIfException env $
    objectFromPtr =<<
    [CU.exp| jobjectArray {
      (*$(JNIEnv *env))->NewObjectArray($(JNIEnv *env),
                                        $(jsize sz),
                                        $fptr-ptr:(jclass cls),
                                        NULL) } |]

#define NEW_ARRAY(name, c_rettype) \
new/**/name/**/Array :: Int32 -> IO J/**/name/**/Array; \
new/**/name/**/Array sz = withJNIEnv $ \env -> \
    throwIfException env $ \
    objectFromPtr =<< \
    [CU.exp| c_rettype/**/Array { \
      (*$(JNIEnv *env))->New/**/name/**/Array($(JNIEnv *env), \
                                              $(jsize sz)) } |]

NEW_ARRAY(Boolean, jboolean)
NEW_ARRAY(Byte, jbyte)
NEW_ARRAY(Char, jchar)
NEW_ARRAY(Short, jshort)
NEW_ARRAY(Int, jint)
NEW_ARRAY(Long, jlong)
NEW_ARRAY(Float, jfloat)
NEW_ARRAY(Double, jdouble)

newString :: Ptr Word16 -> Int32 -> IO JString
newString ptr len = withJNIEnv $ \env ->
    throwIfException env $
    objectFromPtr =<<
    [CU.exp| jstring {
      (*$(JNIEnv *env))->NewString($(JNIEnv *env),
                                   $(jchar *ptr),
                                   $(jsize len)) } |]

getArrayLength :: Coercible o (JArray a) => o -> IO Int32
getArrayLength (coerce -> upcast -> array) = withJNIEnv $ \env ->
    [C.exp| jsize {
      (*$(JNIEnv *env))->GetArrayLength($(JNIEnv *env),
                                        $fptr-ptr:(jarray array)) } |]

getStringLength :: JString -> IO Int32
getStringLength jstr = withJNIEnv $ \env ->
    [CU.exp| jsize {
      (*$(JNIEnv *env))->GetStringLength($(JNIEnv *env),
                                         $fptr-ptr:(jstring jstr)) } |]

#define GET_ARRAY_ELEMENTS(name, hs_rettype, c_rettype) \
get/**/name/**/ArrayElements :: J/**/name/**/Array -> IO (Ptr hs_rettype); \
get/**/name/**/ArrayElements (upcast -> array) = withJNIEnv $ \env -> \
    throwIfNull $ \
    [CU.exp| c_rettype* { \
      (*$(JNIEnv *env))->Get/**/name/**/ArrayElements($(JNIEnv *env), \
                                                      $fptr-ptr:(jobject array), \
                                                      NULL) } |]

GET_ARRAY_ELEMENTS(Boolean, Word8, jboolean)
GET_ARRAY_ELEMENTS(Byte, CChar, jbyte)
GET_ARRAY_ELEMENTS(Char, Word16, jchar)
GET_ARRAY_ELEMENTS(Short, Int16, jshort)
GET_ARRAY_ELEMENTS(Int, Int32, jint)
GET_ARRAY_ELEMENTS(Long, Int64, jlong)
GET_ARRAY_ELEMENTS(Float, Float, jfloat)
GET_ARRAY_ELEMENTS(Double, Double, jdouble)

getStringChars :: JString -> IO (Ptr Word16)
getStringChars jstr = withJNIEnv $ \env ->
    throwIfNull $
    [CU.exp| const jchar* {
      (*$(JNIEnv *env))->GetStringChars($(JNIEnv *env),
                                        $fptr-ptr:(jstring jstr),
                                        NULL) } |]

#define SET_ARRAY_REGION(name, hs_argtype, c_argtype) \
set/**/name/**/ArrayRegion :: J/**/name/**/Array -> Int32 -> Int32 -> Ptr hs_argtype -> IO (); \
set/**/name/**/ArrayRegion array start len buf = withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Set/**/name/**/ArrayRegion($(JNIEnv *env), \
                                                    $fptr-ptr:(c_argtype/**/Array array), \
                                                    $(jsize start), \
                                                    $(jsize len), \
                                                    $(c_argtype *buf)) } |]

SET_ARRAY_REGION(Boolean, Word8, jboolean)
SET_ARRAY_REGION(Byte, CChar, jbyte)
SET_ARRAY_REGION(Char, Word16, jchar)
SET_ARRAY_REGION(Short, Int16, jshort)
SET_ARRAY_REGION(Int, Int32, jint)
SET_ARRAY_REGION(Long, Int64, jlong)
SET_ARRAY_REGION(Float, Float, jfloat)
SET_ARRAY_REGION(Double, Double, jdouble)

#define RELEASE_ARRAY_ELEMENTS(name, hs_argtype, c_argtype) \
release/**/name/**/ArrayElements :: J/**/name/**/Array -> Ptr hs_argtype -> IO (); \
release/**/name/**/ArrayElements (upcast -> array) xs = withJNIEnv $ \env -> \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Release/**/name/**/ArrayElements($(JNIEnv *env), \
                                                          $fptr-ptr:(jobject array), \
                                                          $(c_argtype *xs), \
                                                          JNI_ABORT) } |]

RELEASE_ARRAY_ELEMENTS(Boolean, Word8, jboolean)
RELEASE_ARRAY_ELEMENTS(Byte, CChar, jbyte)
RELEASE_ARRAY_ELEMENTS(Char, Word16, jchar)
RELEASE_ARRAY_ELEMENTS(Short, Int16, jshort)
RELEASE_ARRAY_ELEMENTS(Int, Int32, jint)
RELEASE_ARRAY_ELEMENTS(Long, Int64, jlong)
RELEASE_ARRAY_ELEMENTS(Float, Float, jfloat)
RELEASE_ARRAY_ELEMENTS(Double, Double, jdouble)

releaseStringChars :: JString -> Ptr Word16 -> IO ()
releaseStringChars jstr chars = withJNIEnv $ \env ->
    [CU.exp| void {
      (*$(JNIEnv *env))->ReleaseStringChars($(JNIEnv *env),
                                            $fptr-ptr:(jstring jstr),
                                            $(jchar *chars)) } |]

getObjectArrayElement :: Coercible o (JArray a) => o -> Int32 -> IO (J a)
getObjectArrayElement (coerce -> upcast -> array) i = withJNIEnv $ \env ->
    unsafeCast <$> (objectFromPtr =<<)
    [C.exp| jobject {
      (*$(JNIEnv *env))->GetObjectArrayElement($(JNIEnv *env),
                                               $fptr-ptr:(jarray array),
                                               $(jsize i)) } |]

setObjectArrayElement :: Coercible o (J a) => JObjectArray -> Int32 -> o -> IO ()
setObjectArrayElement array i (coerce -> upcast -> x) = withJNIEnv $ \env ->
    [C.exp| void {
      (*$(JNIEnv *env))->SetObjectArrayElement($(JNIEnv *env),
                                               $fptr-ptr:(jobjectArray array),
                                               $(jsize i),
                                               $fptr-ptr:(jobject x)); } |]
