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

{-# OPTIONS_GHC -fno-warn-name-shadowing -F -pgmF JNI_CAT_TOKENS_PATH #-}

-- XXX This file uses #### for concatenating tokens with a preprocessor in
-- a portable way. See note cat-tokens in jni/cat-tokens/Main.hs.

module Foreign.JNI.Unsafe.Internal
  ( -- * JNI functions
    -- ** VM management
    withJVM
  , newJVM
  , destroyJVM
  , startFinalizerThread
  , stopFinalizerThread
  , getVersion
  , getJavaVM
  , setJVM
    -- ** Class loading
  , defineClass
  , JNINativeMethod(..)
  , registerNatives
    -- ** String wrappers
  , ReferenceTypeName
  , MethodSignature
  , Signature
    -- ** Exceptions
  , JVMException(..)
  , throw
  , throwNew
    -- ** Query functions
  , findClass
  , getFieldID
  , getStaticFieldID
  , getMethodID
  , getStaticMethodID
  , getObjectClass
  , isInstanceOf
    -- ** Reference manipulation
  , newGlobalRef
  , deleteGlobalRef
  , newGlobalRefNonFinalized
  , deleteGlobalRefNonFinalized
  , newLocalRef
  , deleteLocalRef
  , isSameObject
  , pushLocalFrame
  , popLocalFrame
  , submitToFinalizerThread
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
  , ArrayCopyFailed(..)
  , NullPointerException(..)
  , getBooleanArrayElements
  , getByteArrayElements
  , getCharArrayElements
  , getShortArrayElements
  , getIntArrayElements
  , getLongArrayElements
  , getFloatArrayElements
  , getDoubleArrayElements
  , getStringChars
  , getBooleanArrayRegion
  , getByteArrayRegion
  , getCharArrayRegion
  , getShortArrayRegion
  , getIntArrayRegion
  , getLongArrayRegion
  , getFloatArrayRegion
  , getDoubleArrayRegion
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
    -- * Thread management
  , attachCurrentThreadAsDaemon
  , detachCurrentThread
  , isCurrentThreadAttached
  , runInAttachedThread
  , ThreadNotAttached(..)
    -- * NIO support
  , DirectBufferFailed(..)
  , newDirectByteBuffer
  , getDirectBufferAddress
  , getDirectBufferCapacity
  ) where

import Control.Exception
  ( Exception
  , bracket
  , finally
  , throwIO
  )
import Control.Monad (unless, void, when)
import Data.Choice
import Data.Coerce
import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import Foreign.C (CChar)
import Foreign.ForeignPtr
  ( finalizeForeignPtr
  , newForeignPtr_
  , withForeignPtr
  )
import Foreign.JNI.Internal
import qualified Foreign.JNI.Internal.RWLock as RWLock
import Foreign.JNI.NativeMethod
import Foreign.JNI.Types
import qualified Foreign.JNI.String as JNI
import Foreign.JNI.Unsafe.Internal.JVM
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import GHC.ForeignPtr (newConcForeignPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.IO (fixIO)
import Prelude hiding (String)


C.context (C.baseCtx <> C.bsCtx <> jniCtx)

C.include "<jni.h>"
C.include "<stdio.h>"
C.include "<errno.h>"
C.include "<stdlib.h>"

-- | A JNI call may cause a (Java) exception to be raised. This module raises it
-- as a Haskell exception wrapping the Java exception.
newtype JVMException = JVMException JThrowable

instance Exception JVMException

instance Show JVMException where
  show (JVMException e) = show e ++ ": Call (Foreign.JNI.showException e) to see details."

-- | Thrown when @Get<PrimitiveType>ArrayElements@ returns a null pointer,
-- because it wanted to copy the array contents but couldn't. In this case the
-- JVM doesn't throw OutOfMemory according to the JNI spec.
data ArrayCopyFailed = ArrayCopyFailed
  deriving (Exception, Show)

-- Thrown when @NewDirectByteBuffer@ or @GetDirectBufferAddress@ returns NULL,
-- and when @GetDirectBufferCapacity@ return @-1@.
data DirectBufferFailed = DirectBufferFailed
  deriving (Exception, Show)

-- | A null reference is found where a non-null reference was expected.
data NullPointerException = NullPointerException
  deriving (Exception, Show)

-- | Map Java exceptions to Haskell exceptions.
throwIfException :: Ptr JNIEnv -> IO a -> IO a
throwIfException env m = m `finally` do
    excptr <- [CU.exp| jthrowable { (*$(JNIEnv *env))->ExceptionOccurred($(JNIEnv *env)) } |]
    unless (excptr == nullPtr) $ do
      [CU.exp| void { (*$(JNIEnv *env))->ExceptionClear($(JNIEnv *env)) } |]
      throwIO . JVMException =<< newGlobalRef =<< objectFromPtr excptr

-- | Check whether a pointer is null.
throwIfNull :: Exception e => e -> IO (Ptr a) -> IO (Ptr a)
throwIfNull e m = do
    ptr <- m
    if ptr == nullPtr
    then throwIO e
    else return ptr

-- | Throws an error if the given reference is null, otherwise performs
-- the given io action.
throwIfJNull :: J ty -> IO a -> IO a
throwIfJNull j io =
    if j == jnull
    then throwIO NullPointerException
    else io

defineClass
  :: Coercible o (J ('Class "java.lang.ClassLoader"))
  => ReferenceTypeName -- ^ Class name
  -> o -- ^ Loader
  -> ByteString -- ^ Bytecode buffer
  -> IO JClass
defineClass (coerce -> name) (coerce -> upcast -> loader) buf = withJNIEnv $ \env ->
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
throwNew cls msg = throwIfJNull cls $ withJNIEnv $ \env ->
    JNI.withString msg $ \msgp -> void $ do
    [CU.exp| jint {
       (*$(JNIEnv *env))->ThrowNew($(JNIEnv *env),
                                   $fptr-ptr:(jclass cls),
                                   $(char *msgp)) } |]

findClass
  :: ReferenceTypeName -- ^ Class name
  -> IO JClass
findClass (coerce -> name) = withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString name $ \namep ->
    objectFromPtr =<<
    [CU.exp| jclass { (*$(JNIEnv *env))->FindClass($(JNIEnv *env), $(char *namep)) } |]

newObject :: JClass -> MethodSignature -> [JValue] -> IO JObject
newObject cls (coerce -> sig) args = throwIfJNull cls $ withJNIEnv $ \env ->
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
  -> Signature -- ^ JNI signature
  -> IO JFieldID
getFieldID cls fieldname (coerce -> sig) = throwIfJNull cls $
    withJNIEnv $ \env ->
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
  -> Signature -- ^ JNI signature
  -> IO JFieldID
getStaticFieldID cls fieldname (coerce -> sig) = throwIfJNull cls $
    withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString fieldname $ \fieldnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jfieldID {
      (*$(JNIEnv *env))->GetStaticFieldID($(JNIEnv *env),
                                          $fptr-ptr:(jclass cls),
                                          $(char *fieldnamep),
                                          $(char *sigp)) } |]

#define GET_FIELD(name, hs_rettype, c_rettype) \
get####name####Field :: Coercible o (J a) => o -> JFieldID -> IO hs_rettype; \
get####name####Field (coerce -> upcast -> obj) field = withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| c_rettype { \
      (*$(JNIEnv *env))->Get####name####Field($(JNIEnv *env), \
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
getStatic####name####Field :: JClass -> JFieldID -> IO hs_rettype; \
getStatic####name####Field klass field = throwIfJNull klass $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| c_rettype { \
      (*$(JNIEnv *env))->GetStatic####name####Field($(JNIEnv *env), \
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
set####name####Field :: Coercible o (J a) => o -> JFieldID -> hs_fieldtype -> IO (); \
set####name####Field (coerce -> upcast -> obj) field x = \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.block| void { \
      (*$(JNIEnv *env))->Set####name####Field($(JNIEnv *env), \
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
setStatic####name####Field :: JClass -> JFieldID -> hs_fieldtype -> IO (); \
setStatic####name####Field klass field x = throwIfJNull klass $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.block| void { \
      (*$(JNIEnv *env))->SetStatic####name####Field($(JNIEnv *env), \
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
  -> MethodSignature -- ^ JNI signature
  -> IO JMethodID
getMethodID cls methodname (coerce -> sig) = throwIfJNull cls $
    withJNIEnv $ \env ->
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
  -> MethodSignature -- ^ JNI signature
  -> IO JMethodID
getStaticMethodID cls methodname (coerce -> sig) = throwIfJNull cls $
    withJNIEnv $ \env ->
    throwIfException env $
    JNI.withString methodname $ \methodnamep ->
    JNI.withString sig $ \sigp ->
    [CU.exp| jmethodID {
      (*$(JNIEnv *env))->GetStaticMethodID($(JNIEnv *env),
                                           $fptr-ptr:(jclass cls),
                                           $(char *methodnamep),
                                           $(char *sigp)) } |]

getObjectClass :: Coercible o (J ty) => o -> IO JClass
getObjectClass (coerce -> upcast -> obj) = throwIfJNull obj $
    withJNIEnv $ \env ->
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
    fixIO $ \j ->
      coerce <$> J <$>
        newConcForeignPtr gobj
          (submitToFinalizerThread $ deleteGlobalRefNonFinalized j)

deleteGlobalRef :: Coercible o (J ty) => o -> IO ()
deleteGlobalRef (coerce -> J p) = finalizeForeignPtr p

-- | Like 'newGlobalRef' but it doesn't attach a finalizer to destroy
-- the reference when it is not longer reachable. Use
-- 'deleteGlobalRefNonFinalized' to destroy this reference.
newGlobalRefNonFinalized :: Coercible o (J ty) => o -> IO o
newGlobalRefNonFinalized (coerce -> upcast -> obj) = withJNIEnv $ \env -> do
    gobj <-
      [CU.exp| jobject {
        (*$(JNIEnv *env))->NewGlobalRef($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]
    coerce <$> J <$> newForeignPtr_ gobj

-- | Like 'deleteGlobalRef' but it can be used only on references created with
-- 'newGlobalRefNonFinalized'.
deleteGlobalRefNonFinalized :: Coercible o (J ty) => o -> IO ()
deleteGlobalRefNonFinalized (coerce -> upcast -> obj) = do
    bracket (RWLock.tryAcquireReadLock globalJVMLock)
            (\doRead -> when (toBool doRead) $ RWLock.releaseReadLock globalJVMLock)
            $ \doRead ->
      when (toBool doRead) $ withJNIEnv $ \env -> do
        [CU.block| void { (*$(JNIEnv *env))->DeleteGlobalRef($(JNIEnv *env)
                                                            ,$fptr-ptr:(jobject obj));
                        } |]

-- NB: Cannot add a finalizer to local references because it may
-- run in a thread where the reference is not valid.
newLocalRef :: Coercible o (J ty) => o -> IO o
newLocalRef (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    coerce <$> (objectFromPtr =<<)
    [CU.exp| jobject {
      (*$(JNIEnv *env))->NewLocalRef($(JNIEnv *env),
                                     $fptr-ptr:(jobject obj)) } |]

deleteLocalRef :: Coercible o (J ty) => o -> IO ()
deleteLocalRef (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    [CU.exp| void {
      (*$(JNIEnv *env))->DeleteLocalRef($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj)) } |]

isSameObject :: Coercible o (J ty) => o -> o -> IO Bool
isSameObject (coerce -> upcast -> obj1) (coerce -> upcast -> obj2) = do
    w <- withJNIEnv $ \env ->
      [CU.exp| jboolean {
        (*$(JNIEnv *env))->IsSameObject($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj1),
                                        $fptr-ptr:(jobject obj2)) } |]
    return $ toEnum $ fromIntegral w

pushLocalFrame :: Int32 -> IO ()
pushLocalFrame (coerce -> capacity) = withJNIEnv $ \env ->
    -- We ignore the output as it is always 0 on success and throws an
    -- exception otherwise.
    throwIfException env $ void $
    [CU.block| void {
      (*$(JNIEnv *env))->PushLocalFrame($(JNIEnv *env),
                                        $(jint capacity)); } |]

popLocalFrame :: Coercible o (J ty) => o -> IO o
popLocalFrame (coerce -> upcast -> obj) = withJNIEnv $ \env ->
    coerce <$> (objectFromPtr =<<)
    [CU.exp| jobject {
      (*$(JNIEnv *env))->PopLocalFrame($(JNIEnv *env),
                                       $fptr-ptr:(jobject obj)) } |]

-- Modern CPP does have ## for concatenating strings, but we use the hacky ####
-- comment syntax for string concatenation. This is because GHC passes
-- the -traditional flag to the preprocessor by default, which turns off several
-- modern CPP features.

#define CALL_METHOD(name, hs_rettype, c_rettype) \
call####name####Method :: Coercible o (J a) => o -> JMethodID -> [JValue] -> IO hs_rettype; \
call####name####Method (coerce -> upcast -> obj) method args = withJNIEnv $ \env -> \
    throwIfException env $ \
    withJValues args $ \cargs -> \
    [C.exp| c_rettype { \
      (*$(JNIEnv *env))->Call####name####MethodA($(JNIEnv *env), \
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
callStatic####name####Method :: JClass -> JMethodID -> [JValue] -> IO hs_rettype; \
callStatic####name####Method cls method args = throwIfJNull cls $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    withJValues args $ \cargs -> \
    [C.exp| c_rettype { \
      (*$(JNIEnv *env))->CallStatic####name####MethodA($(JNIEnv *env), \
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
newObjectArray sz cls = throwIfJNull cls $ withJNIEnv $ \env ->
    throwIfException env $
    objectFromPtr =<<
    [CU.exp| jobjectArray {
      (*$(JNIEnv *env))->NewObjectArray($(JNIEnv *env),
                                        $(jsize sz),
                                        $fptr-ptr:(jclass cls),
                                        NULL) } |]

#define NEW_ARRAY(name, c_rettype) \
new####name####Array :: Int32 -> IO J####name####Array; \
new####name####Array sz = withJNIEnv $ \env -> \
    throwIfException env $ \
    objectFromPtr =<< \
    [CU.exp| c_rettype####Array { \
      (*$(JNIEnv *env))->New####name####Array($(JNIEnv *env), \
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
getArrayLength (coerce -> upcast -> array) = throwIfJNull array $
    withJNIEnv $ \env ->
    [C.exp| jsize {
      (*$(JNIEnv *env))->GetArrayLength($(JNIEnv *env),
                                        $fptr-ptr:(jarray array)) } |]

getStringLength :: JString -> IO Int32
getStringLength jstr = throwIfJNull jstr $ withJNIEnv $ \env ->
    [CU.exp| jsize {
      (*$(JNIEnv *env))->GetStringLength($(JNIEnv *env),
                                         $fptr-ptr:(jstring jstr)) } |]

#define GET_ARRAY_ELEMENTS(name, hs_rettype, c_rettype) \
get####name####ArrayElements :: J####name####Array -> IO (Ptr hs_rettype); \
get####name####ArrayElements (upcast -> array) = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    throwIfNull ArrayCopyFailed $ \
    [CU.exp| c_rettype* { \
      (*$(JNIEnv *env))->Get####name####ArrayElements($(JNIEnv *env), \
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
getStringChars jstr = throwIfJNull jstr $ withJNIEnv $ \env ->
    throwIfNull ArrayCopyFailed $
    [CU.exp| const jchar* {
      (*$(JNIEnv *env))->GetStringChars($(JNIEnv *env),
                                        $fptr-ptr:(jstring jstr),
                                        NULL) } |]

#define GET_ARRAY_REGION(name, hs_argtype, c_argtype) \
get####name####ArrayRegion :: J####name####Array -> Int32 -> Int32 -> Ptr hs_argtype -> IO (); \
get####name####ArrayRegion array start len buf = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Get####name####ArrayRegion($(JNIEnv *env), \
                                                    $fptr-ptr:(c_argtype####Array array), \
                                                    $(jsize start), \
                                                    $(jsize len), \
                                                    $(c_argtype *buf)) } |]

GET_ARRAY_REGION(Boolean, Word8, jboolean)
GET_ARRAY_REGION(Byte, CChar, jbyte)
GET_ARRAY_REGION(Char, Word16, jchar)
GET_ARRAY_REGION(Short, Int16, jshort)
GET_ARRAY_REGION(Int, Int32, jint)
GET_ARRAY_REGION(Long, Int64, jlong)
GET_ARRAY_REGION(Float, Float, jfloat)
GET_ARRAY_REGION(Double, Double, jdouble)

#define SET_ARRAY_REGION(name, hs_argtype, c_argtype) \
set####name####ArrayRegion :: J####name####Array -> Int32 -> Int32 -> Ptr hs_argtype -> IO (); \
set####name####ArrayRegion array start len buf = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    throwIfException env $ \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Set####name####ArrayRegion($(JNIEnv *env), \
                                                    $fptr-ptr:(c_argtype####Array array), \
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
release####name####ArrayElements :: J####name####Array -> Ptr hs_argtype -> IO (); \
release####name####ArrayElements (upcast -> array) xs = throwIfJNull array $ \
    withJNIEnv $ \env -> \
    [CU.exp| void { \
      (*$(JNIEnv *env))->Release####name####ArrayElements($(JNIEnv *env), \
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
releaseStringChars jstr chars = throwIfJNull jstr $ withJNIEnv $ \env ->
    [CU.exp| void {
      (*$(JNIEnv *env))->ReleaseStringChars($(JNIEnv *env),
                                            $fptr-ptr:(jstring jstr),
                                            $(jchar *chars)) } |]

getObjectArrayElement
  :: forall a o.
     (IsReferenceType a, Coercible o (J a))
  => JArray a
  -> Int32
  -> IO o
getObjectArrayElement (arrayUpcast -> array) i = throwIfJNull array $
    withJNIEnv $ \env ->
    ( (coerce :: J a -> o)
    . (unsafeCast :: JObject -> J a)
    ) <$> (objectFromPtr =<<)
    [C.exp| jobject {
      (*$(JNIEnv *env))->GetObjectArrayElement($(JNIEnv *env),
                                               $fptr-ptr:(jarray array),
                                               $(jsize i)) } |]

setObjectArrayElement
  :: forall a o.
     (IsReferenceType a, Coercible o (J a))
  => JArray a
  -> Int32
  -> o
  -> IO ()
setObjectArrayElement (arrayUpcast -> array)
                      i
                      ((coerce :: o -> J a) -> upcast -> x) =
    throwIfJNull array $
    withJNIEnv $ \env ->
    [C.exp| void {
      (*$(JNIEnv *env))->SetObjectArrayElement($(JNIEnv *env),
                                               $fptr-ptr:(jobjectArray array),
                                               $(jsize i),
                                               $fptr-ptr:(jobject x)); } |]

newDirectByteBuffer :: Ptr CChar -> Int64 -> IO JByteBuffer
newDirectByteBuffer (castPtr -> address) capacity =
    (throwIfNull NullPointerException (return address) >>) $
    withJNIEnv $ \env ->
    fmap (unsafeCast :: JObject -> JByteBuffer) $
    (objectFromPtr =<<) $
    throwIfNull DirectBufferFailed $
    [C.exp| jobject {
      (*$(JNIEnv *env))->NewDirectByteBuffer($(JNIEnv *env),
                                             $(void *address),
                                             $(jlong capacity)) } |]

getDirectBufferAddress :: JByteBuffer -> IO (Ptr CChar)
getDirectBufferAddress (upcast -> jbuffer) =
    throwIfJNull jbuffer $
    withJNIEnv $ \env ->
    fmap castPtr $
    throwIfNull DirectBufferFailed $
    [C.exp| void* {
      (*$(JNIEnv *env))->GetDirectBufferAddress($(JNIEnv *env),
                                                $fptr-ptr:(jobject jbuffer)) } |]

getDirectBufferCapacity :: JByteBuffer -> IO Int64
getDirectBufferCapacity (upcast -> jbuffer) = do
    capacity <- throwIfJNull jbuffer $
      withJNIEnv $ \env ->
      [C.exp| jlong {
        (*$(JNIEnv *env))->GetDirectBufferCapacity($(JNIEnv *env),
                                                   $fptr-ptr:(jobject jbuffer)) } |]
    if capacity >= 0 then
      return capacity
    else
      throwIO DirectBufferFailed

isInstanceOf :: Coercible o (J ty) => o -> JClass -> IO Bool
isInstanceOf (coerce -> upcast -> obj) cls = do
    w <- throwIfJNull obj $ throwIfJNull cls $
      withJNIEnv $ \env ->
      [C.exp| jboolean {
        (*$(JNIEnv *env))->IsInstanceOf($(JNIEnv *env),
                                        $fptr-ptr:(jobject obj),
                                        $fptr-ptr:(jclass cls)) } |]
    return $ toEnum $ fromIntegral w
