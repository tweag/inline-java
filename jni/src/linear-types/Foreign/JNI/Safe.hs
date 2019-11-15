-- | Low-level bindings to the Java Native Interface (JNI).
--
-- This module provides a safer interface with linear types to the functions
-- of JNI. The compiler will check that all local references to java
-- objects are eventually deleted. Unlike garbage collection
-- and finalizers, the references are guaranteed to be removed when
-- execution reaches explicit API calls for that sake. See
-- <https://www.tweag.io/posts/2017-11-29-linear-jvm.html this blog post>
-- for the motivation.
--
-- Class references returned by 'findClass' and 'getObjectClass' are not
-- given linear multiplicity. The lifetime of classes is not so efemeral
-- and we expect them to be less prone to leak. For a similar reason,
-- global references aren't given linear multiplity either.
--
-- Read the
-- <https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html JNI spec>
-- for authoritative documentation as to what each of the functions in
-- this module does. The names of the bindings in this module were chosen to
-- match the names of the functions in the JNI spec.
--
-- All bindings in this module access the JNI via a thread-local variable of
-- type @JNIEnv *@. If the current OS thread has not yet been "attached" to the
-- JVM, it needs to be attached. See 'JNI.runInAttachedThread'.
--
-- The 'String' type in this module is the type of JNI strings. See
-- "Foreign.JNI.String".
--
-- == Notes on linearity
--
-- Most functions taking references with linear multiplicty return the same
-- references so they can be used again. One notable exception is the
-- 'deleteLocalRef' function, which deletes a reference without returning it.
--
-- Another exception are the /call/ functions which take a list of 'JValue'
-- (`[JValue]`). /call/ functions delete all references with linear multiplicity
-- in the list of JValues. The references are not returned, so if the
-- caller wants to use them again, they need to be duplicated with
-- 'newLocalRef' before the call. Returning the `[JValue]`s would make it
-- rather clumsy to extract an object reference from it to use it again.
--
-- Because /call/ functions delete their reference arguments, they have
-- to discriminate references with linear multiplicity from the rest.
-- We introduce a new type 'Foreign.JNI.Types.Safe.J' of references with
-- linear multiplicity, and a type 'Foreign.JNI.Types.Safe.JValue' that
-- is a sum of primitive types, unrestricted references and references with
-- linear multiplicity.
--
-- Some functions like 'setObjectArrayElement' or 'setObjectField' offer
-- variants 'setObjectArrayElement_' and 'setObjectField_' which delete
-- one of the objects that the former functions would otherwise return.
--
-- Because we use linear multiplicities with local references only, we can
-- depend on local JNI frames to cleanup references when an exception occurs.
-- For this reason, functions or code blocks which use the JNI interface must
-- be wrapped with either 'withLocalFrame' or 'withLocalFrame_'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- XXX This file uses cpphs for preprocessing instead of the system's native
-- CPP, because the OS X has subtly different whitespace behaviour in the
-- presence of concatenation.

module Foreign.JNI.Safe
  ( module Foreign.JNI.Safe
  , JNI.withJVM
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class.Linear (MonadIO, liftIO, liftIOU)
import qualified Control.Monad.Linear as Linear
import Data.Coerce
import Data.Functor
import Data.Int
import Data.Word
import Foreign.C.Types
import qualified Foreign.JNI as JNI
import qualified Foreign.JNI.String as JNI
import qualified Foreign.JNI.Types as JNI
import Foreign.JNI.Types.Safe
import Foreign.Ptr (Ptr)
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe
import Prelude ((.))
import qualified Prelude
import Prelude.Linear hiding ((<$), (<*), (.))
import qualified Prelude.Linear as Linear ((<$))


throw :: (MonadIO m, Coercible o (J a)) => o ->. m o
throw = Unsafe.toLinear $ \x -> liftIO (x Prelude.<$ JNI.throw x)

throwNew :: MonadIO m => JNI.JClass -> JNI.String ->. m ()
throwNew jclass = Unsafe.toLinear $ \msg ->
    liftIO (JNI.throwNew jclass msg)

findClass :: MonadIO m => ReferenceTypeName -> m (Unrestricted JNI.JClass)
findClass name = liftIO (Unrestricted <$> JNI.findClass name)

newObject
  :: MonadIO m
  => JNI.JClass
  -> MethodSignature
  ->. [JValue]
  ->. m JObject
newObject jclass = Unsafe.toLinear2 $ \m args ->
  liftIO (J <$> JNI.newObject jclass m (toJNIJValues args))

getFieldID
  :: MonadIO m
  => JNI.JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  ->. Signature -- ^ JNI signature
  ->. m (Unrestricted JFieldID)
getFieldID jclass = Unsafe.toLinear2 $ \fieldname sig ->
    liftIO (Unrestricted <$> JNI.getFieldID jclass fieldname sig)

getStaticFieldID
  :: MonadIO m
  => JNI.JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  ->. Signature -- ^ JNI signature
  ->. m (Unrestricted JFieldID)
getStaticFieldID jclass = Unsafe.toLinear2 $ \fieldname sig ->
    liftIO (Unrestricted <$> JNI.getStaticFieldID jclass fieldname sig)

#define GET_FIELD(name, hs_rettype) \
get/**/name/**/Field :: MonadIO m => J a ->. JFieldID ->. m (J a, Unrestricted hs_rettype); \
get/**/name/**/Field = Unsafe.toLinear2 $ \obj field -> \
    liftIO \
      ((,) obj . Unrestricted <$> JNI.get/**/name/**/Field (unJ obj) field)

getObjectField :: MonadIO m => J a ->. JFieldID ->. m (J a, JObject)
getObjectField = Unsafe.toLinear2 $ \obj field ->
    liftIO ((,) obj . J <$> JNI.getObjectField (unJ obj) field)

GET_FIELD(Boolean, Word8)
GET_FIELD(Byte, CChar)
GET_FIELD(Char, Word16)
GET_FIELD(Short, Int16)
GET_FIELD(Int, Int32)
GET_FIELD(Long, Int64)
GET_FIELD(Float, Float)
GET_FIELD(Double, Double)

#define GET_STATIC_FIELD(name, hs_rettype) \
getStatic/**/name/**/Field :: MonadIO m => JNI.JClass -> JFieldID ->. m (Unrestricted hs_rettype); \
getStatic/**/name/**/Field jclass = Unsafe.toLinear $ \field -> \
    liftIOU (JNI.getStatic/**/name/**/Field jclass field)

getStaticObjectField :: MonadIO m => JNI.JClass -> JFieldID ->. m JObject
getStaticObjectField jclass = Unsafe.toLinear $ \field ->
    liftIO (J <$> JNI.getStaticObjectField jclass field)

GET_STATIC_FIELD(Boolean, Word8)
GET_STATIC_FIELD(Byte, CChar)
GET_STATIC_FIELD(Char, Word16)
GET_STATIC_FIELD(Short, Int16)
GET_STATIC_FIELD(Int, Int32)
GET_STATIC_FIELD(Long, Int64)
GET_STATIC_FIELD(Float, Float)
GET_STATIC_FIELD(Double, Double)

#define SET_FIELD(name, hs_fieldtype) \
set/**/name/**/Field :: MonadIO m => J a ->. JFieldID ->. hs_fieldtype ->. m (J a); \
set/**/name/**/Field = Unsafe.toLinear2 $ \obj field -> Unsafe.toLinear $ \v -> \
    liftIO \
      (obj <$ JNI.set/**/name/**/Field (unJ obj) field v)

setObjectField
  :: MonadIO m
  => J a
  ->. JFieldID
  ->. JObject
  ->. m (J a, JObject)
setObjectField = Unsafe.toLinear3 $ \obj field v ->
    liftIO ((obj, v) <$ JNI.setObjectField (unJ obj) field (unJ v))

setObjectField_
  :: MonadIO m
  => J a
  ->. JFieldID
  ->. JObject
  ->. m (J a)
setObjectField_ _o f _v =
    setObjectField _o f _v Linear.>>= \(_o, _v) ->
      _o Linear.<$ deleteLocalRef _v

SET_FIELD(Boolean, Word8)
SET_FIELD(Byte, CChar)
SET_FIELD(Char, Word16)
SET_FIELD(Short, Int16)
SET_FIELD(Int, Int32)
SET_FIELD(Long, Int64)
SET_FIELD(Float, Float)
SET_FIELD(Double, Double)

#define SET_STATIC_FIELD(name, hs_fieldtype) \
setStatic/**/name/**/Field :: MonadIO m => JNI.JClass -> JFieldID ->. hs_fieldtype ->. m (); \
setStatic/**/name/**/Field jclass = Unsafe.toLinear2 $ \field v -> \
    liftIO (JNI.setStatic/**/name/**/Field jclass field v)

setStaticObjectField
  :: MonadIO m
  => JNI.JClass
  -> JFieldID
  ->. JObject
  ->. m JObject
setStaticObjectField jclass =
    Unsafe.toLinear2 $ \field v ->
      liftIO (v <$ JNI.setStaticObjectField jclass field (unJ v))

setStaticObjectField_
  :: MonadIO m
  => JNI.JClass
  -> JFieldID
  ->. JObject
  ->. m ()
setStaticObjectField_ c f _v =
    setStaticObjectField c f _v Linear.>>= deleteLocalRef

SET_STATIC_FIELD(Boolean, Word8)
SET_STATIC_FIELD(Byte, CChar)
SET_STATIC_FIELD(Char, Word16)
SET_STATIC_FIELD(Short, Int16)
SET_STATIC_FIELD(Int, Int32)
SET_STATIC_FIELD(Long, Int64)
SET_STATIC_FIELD(Float, Float)
SET_STATIC_FIELD(Double, Double)

getMethodID
  :: MonadIO m
  => JNI.JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  ->. MethodSignature -- ^ JNI signature
  ->. m (Unrestricted JMethodID)
getMethodID jclass = Unsafe.toLinear2 $ \methodname sig ->
  liftIO (Unrestricted <$> JNI.getMethodID jclass methodname sig)

getStaticMethodID
  :: MonadIO m
  => JNI.JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  ->. MethodSignature -- ^ JNI signature
  ->. m (Unrestricted JMethodID)
getStaticMethodID jclass = Unsafe.toLinear2 $ \methodname sig ->
  liftIOU (JNI.getStaticMethodID jclass methodname sig)

getObjectClass :: MonadIO m => J ty ->. m (J ty, Unrestricted JNI.JClass)
getObjectClass = Unsafe.toLinear $ \o ->
    liftIO ((,) o . Unrestricted <$> JNI.getObjectClass (unJ o))

-- | Creates a global reference to the object referred to by
-- the given reference.
--
-- Arranges for a finalizer to call 'deleteGlobalRef' when the
-- global reference is no longer reachable on the Haskell side.
newGlobalRef :: MonadIO m => J ty ->. m (J ty, Unrestricted (JNI.J ty))
newGlobalRef = Unsafe.toLinear $ \o -> liftIO
    ((,) o . Unrestricted <$> JNI.newGlobalRef (unJ o))

-- | Like 'newGlobalRef' but it deletes the input instead of returning it.
newGlobalRef_ :: MonadIO m => J ty ->. m (Unrestricted (JNI.J ty))
newGlobalRef_ j =
    newGlobalRef j Linear.>>= \(j1, g) -> g Linear.<$ deleteLocalRef j1

deleteGlobalRef :: MonadIO m => JNI.J ty -> m ()
deleteGlobalRef o = liftIO (JNI.deleteGlobalRef o)

-- | Like 'newGlobalRef' but it doesn't attach a finalizer to destroy
-- the reference when it is not longer reachable. Use
-- 'deleteGlobalRefNonFinalized' to destroy this reference.
newGlobalRefNonFinalized
  :: MonadIO m => J ty ->. m (J ty, Unrestricted (JNI.J ty))
newGlobalRefNonFinalized = Unsafe.toLinear $ \o ->
    liftIO ((,) o . Unrestricted <$> JNI.newGlobalRefNonFinalized (unJ o))

-- | Like 'deleteGlobalRef' but it can be used only on references created with
-- 'newGlobalRefNonFinalized'.
deleteGlobalRefNonFinalized :: MonadIO m => J ty -> m ()
deleteGlobalRefNonFinalized o = liftIO (JNI.deleteGlobalRef o)

-- NB: Cannot add a finalizer to local references because it may
-- run in a thread where the reference is not valid.
newLocalRef :: MonadIO m => J ty ->. m (J ty, J ty)
newLocalRef = Unsafe.toLinear $ \o ->
    liftIO ((,) o . J <$> JNI.newLocalRef (unJ o))

deleteLocalRef :: MonadIO m => J ty ->. m ()
deleteLocalRef = Unsafe.toLinear $ \o ->
    liftIO (JNI.deleteLocalRef (unJ o))

-- | Runs the given computation in a local frame, which ensures that
-- if it throws an exception, all live local references created during
-- the computation will be deleted.
withLocalFrame :: Linear.IO (Unrestricted a) -> IO a
withLocalFrame = withLocalFrameWithSize 30

withLocalFrame_ :: Linear.IO () -> IO ()
withLocalFrame_ = withLocalFrameWithSize_ 30

withLocalFrameWithSize :: Int32 -> Linear.IO (Unrestricted a) -> IO a
withLocalFrameWithSize capacity linearIO = do
    bracket_
      (JNI.pushLocalFrame capacity)
      (JNI.popLocalFrame jnull)
      (Linear.withLinearIO linearIO)

withLocalFrameWithSize_ :: Int32 -> Linear.IO () -> IO ()
withLocalFrameWithSize_ capacity linearIO = do
    bracket_
      (JNI.pushLocalFrame capacity)
      (JNI.popLocalFrame jnull)
      (Linear.withLinearIO (linearIO Linear.>> Linear.return (Unrestricted ())))

#define CALL_METHOD(name, hs_rettype) \
call/**/name/**/Method :: MonadIO m => J a ->. JMethodID ->. [JValue] ->. m (J a, Unrestricted hs_rettype); \
call/**/name/**/Method = Unsafe.toLinear3 $ \obj method args -> \
    liftIO Prelude.$ \
      (,) obj . Unrestricted <$> JNI.call/**/name/**/Method (unJ obj) method (toJNIJValues args) \
       Prelude.<* deleteLinearJObjects args

deleteLinearJObjects :: [JValue] -> IO ()
deleteLinearJObjects = mapM_ Prelude.$ \case
    JObject j ->  (JNI.deleteLocalRef j)
    _ -> return ()

callVoidMethod :: MonadIO m => J a ->. JMethodID ->. [JValue] ->. m (J a)
callVoidMethod = Unsafe.toLinear3 $ \obj method args ->
    liftIO Prelude.$
      obj <$ JNI.callVoidMethod (unJ obj) method (toJNIJValues args)
        Prelude.<* deleteLinearJObjects args

callObjectMethod
  :: MonadIO m
  => J a
  ->. JMethodID
  ->. [JValue]
  ->. m (J a, JObject)
callObjectMethod = Unsafe.toLinear3 $ \obj method args ->
    liftIO Prelude.$
      (,) obj . J <$> JNI.callObjectMethod (unJ obj) method (toJNIJValues args)
        Prelude.<* deleteLinearJObjects args

CALL_METHOD(Boolean, Bool)
CALL_METHOD(Byte, CChar)
CALL_METHOD(Char, Word16)
CALL_METHOD(Short, Int16)
CALL_METHOD(Int, Int32)
CALL_METHOD(Long, Int64)
CALL_METHOD(Float, Float)
CALL_METHOD(Double, Double)

#define CALL_STATIC_METHOD(name, hs_rettype) \
callStatic/**/name/**/Method :: MonadIO m => JNI.JClass -> JMethodID ->. [JValue] ->. m (Unrestricted hs_rettype); \
callStatic/**/name/**/Method cls = Unsafe.toLinear2 $ \method args -> \
    liftIOU Prelude.$ \
      JNI.callStatic/**/name/**/Method cls method (toJNIJValues args) \
        Prelude.<* deleteLinearJObjects args

callStaticObjectMethod
  :: MonadIO m
  => JNI.JClass
  -> JMethodID
  ->. [JValue]
  ->. m JObject
callStaticObjectMethod jclass = Unsafe.toLinear2 $ \method args ->
    liftIO Prelude.$ do
      J <$> JNI.callStaticObjectMethod jclass method (toJNIJValues args)
        Prelude.<* deleteLinearJObjects args

CALL_STATIC_METHOD(Void, ())
CALL_STATIC_METHOD(Boolean, Bool)
CALL_STATIC_METHOD(Byte, CChar)
CALL_STATIC_METHOD(Char, Word16)
CALL_STATIC_METHOD(Short, Int16)
CALL_STATIC_METHOD(Int, Int32)
CALL_STATIC_METHOD(Long, Int64)
CALL_STATIC_METHOD(Float, Float)
CALL_STATIC_METHOD(Double, Double)

newObjectArray :: MonadIO m => Int32 -> JNI.JClass -> m JObjectArray
newObjectArray sz cls = liftIO (J <$> JNI.newObjectArray sz cls)

#define NEW_ARRAY(name) \
new/**/name/**/Array :: MonadIO m => Int32 -> m J/**/name/**/Array; \
new/**/name/**/Array sz = liftIO (J <$> JNI.new/**/name/**/Array sz)

NEW_ARRAY(Boolean)
NEW_ARRAY(Byte)
NEW_ARRAY(Char)
NEW_ARRAY(Short)
NEW_ARRAY(Int)
NEW_ARRAY(Long)
NEW_ARRAY(Float)
NEW_ARRAY(Double)

newString :: MonadIO m => Ptr Word16 -> Int32 -> m JString
newString ptr len = liftIO (J <$> JNI.newString ptr len)

getArrayLength :: MonadIO m => JArray a ->. m (JArray a, Unrestricted Int32)
getArrayLength = Unsafe.toLinear $ \o ->
    liftIO ((,) o . Unrestricted <$> JNI.getArrayLength (unJ o))

getStringLength :: MonadIO m => JString ->. m (JString, Int32)
getStringLength = Unsafe.toLinear $ \o ->
    liftIO ((,) o <$> JNI.getStringLength (unJ o))

#define GET_ARRAY_ELEMENTS(name, hs_rettype) \
get/**/name/**/ArrayElements :: MonadIO m => J/**/name/**/Array ->. m (J/**/name/**/Array, Unrestricted (Ptr hs_rettype)); \
get/**/name/**/ArrayElements = Unsafe.toLinear $ \a -> \
      liftIO Prelude.$ \
        (,) a . Unrestricted <$> \
          JNI.get/**/name/**/ArrayElements (unJ a)

GET_ARRAY_ELEMENTS(Boolean, Word8)
GET_ARRAY_ELEMENTS(Byte, CChar)
GET_ARRAY_ELEMENTS(Char, Word16)
GET_ARRAY_ELEMENTS(Short, Int16)
GET_ARRAY_ELEMENTS(Int, Int32)
GET_ARRAY_ELEMENTS(Long, Int64)
GET_ARRAY_ELEMENTS(Float, Float)
GET_ARRAY_ELEMENTS(Double, Double)

getStringChars :: MonadIO m => JString ->. m (JString, Ptr Word16)
getStringChars = Unsafe.toLinear $ \jstr ->
    liftIO ((,) jstr <$> JNI.getStringChars (unJ jstr))

#define SET_ARRAY_REGION(name, hs_argtype) \
set/**/name/**/ArrayRegion :: MonadIO m => J/**/name/**/Array ->. Int32 -> Int32 -> Ptr hs_argtype -> m J/**/name/**/Array; \
set/**/name/**/ArrayRegion = Unsafe.toLinear $ \array start len buf -> \
    liftIO (array <$ JNI.set/**/name/**/ArrayRegion (unJ array) start len buf)

SET_ARRAY_REGION(Boolean, Word8)
SET_ARRAY_REGION(Byte, CChar)
SET_ARRAY_REGION(Char, Word16)
SET_ARRAY_REGION(Short, Int16)
SET_ARRAY_REGION(Int, Int32)
SET_ARRAY_REGION(Long, Int64)
SET_ARRAY_REGION(Float, Float)
SET_ARRAY_REGION(Double, Double)

#define RELEASE_ARRAY_ELEMENTS(name, hs_argtype) \
release/**/name/**/ArrayElements :: MonadIO m => J/**/name/**/Array ->. Ptr hs_argtype ->. m J/**/name/**/Array; \
release/**/name/**/ArrayElements = Unsafe.toLinear2 $ \array xs -> \
    liftIO (array <$ JNI.release/**/name/**/ArrayElements (unJ array) xs)

RELEASE_ARRAY_ELEMENTS(Boolean, Word8)
RELEASE_ARRAY_ELEMENTS(Byte, CChar)
RELEASE_ARRAY_ELEMENTS(Char, Word16)
RELEASE_ARRAY_ELEMENTS(Short, Int16)
RELEASE_ARRAY_ELEMENTS(Int, Int32)
RELEASE_ARRAY_ELEMENTS(Long, Int64)
RELEASE_ARRAY_ELEMENTS(Float, Float)
RELEASE_ARRAY_ELEMENTS(Double, Double)

releaseStringChars :: MonadIO m => JString ->. Ptr Word16 -> m JString
releaseStringChars = Unsafe.toLinear $ \jstr chars ->
    liftIO (jstr <$ JNI.releaseStringChars (unJ jstr) chars)

getObjectArrayElement
  :: (IsReferenceType a, MonadIO m)
  => JArray a
  ->. Int32
  ->. m (JArray a, J a)
getObjectArrayElement = Unsafe.toLinear2 $ \a i ->
    liftIO ((,) a . J <$> JNI.getObjectArrayElement (unJ a) i)

setObjectArrayElement
  :: (IsReferenceType a, MonadIO m)
  => JArray a
  ->. Int32
  -> J a
  ->. m (JArray a, J a)
setObjectArrayElement = Unsafe.toLinear $ \a i -> Unsafe.toLinear $ \o ->
    liftIO ((a, o) <$ JNI.setObjectArrayElement (unJ a) i (unJ o))

setObjectArrayElement_
  :: (IsReferenceType a, MonadIO m)
  => JArray a
  ->. Int32
  -> J a
  ->. m (JArray a)
setObjectArrayElement_ _a i _j =
    setObjectArrayElement _a i _j Linear.>>= \(_a, _j) ->
      _a Linear.<$ deleteLocalRef _j
