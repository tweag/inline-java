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


throw :: Coercible o (J a) => o ->. Linear.IO o
throw = Unsafe.toLinear $ \x -> Linear.fromSystemIO $ x Prelude.<$ JNI.throw x

throwNew :: JNI.JClass -> JNI.String ->. Linear.IO ()
throwNew jclass = Unsafe.toLinear $ \msg ->
    Linear.fromSystemIO $ JNI.throwNew jclass msg

findClass :: ReferenceTypeName -> Linear.IO (Unrestricted JNI.JClass)
findClass name = Linear.fromSystemIO $ Unrestricted <$> JNI.findClass name

newObject
  :: JNI.JClass
  -> MethodSignature
  ->. [JValue]
  ->. Linear.IO JObject
newObject jclass = Unsafe.toLinear2 $ \m args ->
  Linear.fromSystemIO $ J <$> JNI.newObject jclass m (toJNIJValues args)

getFieldID
  :: JNI.JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  ->. Signature -- ^ JNI signature
  ->. Linear.IO (Unrestricted JFieldID)
getFieldID jclass = Unsafe.toLinear2 $ \fieldname sig ->
    Linear.fromSystemIO $ Unrestricted <$> JNI.getFieldID jclass fieldname sig

getStaticFieldID
  :: JNI.JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  ->. Signature -- ^ JNI signature
  ->. Linear.IO (Unrestricted JFieldID)
getStaticFieldID jclass = Unsafe.toLinear2 $ \fieldname sig ->
    Linear.fromSystemIO $
      Unrestricted <$> JNI.getStaticFieldID jclass fieldname sig

#define GET_FIELD(name, hs_rettype) \
get/**/name/**/Field :: J a ->. JFieldID ->. Linear.IO (J a, Unrestricted hs_rettype); \
get/**/name/**/Field = Unsafe.toLinear2 $ \obj field -> \
    Linear.fromSystemIO $ \
      (,) obj . Unrestricted <$> JNI.get/**/name/**/Field (unJ obj) field

getObjectField :: J a ->. JFieldID ->. Linear.IO (J a, JObject)
getObjectField = Unsafe.toLinear2 $ \obj field ->
    Linear.fromSystemIO $ (,) obj . J <$> JNI.getObjectField (unJ obj) field

GET_FIELD(Boolean, Word8)
GET_FIELD(Byte, CChar)
GET_FIELD(Char, Word16)
GET_FIELD(Short, Int16)
GET_FIELD(Int, Int32)
GET_FIELD(Long, Int64)
GET_FIELD(Float, Float)
GET_FIELD(Double, Double)

#define GET_STATIC_FIELD(name, hs_rettype) \
getStatic/**/name/**/Field :: JNI.JClass -> JFieldID ->. Linear.IO (Unrestricted hs_rettype); \
getStatic/**/name/**/Field jclass = Unsafe.toLinear $ \field -> \
    Linear.fromSystemIOU (JNI.getStatic/**/name/**/Field jclass field)

getStaticObjectField :: JNI.JClass -> JFieldID ->. Linear.IO JObject
getStaticObjectField jclass = Unsafe.toLinear $ \field ->
    Linear.fromSystemIO $ J <$> JNI.getStaticObjectField jclass field

GET_STATIC_FIELD(Boolean, Word8)
GET_STATIC_FIELD(Byte, CChar)
GET_STATIC_FIELD(Char, Word16)
GET_STATIC_FIELD(Short, Int16)
GET_STATIC_FIELD(Int, Int32)
GET_STATIC_FIELD(Long, Int64)
GET_STATIC_FIELD(Float, Float)
GET_STATIC_FIELD(Double, Double)

#define SET_FIELD(name, hs_fieldtype) \
set/**/name/**/Field :: J a ->. JFieldID ->. hs_fieldtype ->. Linear.IO (J a); \
set/**/name/**/Field = Unsafe.toLinear2 $ \obj field -> Unsafe.toLinear $ \v -> \
    Linear.fromSystemIO $ \
      obj <$ JNI.set/**/name/**/Field (unJ obj) field v

setObjectField
  :: J a
  ->. JFieldID
  ->. JObject
  ->. Linear.IO (J a, JObject)
setObjectField = Unsafe.toLinear3 $ \obj field v ->
    Linear.fromSystemIO $
      (obj, v) <$ JNI.setObjectField (unJ obj) field (unJ v)

setObjectField_
  :: J a
  ->. JFieldID
  ->. JObject
  ->. Linear.IO (J a)
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
setStatic/**/name/**/Field :: JNI.JClass -> JFieldID ->. hs_fieldtype ->. Linear.IO (); \
setStatic/**/name/**/Field jclass = Unsafe.toLinear2 $ \field v -> \
    Linear.fromSystemIO $ JNI.setStatic/**/name/**/Field jclass field v

setStaticObjectField
  :: JNI.JClass
  -> JFieldID
  ->. JObject
  ->. Linear.IO JObject
setStaticObjectField jclass =
    Unsafe.toLinear2 $ \field v ->
      Linear.fromSystemIO $ v <$ JNI.setStaticObjectField jclass field (unJ v)

setStaticObjectField_
  :: JNI.JClass
  -> JFieldID
  ->. JObject
  ->. Linear.IO ()
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
  :: JNI.JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  ->. MethodSignature -- ^ JNI signature
  ->. Linear.IO (Unrestricted JMethodID)
getMethodID jclass = Unsafe.toLinear2 $ \methodname sig ->
  Linear.fromSystemIO $ Unrestricted <$> JNI.getMethodID jclass methodname sig

getStaticMethodID
  :: JNI.JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  ->. MethodSignature -- ^ JNI signature
  ->. Linear.IO (Unrestricted JMethodID)
getStaticMethodID jclass = Unsafe.toLinear2 $ \methodname sig ->
  Linear.fromSystemIOU (JNI.getStaticMethodID jclass methodname sig)

getObjectClass :: J ty ->. Linear.IO (J ty, Unrestricted JNI.JClass)
getObjectClass = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ (,) o . Unrestricted <$> JNI.getObjectClass (unJ o)

-- | Creates a global reference to the object referred to by
-- the given reference.
--
-- Arranges for a finalizer to call 'deleteGlobalRef' when the
-- global reference is no longer reachable on the Haskell side.
newGlobalRef :: J ty ->. Linear.IO (J ty, Unrestricted (JNI.J ty))
newGlobalRef = Unsafe.toLinear $ \o -> Linear.fromSystemIO
    ((,) o . Unrestricted <$> JNI.newGlobalRef (unJ o))

-- | Like 'newGlobalRef' but it deletes the input instead of returning it.
newGlobalRef_ :: J ty ->. Linear.IO (Unrestricted (JNI.J ty))
newGlobalRef_ j =
    newGlobalRef j Linear.>>= \(j1, g) -> g Linear.<$ deleteLocalRef j1

deleteGlobalRef :: JNI.J ty -> Linear.IO ()
deleteGlobalRef o = Linear.fromSystemIO $ JNI.deleteGlobalRef o

-- | Like 'newGlobalRef' but it doesn't attach a finalizer to destroy
-- the reference when it is not longer reachable. Use
-- 'deleteGlobalRefNonFinalized' to destroy this reference.
newGlobalRefNonFinalized
  :: J ty ->. Linear.IO (J ty, Unrestricted (JNI.J ty))
newGlobalRefNonFinalized = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $
      (,) o . Unrestricted <$> JNI.newGlobalRefNonFinalized (unJ o)

-- | Like 'deleteGlobalRef' but it can be used only on references created with
-- 'newGlobalRefNonFinalized'.
deleteGlobalRefNonFinalized :: J ty -> Linear.IO ()
deleteGlobalRefNonFinalized o = Linear.fromSystemIO $ JNI.deleteGlobalRef o


-- NB: Cannot add a finalizer to local references because it may
-- run in a thread where the reference is not valid.
newLocalRef :: J ty ->. Linear.IO (J ty, J ty)
newLocalRef = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ (,) o . J <$> JNI.newLocalRef (unJ o)

deleteLocalRef :: J ty ->. Linear.IO ()
deleteLocalRef = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ JNI.deleteLocalRef (unJ o)

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
call/**/name/**/Method :: J a ->. JMethodID ->. [JValue] ->. Linear.IO (J a, Unrestricted hs_rettype); \
call/**/name/**/Method = Unsafe.toLinear3 $ \obj method args -> \
    Linear.fromSystemIO $ \
      (,) obj . Unrestricted <$> JNI.call/**/name/**/Method (unJ obj) method (toJNIJValues args) \
       Prelude.<* deleteLinearJObjects args

deleteLinearJObjects :: [JValue] -> IO ()
deleteLinearJObjects = mapM_ Prelude.$ \case
    JObject j ->  (JNI.deleteLocalRef j)
    _ -> return ()

callVoidMethod :: J a ->. JMethodID ->. [JValue] ->. Linear.IO (J a)
callVoidMethod = Unsafe.toLinear3 $ \obj method args ->
    Linear.fromSystemIO $
      obj <$ JNI.callVoidMethod (unJ obj) method (toJNIJValues args)
        Prelude.<* deleteLinearJObjects args

callObjectMethod
  :: J a
  ->. JMethodID
  ->. [JValue]
  ->. Linear.IO (J a, JObject)
callObjectMethod = Unsafe.toLinear3 $ \obj method args ->
    Linear.fromSystemIO $
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
callStatic/**/name/**/Method :: JNI.JClass -> JMethodID ->. [JValue] ->. Linear.IO (Unrestricted hs_rettype); \
callStatic/**/name/**/Method cls = Unsafe.toLinear2 $ \method args -> \
    Linear.fromSystemIOU \
      (JNI.callStatic/**/name/**/Method cls method (toJNIJValues args) \
        Prelude.<* deleteLinearJObjects args)

callStaticObjectMethod
  :: JNI.JClass
  -> JMethodID
  ->. [JValue]
  ->. Linear.IO JObject
callStaticObjectMethod jclass = Unsafe.toLinear2 $ \method args ->
    Linear.fromSystemIO $ do
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

newObjectArray :: Int32 -> JNI.JClass -> Linear.IO JObjectArray
newObjectArray sz cls = Linear.fromSystemIO $ J <$> JNI.newObjectArray sz cls

#define NEW_ARRAY(name) \
new/**/name/**/Array :: Int32 -> Linear.IO J/**/name/**/Array; \
new/**/name/**/Array sz = Linear.fromSystemIO $ J <$> JNI.new/**/name/**/Array sz

NEW_ARRAY(Boolean)
NEW_ARRAY(Byte)
NEW_ARRAY(Char)
NEW_ARRAY(Short)
NEW_ARRAY(Int)
NEW_ARRAY(Long)
NEW_ARRAY(Float)
NEW_ARRAY(Double)

newString :: Ptr Word16 -> Int32 -> Linear.IO JString
newString ptr len = Linear.fromSystemIO $ J <$> JNI.newString ptr len

getArrayLength :: JArray a ->. Linear.IO (JArray a, Unrestricted Int32)
getArrayLength = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ (,) o . Unrestricted <$> JNI.getArrayLength (unJ o)

getStringLength :: JString ->. Linear.IO (JString, Int32)
getStringLength = Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ (,) o <$> JNI.getStringLength (unJ o)

#define GET_ARRAY_ELEMENTS(name, hs_rettype) \
get/**/name/**/ArrayElements :: J/**/name/**/Array ->. Linear.IO (J/**/name/**/Array, Unrestricted (Ptr hs_rettype)); \
get/**/name/**/ArrayElements = Unsafe.toLinear $ \a -> \
      Linear.fromSystemIO $ \
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

getStringChars :: JString ->. Linear.IO (JString, Ptr Word16)
getStringChars = Unsafe.toLinear $ \jstr ->
    Linear.fromSystemIO $ (,) jstr <$> JNI.getStringChars (unJ jstr)

#define SET_ARRAY_REGION(name, hs_argtype) \
set/**/name/**/ArrayRegion :: J/**/name/**/Array ->. Int32 -> Int32 -> Ptr hs_argtype -> Linear.IO J/**/name/**/Array; \
set/**/name/**/ArrayRegion = Unsafe.toLinear $ \array start len buf -> \
    Linear.fromSystemIO $ array <$ JNI.set/**/name/**/ArrayRegion (unJ array) start len buf

SET_ARRAY_REGION(Boolean, Word8)
SET_ARRAY_REGION(Byte, CChar)
SET_ARRAY_REGION(Char, Word16)
SET_ARRAY_REGION(Short, Int16)
SET_ARRAY_REGION(Int, Int32)
SET_ARRAY_REGION(Long, Int64)
SET_ARRAY_REGION(Float, Float)
SET_ARRAY_REGION(Double, Double)

#define RELEASE_ARRAY_ELEMENTS(name, hs_argtype) \
release/**/name/**/ArrayElements :: J/**/name/**/Array ->. Ptr hs_argtype ->. Linear.IO J/**/name/**/Array; \
release/**/name/**/ArrayElements = Unsafe.toLinear2 $ \array xs -> \
    Linear.fromSystemIO $ array <$ JNI.release/**/name/**/ArrayElements (unJ array) xs

RELEASE_ARRAY_ELEMENTS(Boolean, Word8)
RELEASE_ARRAY_ELEMENTS(Byte, CChar)
RELEASE_ARRAY_ELEMENTS(Char, Word16)
RELEASE_ARRAY_ELEMENTS(Short, Int16)
RELEASE_ARRAY_ELEMENTS(Int, Int32)
RELEASE_ARRAY_ELEMENTS(Long, Int64)
RELEASE_ARRAY_ELEMENTS(Float, Float)
RELEASE_ARRAY_ELEMENTS(Double, Double)

releaseStringChars :: JString ->. Ptr Word16 -> Linear.IO JString
releaseStringChars = Unsafe.toLinear $ \jstr chars ->
    Linear.fromSystemIO $ jstr <$ JNI.releaseStringChars (unJ jstr) chars

getObjectArrayElement
  :: IsReferenceType a
  => JArray a
  ->. Int32
  ->. Linear.IO (JArray a, J a)
getObjectArrayElement = Unsafe.toLinear2 $ \a i ->
    Linear.fromSystemIO $ (,) a . J <$> JNI.getObjectArrayElement (unJ a) i

setObjectArrayElement
  :: IsReferenceType a
  => JArray a
  ->. Int32
  -> J a
  ->. Linear.IO (JArray a, J a)
setObjectArrayElement = Unsafe.toLinear $ \a i -> Unsafe.toLinear $ \o ->
    Linear.fromSystemIO $ (a, o) <$ JNI.setObjectArrayElement (unJ a) i (unJ o)

setObjectArrayElement_
  :: IsReferenceType a
  => JArray a
  ->. Int32
  -> J a
  ->. Linear.IO (JArray a)
setObjectArrayElement_ _a i _j =
    setObjectArrayElement _a i _j Linear.>>= \(_a, _j) ->
      _a Linear.<$ deleteLocalRef _j
