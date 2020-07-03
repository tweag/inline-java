-- | Types used in the safe interface of JNI

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Foreign.JNI.Types.Safe
  ( module Foreign.JNI.Types.Safe
  , JNI.IsReferenceType
  , JNI.IsPrimitiveType
  , JNI.JFieldID
  , JNI.JMethodID
  , JNI.JType(..)
  , type (JNI.<>)
  , JNI.JVM
  , JNI.MethodSignature
  , JNI.ReferenceTypeName
  , JNI.Signature
  , JNI.Sing(..)
  , JNI.methodSignature
  , JNI.referenceTypeName
  ) where

import Data.Singletons
import Foreign.JNI.Types (type (<>), JType(..), IsReferenceType)
import qualified Foreign.JNI.Types as JNI
import qualified Unsafe.Linear as Unsafe

-- | A wrapper over J references
--
-- This wrapper is used to identify local references
-- which are tracked with linear types.
newtype J (a :: JType) = J { unJ :: JNI.J a }
  deriving (Eq, Show)

type role J representational

-- | The null reference.
jnull :: J a
jnull = J JNI.jnull

-- | Any object can be cast to @Object@.
upcast :: J a #-> JObject
upcast = Unsafe.toLinear (J . JNI.upcast . unJ)

-- | Any array of a reference type can be casted to an array of @Object@s.
arrayUpcast :: IsReferenceType ty => J ('Array ty) #-> JObjectArray
arrayUpcast = Unsafe.toLinear (J . JNI.arrayUpcast . unJ)

-- | Unsafe type cast. Should only be used to downcast.
unsafeCast :: J a #-> J b
unsafeCast = Unsafe.toLinear (J . JNI.unsafeCast . unJ)

-- | Parameterize the type of an object, making its type a /generic type/.
unsafeGeneric :: J a #-> J (a <> g)
unsafeGeneric = Unsafe.toLinear (J . JNI.generic . unJ)

-- | Get the base type of a generic type.
ungeneric :: J (a <> g) #-> J a
ungeneric = Unsafe.toLinear (J . JNI.unsafeUngeneric . unJ)

-- | A union type for uniformly passing arguments to methods.
data JValue
  = JValue JNI.JValue
  | forall a. SingI a => JObject {-# UNPACK #-} !(J a)

instance Show JValue where
  show (JValue jvalue) = "JValue (" ++ show jvalue ++ ")"
  show (JObject x) = "JObject " ++ show x

instance Eq JValue where
  (JValue x) == (JValue y) = x == y
  (JObject j0) == (JObject j1) = upcast j0 == upcast j1
  _ == _ = False

-- The array is valid only while evaluating @f@.
toJNIJValues :: [JValue] -> [JNI.JValue]
toJNIJValues = map $ \case
    JValue jvalue -> jvalue
    JObject (J j) -> JNI.JObject j

-- | A type to wrap unrestricted references
--
-- Unlike with linear references, the programmer is reponsible
-- for ensuring that the unrestricted references are destroyed
-- in timely fashion.
data UnsafeUnrestrictedReference a where
  UnsafeUnrestrictedReference :: a -> UnsafeUnrestrictedReference a

type JObject = J ('Class "java.lang.Object")
type JString = J ('Class "java.lang.String")
type JThrowable = J ('Class "java.lang.Throwable")
type JArray a = J ('Array a)
type JObjectArray = JArray ('Class "java.lang.Object")
type JBooleanArray = JArray ('Prim "boolean")
type JByteArray = JArray ('Prim "byte")
type JCharArray = JArray ('Prim "char")
type JShortArray = JArray ('Prim "short")
type JIntArray = JArray ('Prim "int")
type JLongArray = JArray ('Prim "long")
type JFloatArray = JArray ('Prim "float")
type JDoubleArray = JArray ('Prim "double")
