{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Foreign.JNI.Types
  ( JType(..)
  , IsPrimitiveType
  , IsReferenceType
  , Sing(..)
  , SSing(..)
  , type (<>)
    -- * JNI types
  , J(..)
  , jnull
  , upcast
  , arrayUpcast
  , unsafeCast
  , generic
  , unsafeUngeneric
  , jtypeOf
  , ReferenceTypeName
  , singToIsReferenceType
  , referenceTypeName
  , Signature
  , signature
  , MethodSignature
  , methodSignature
  , JVM(..)
  , JNIEnv(..)
  , JMethodID(..)
  , JFieldID(..)
  , JValue(..)
  , withJValues
    -- * Conversions
  , objectFromPtr
  , unsafeObjectToPtr
    -- * JNI defined object types
  , JObject
  , JClass
  , JString
  , JArray
  , JObjectArray
  , JBooleanArray
  , JByteArray
  , JCharArray
  , JShortArray
  , JIntArray
  , JLongArray
  , JFloatArray
  , JDoubleArray
  , JThrowable
  , JByteBuffer
  -- * inline-c contexts
  , jniCtx
  ) where

import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as Prim
import Data.ByteString.Builder (Builder)
import Data.Char (chr, ord)
import Data.Constraint (Dict(..))
import Data.Int
import qualified Data.Map as Map
import Data.Singletons
import Data.Word
import Foreign.C (CChar)
import Foreign.ForeignPtr
  ( ForeignPtr
  , castForeignPtr
  , newForeignPtr_
  , withForeignPtr
  )
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.JNI.Internal
import Foreign.JNI.NativeMethod
import qualified Foreign.JNI.String as JNI
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr
import Foreign.Storable (Storable(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.C.Types (TypeSpecifier(TypeName))
import Language.C.Inline.Context (Context(..), fptrCtx)
import System.IO.Unsafe (unsafePerformIO)

-- | A JVM instance.
newtype JVM = JVM_ (Ptr JVM)
  deriving (Eq, Show, Storable, NFData)

-- | The thread-local JNI context. Do not share this object between threads.
newtype JNIEnv = JNIEnv_ (Ptr JNIEnv)
  deriving (Eq, Show, Storable, NFData)

-- | A thread-local reference to a field of an object.
newtype JFieldID = JFieldID_ (Ptr JFieldID)
  deriving (Eq, Show, Storable, NFData)

-- | A thread-local reference to a method of an object.
newtype JMethodID = JMethodID_ (Ptr JMethodID)
  deriving (Eq, Show, Storable, NFData)

-- | Not part of the JNI. The kind of 'J' type indices. Useful to reflect the
-- object's class at the type-level.
data JType
  = Class Symbol                               -- ^ Class name
  | Iface Symbol                               -- ^ Interface name
  | Prim Symbol                                -- ^ Primitive type
  | Array JType                                -- ^ Array type
  | Generic JType [JType]                      -- ^ Parameterized (generic) type
  | Void                                       -- ^ Void special type

-- | The class of Java types that are "unboxed".
class SingI ty => IsPrimitiveType (ty :: JType)
instance KnownSymbol sym => IsPrimitiveType ('Prim sym)

class IsReferenceType (ty :: JType)
instance IsReferenceType ('Class sym)
instance IsReferenceType ('Iface sym)
instance IsReferenceType ('Array ty)
instance IsReferenceType ty => IsReferenceType ('Generic ty tys)

-- | Produces evidence for IsReferenceType from a `Sing ty`.
singToIsReferenceType :: Sing (ty :: JType) -> Maybe (Dict (IsReferenceType ty))
singToIsReferenceType tysing = case tysing of
    SClass _ -> Just Dict
    SPrim _ -> Nothing
    SIface _ -> Just Dict
    SArray _ -> Just Dict
    SGeneric tysing' _ -> (\Dict -> Dict) <$> singToIsReferenceType tysing'
    SVoid -> Nothing

type instance Sing = SSing
data SSing (a :: JType) where
  -- Using String instead of JNI.String for the singleton data constructors
  -- is an optimization. Otherwise, the comparisons in Language.Java.call
  -- and callStatic would involve allocations and cannot be cached.
  --
  -- See commit 3da51a4 and https://github.com/tweag/inline-java/issues/11
  SClass :: String -> SSing ('Class sym)
  SIface :: String -> SSing ('Iface sym)
  SPrim :: String -> SSing ('Prim sym)
  SArray :: SSing ty -> SSing ('Array ty)
  SGeneric :: SSing ty -> Sing tys -> SSing ('Generic ty tys)
  SVoid :: SSing 'Void

realShowsPrec :: Show a => Int -> a -> ShowS
realShowsPrec = showsPrec

instance Show (SSing a) where
  showsPrec d (SClass s) = showParen (d > 10) $
      showString "SClass " . realShowsPrec 11 s
  showsPrec d (SIface s) = showParen (d > 10) $
      showString "SIface " . realShowsPrec 11 s
  showsPrec d (SPrim s) = showParen (d > 10) $
      showString "SPrim " . realShowsPrec 11 s
  showsPrec d (SArray s) = showParen (d > 10) $
      showString "SArray " . showsPrec 11 s
  showsPrec d (SGeneric s sargs) = showParen (d > 10) $
      showString "SGeneric " . showsPrec 11 s . showsPrec 11 sargs
  showsPrec _ SVoid = showString "SVoid"

-- XXX SingI constraint temporary hack because GHC 7.10 has trouble inferring
-- this constraint in 'signature'.
instance (KnownSymbol sym, SingI sym) => SingI ('Class (sym :: Symbol)) where
  sing = SClass $ symbolVal (undefined :: proxy sym)
instance (KnownSymbol sym, SingI sym) => SingI ('Iface (sym :: Symbol)) where
  sing = SIface $ symbolVal (undefined :: proxy sym)
instance (KnownSymbol sym, SingI sym) => SingI ('Prim (sym :: Symbol)) where
  sing = SPrim $ symbolVal (undefined :: proxy sym)
instance SingI ty => SingI ('Array ty) where
  sing = SArray sing
instance (SingI ty, SingI tys) => SingI ('Generic ty tys) where
  sing = SGeneric sing sing
instance SingI 'Void where
  sing = SVoid

-- | Shorthand for parametized Java types.
type a <> g = 'Generic a g

-- | Type indexed Java Objects.
newtype J (a :: JType) = J (ForeignPtr (J a))
  deriving (Eq, Show)

type role J representational

-- | The null reference.
jnull :: J a
jnull = J $ unsafePerformIO $ newForeignPtr_ nullPtr

-- | Any object can be cast to @Object@.
upcast :: J a -> JObject
upcast = unsafeCast

-- | Any array of a reference type can be casted to an array of @Object@s.
arrayUpcast :: IsReferenceType ty => J ('Array ty) -> JObjectArray
arrayUpcast = unsafeCast

-- | Unsafe type cast. Should only be used to downcast.
unsafeCast :: J a -> J b
unsafeCast (J x) = J (castForeignPtr x)

-- | Parameterize the type of an object, making its type a /generic type/.
generic :: J a -> J (a <> g)
generic = unsafeCast

-- | Get the base type of a generic type.
unsafeUngeneric :: J (a <> g) -> J a
unsafeUngeneric = unsafeCast

-- | A union type for uniformly passing arguments to methods.
data JValue
  = JBoolean Word8
  | JByte CChar
  | JChar Word16
  | JShort Int16
  | JInt Int32
  | JLong Int64
  | JFloat Float
  | JDouble Double
  | forall a. SingI a => JObject {-# UNPACK #-} !(J a)

instance Show JValue where
  show (JBoolean x) = "JBoolean " ++ show x
  show (JByte x) = "JByte " ++ show x
  show (JChar x) = "JChar " ++ show x
  show (JShort x) = "JShort " ++ show x
  show (JInt x) = "JInt " ++ show x
  show (JLong x) = "JLong " ++ show x
  show (JFloat x) = "JFloat " ++ show x
  show (JDouble x) = "JDouble " ++ show x
  show (JObject x) = "JObject " ++ show x

instance Eq JValue where
  (JBoolean x) == (JBoolean y) = x == y
  (JByte x) == (JByte y) = x == y
  (JChar x) == (JChar y) = x == y
  (JShort x) == (JShort y) = x == y
  (JInt x) == (JInt y) = x == y
  (JLong x) == (JLong y) = x == y
  (JFloat x) == (JFloat y) = x == y
  (JDouble x) == (JDouble y) = x == y
  (JObject (J x)) == (JObject (J y)) = castForeignPtr x == castForeignPtr y
  _ == _ = False

sizeOfJValue, alignmentJValue :: Int
sizeOfJValue      = 8
alignmentJValue   = 8

-- | @withJValue jvalues f@ provides a pointer to an array containing the given
-- @jvalues@.
--
-- The array is valid only while evaluating @f@.
withJValues :: [JValue] -> (Ptr JValue -> IO a) -> IO a
withJValues args f =
    allocaBytesAligned (sizeOfJValue * length args) alignmentJValue $ \p ->
      foldr (.) id (zipWith (withJValueOff p) [0..] args) (f p)

-- @withJValueOff p n jvalue io@ writes the given @jvalue@ to @p `plusPtr` n@
-- and runs @io@.
--
-- The jvalue is guaranteed to stay valid while @io@ evaluates.
withJValueOff :: Ptr JValue -> Int -> JValue -> IO a -> IO a
withJValueOff p n jvalue io = case jvalue of
    JBoolean x -> pokeByteOff (castPtr p) offset x >> io
    JByte    x -> pokeByteOff (castPtr p) offset x >> io
    JChar    x -> pokeByteOff (castPtr p) offset x >> io
    JShort   x -> pokeByteOff (castPtr p) offset x >> io
    JInt     x -> pokeByteOff (castPtr p) offset x >> io
    JLong    x -> pokeByteOff (castPtr p) offset x >> io
    JFloat   x -> pokeByteOff (castPtr p) offset x >> io
    JDouble  x -> pokeByteOff (castPtr p) offset x >> io

    JObject (J x) -> withForeignPtr x $ \xp ->
      pokeByteOff (castPtr p) offset xp >> io
  where
    offset = n * sizeOfJValue

-- | Get the Java type of a value.
jtypeOf :: JValue -> SomeSing JType
jtypeOf (JBoolean _) = SomeSing (sing :: Sing ('Prim "boolean"))
jtypeOf (JByte _) = SomeSing (sing :: Sing ('Prim "byte"))
jtypeOf (JChar _) = SomeSing (sing :: Sing ('Prim "char"))
jtypeOf (JShort _) = SomeSing (sing :: Sing ('Prim "short"))
jtypeOf (JInt _) = SomeSing (sing :: Sing ('Prim "int"))
jtypeOf (JLong _) = SomeSing (sing :: Sing ('Prim "long"))
jtypeOf (JFloat _) = SomeSing (sing :: Sing ('Prim "float"))
jtypeOf (JDouble _) = SomeSing (sing :: Sing ('Prim "double"))
jtypeOf (JObject (_ :: J ty)) = SomeSing (sing :: Sing ty)

-- | Create a null-terminated string.
build :: Builder -> JNI.String
build =
  JNI.unsafeFromByteString .
  BSL.toStrict .
  Builder.toLazyByteString .
  (<> Builder.char7 '\NUL')

-- | The name of a type, suitable for passing to 'Foreign.JNI.findClass'.
referenceTypeName :: IsReferenceType ty => Sing (ty :: JType) -> ReferenceTypeName
referenceTypeName (SClass sym) = ReferenceTypeName $ build $ classSymbolBuilder sym
referenceTypeName (SIface sym) = ReferenceTypeName $ build $ classSymbolBuilder sym
referenceTypeName ty@(SArray _) = ReferenceTypeName $ build $ signatureBuilder ty
referenceTypeName (SGeneric ty@(SClass _) _) = referenceTypeName ty
referenceTypeName (SGeneric ty@(SIface _) _) = referenceTypeName ty
referenceTypeName _ = error "referenceTypeName: Impossible."

classSymbolBuilder :: String -> Builder
classSymbolBuilder sym =
    Prim.primMapByteStringFixed (subst Prim.>$< Prim.word8)
      (JNI.toByteString $ JNI.fromChars sym)
  where
    subst (chr . fromIntegral -> '.') = fromIntegral (ord '/')
    subst x = x

signatureBuilder :: Sing (ty :: JType) -> Builder
signatureBuilder (SClass sym) = Builder.char7 'L' <> classSymbolBuilder sym <> Builder.char7 ';'
signatureBuilder (SIface sym) = Builder.char7 'L' <> classSymbolBuilder sym <> Builder.char7 ';'
signatureBuilder (SPrim "boolean") = Builder.char7 'Z'
signatureBuilder (SPrim "byte") = Builder.char7 'B'
signatureBuilder (SPrim "char") = Builder.char7 'C'
signatureBuilder (SPrim "short") = Builder.char7 'S'
signatureBuilder (SPrim "int") = Builder.char7 'I'
signatureBuilder (SPrim "long") = Builder.char7 'J'
signatureBuilder (SPrim "float") = Builder.char7 'F'
signatureBuilder (SPrim "double") = Builder.char7 'D'
signatureBuilder (SPrim sym) = error $ "Unknown primitive: " ++ sym
signatureBuilder (SArray ty) = Builder.char7 '[' <> signatureBuilder ty
signatureBuilder (SGeneric ty _) = signatureBuilder ty
signatureBuilder SVoid = Builder.char7 'V'

-- | Construct a JNI type signature from a Java type.
signature :: Sing (ty :: JType) -> Signature
signature = Signature . build . signatureBuilder

-- | Construct a method's JNI type signature, given the type of the arguments
-- and the return type.
methodSignature
  :: [SomeSing JType]
  -> Sing (ty :: JType)
  -> MethodSignature
methodSignature args ret =
    MethodSignature $
    build $
    Builder.char7 '(' <>
    mconcat (map (\(SomeSing s) -> signatureBuilder s) args) <>
    Builder.char7 ')' <>
    signatureBuilder ret

-- | Turn the raw pointer into an object.
objectFromPtr :: Ptr (J a) -> IO (J a)
objectFromPtr ptr = J <$> newForeignPtr_ ptr

-- | Get a raw pointer to an object. This is unsafe because if the argument is
-- the last usage occurrence of the given foreign pointer, then its finalizer(s)
-- will be run, which potentially invalidates the plain pointer just obtained.
unsafeObjectToPtr :: J a -> Ptr (J a)
unsafeObjectToPtr (J fptr) = unsafeForeignPtrToPtr fptr

type JObject = J ('Class "java.lang.Object")
type JClass = J ('Class "java.lang.Class")
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
type JByteBuffer = J ('Class "java.nio.ByteBuffer")

jniCtx :: Context
jniCtx = mempty { ctxTypesTable = Map.fromList tytab } <> fptrCtx
  where
    tytab =
      [ -- Primitive types
        (TypeName "jboolean", [t| Word8 |])
      , (TypeName "jbyte", [t| CChar |])
      , (TypeName "jchar", [t| Word16 |])
      , (TypeName "jshort", [t| Int16 |])
      , (TypeName "jint", [t| Int32 |])
      , (TypeName "jlong", [t| Int64 |])
      , (TypeName "jfloat", [t| Float |])
      , (TypeName "jdouble", [t| Double |])
      -- Reference types
      , (TypeName "jobject", [t| Ptr JObject |])
      , (TypeName "jclass", [t| Ptr JClass |])
      , (TypeName "jstring", [t| Ptr JString |])
      , (TypeName "jarray", [t| Ptr JObject |])
      , (TypeName "jobjectArray", [t| Ptr JObjectArray |])
      , (TypeName "jbooleanArray", [t| Ptr JBooleanArray |])
      , (TypeName "jbyteArray", [t| Ptr JByteArray |])
      , (TypeName "jcharArray", [t| Ptr JCharArray |])
      , (TypeName "jshortArray", [t| Ptr JShortArray |])
      , (TypeName "jintArray", [t| Ptr JIntArray |])
      , (TypeName "jlongArray", [t| Ptr JLongArray |])
      , (TypeName "jfloatArray", [t| Ptr JFloatArray |])
      , (TypeName "jdoubleArray", [t| Ptr JDoubleArray |])
      , (TypeName "jthrowable", [t| Ptr JThrowable |])
      -- Internal types
      , (TypeName "JavaVM", [t| JVM |])
      , (TypeName "JNIEnv", [t| JNIEnv |])
      , (TypeName "JNINativeMethod", [t| JNINativeMethod |])
      , (TypeName "jfieldID", [t| JFieldID |])
      , (TypeName "jmethodID", [t| JMethodID |])
      , (TypeName "jsize", [t| Int32 |])
      , (TypeName "jvalue", [t| JValue |])
      ]
