-- | High-level helper functions for interacting with Java objects, mapping them
-- to Haskell values and vice versa. The 'Reify' and 'Reflect' classes together
-- are to Java what "Foreign.Storable" is to C: they provide a means to
-- marshall/unmarshall Java objects from/to Haskell data types.
--
-- A typical pattern for wrapping Java API's using this module is:
--
-- @
-- {&#45;\# LANGUAGE DataKinds \#&#45;}
-- module Object where
--
-- import Language.Java as J
--
-- newtype Object = Object ('J' (''Class' "java.lang.Object"))
-- instance 'Coercible' Object
--
-- clone :: Object -> IO Object
-- clone obj = J.'call' obj "clone" []
--
-- equals :: Object -> Object -> IO Bool
-- equals obj1 obj2 = J.'call' obj1 "equals" ['jvalue' obj2]
--
-- ...
-- @
--
-- To call Java methods using quasiquoted Java syntax instead, see
-- "Language.Java.Inline".
--
-- __NOTE 1:__ To use any function in this module, you'll need an initialized JVM in the
-- current process, using 'withJVM' or otherwise.
--
-- __NOTE 2:__ Functions in this module memoize (cache) any implicitly performed
-- class and method lookups, for performance. This memoization is safe only when
-- no new named classes are defined at runtime.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java
  ( module Foreign.JNI.Types
  , withJVM
  , classOf
  , new
  , newArray
  , call
  , callStatic
  , getStaticField
  , jvalue
  , CoercionFailure(..)
  , Coercible(..)
  , Reify(..)
  , Reflect(..)
  , Type(..)
  , Uncurry
  , Interp
  , sing
  ) where

import Control.Distributed.Closure
import Control.Distributed.Closure.TH
import Control.Exception (Exception, throw, finally)
import Control.Monad
import Data.Char (chr, ord)
import qualified Data.Choice as Choice
import qualified Data.Coerce as Coerce
import Data.Constraint (Dict(..))
import Data.Int
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, TypeRep, typeOf)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Singletons (SingI(..))
import qualified Data.Text.Foreign as Text
import Data.Text (Text)
#if ! (__GLASGOW_HASKELL__ == 800 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1)
import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as MVector
import Data.Vector.Storable.Mutable (IOVector)
import Foreign (Ptr, Storable, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
#endif
import Foreign.C (CChar)
import Foreign.JNI hiding (throw)
import Foreign.JNI.Types
import qualified Foreign.JNI.String as JNI
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- Note [Class lookup memoization]
--
-- By using unsafeDupablePerformIO, we mark the lookup actions as pure. When the
-- body of the function is inlined within the calling context, the lookups
-- typically become closed expressions, therefore are CAF's that can be floated
-- to top-level by the GHC optimizer.

-- | Tag data types that can be coerced in O(1) time without copy to a Java
-- object or primitive type (i.e. have the same representation) by declaring an
-- instance of this type class for that data type.
class SingI ty => Coercible a (ty :: JType) | a -> ty where
  coerce :: a -> JValue
  unsafeUncoerce :: JValue -> a

  default coerce
    :: Coerce.Coercible a (J ty)
    => a
    -> JValue
  coerce x = JObject (Coerce.coerce x :: J ty)

  default unsafeUncoerce
    :: Coerce.Coercible (J ty) a
    => JValue
    -> a
  unsafeUncoerce (JObject obj) = Coerce.coerce (unsafeCast obj :: J ty)
  unsafeUncoerce _ =
      error "Cannot unsafeUncoerce: object expected but value of primitive type found."

-- | The identity instance.
instance SingI ty => Coercible (J ty) ty

-- | A JNI call may cause a (Java) exception to be raised. This module raises it
-- as a Haskell exception wrapping the Java exception.
data CoercionFailure = CoercionFailure
  { coercionActual :: JValue
  , coercionExpected :: TypeRep
  }

instance Exception CoercionFailure

instance Show CoercionFailure where
  show (CoercionFailure actual expected) =
    "Can't coerce " ++ show actual ++ " to " ++ show expected ++ "."

withTypeRep :: Typeable a => (TypeRep -> a) -> a
withTypeRep f = let x = f (typeOf x) in x

instance Coercible Bool ('Prim "boolean") where
  coerce x = JBoolean (fromIntegral (fromEnum x))
  unsafeUncoerce (JBoolean x) = toEnum (fromIntegral x)
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible CChar ('Prim "byte") where
  coerce = JByte
  unsafeUncoerce (JByte x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Char ('Prim "char") where
  coerce x = JChar (fromIntegral (ord x))
  unsafeUncoerce (JChar x) = chr (fromIntegral x)
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Word16 ('Prim "char") where
  coerce = JChar
  unsafeUncoerce (JChar x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Int16 ('Prim "short") where
  coerce = JShort
  unsafeUncoerce (JShort x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Int32 ('Prim "int") where
  coerce = JInt
  unsafeUncoerce (JInt x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Int64 ('Prim "long") where
  coerce = JLong
  unsafeUncoerce (JLong x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Float ('Prim "float") where
  coerce = JFloat
  unsafeUncoerce (JFloat x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Double ('Prim "double") where
  coerce = JDouble
  unsafeUncoerce (JDouble x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible () 'Void where
  coerce = error "Void value undefined."
  unsafeUncoerce _ = ()
instance Coercible (Choice.Choice a) ('Prim "boolean") where
  coerce = coerce . Choice.toBool
  unsafeUncoerce = Choice.fromBool . unsafeUncoerce

-- | Get the Java class of an object or anything 'Coercible' to one.
classOf :: forall a sym. (Coercible a ('Class sym), KnownSymbol sym) => a -> JNI.String
classOf _ = JNI.fromChars (symbolVal (Proxy :: Proxy sym))

-- | Creates a new instance of the class whose name is resolved from the return
-- type. For instance,
--
-- @
-- do x :: 'J' (''Class' "java.lang.Integer") <- new ['coerce' 42]
--    return x
-- @
new
  :: forall a sym.
     ( Coerce.Coercible a (J ('Class sym))
     , Coercible a ('Class sym)
     )
  => [JValue]
  -> IO a
{-# INLINE new #-}
new args = do
    let argsings = map jtypeOf args
        voidsing = sing :: Sing 'Void
        klass = unsafeDupablePerformIO $ do
          lk <- findClass (referenceTypeName (sing :: Sing ('Class sym)))
          gk <- newGlobalRef lk
          deleteLocalRef lk
          return gk
    Coerce.coerce <$> newObject klass (methodSignature argsings voidsing) args

-- | Creates a new Java array of the given size. The type of the elements
-- of the resulting array is determined by the return type a call to
-- 'newArray' has, at the call site, and must not be left ambiguous.
--
-- To create a Java array of 50 booleans:
--
-- @
-- do arr :: 'J' (''Array' (''Prim' "boolean")) <- 'newArray' 50
--    return arr
-- @
newArray
  :: forall ty.
     SingI ty
  => Int32
  -> IO (J ('Array ty))
{-# INLINE newArray #-}
newArray sz = do
    let tysing = sing :: Sing ty
    case tysing of
      SPrim "boolean" -> unsafeCast <$> newBooleanArray sz
      SPrim "byte" -> unsafeCast <$> newByteArray sz
      SPrim "char" -> unsafeCast <$> newCharArray sz
      SPrim "short" -> unsafeCast <$> newShortArray sz
      SPrim "int" -> unsafeCast <$> newIntArray sz
      SPrim "long" -> unsafeCast <$> newLongArray sz
      SPrim "float" -> unsafeCast <$> newFloatArray sz
      SPrim "double" -> unsafeCast <$> newDoubleArray sz
      SVoid -> fail "newArray of void"
      _ -> case singToIsReferenceType tysing of
        Nothing -> fail $ "newArray of " ++ show tysing
        Just Dict -> do
          let klass = unsafeDupablePerformIO $ do
                lk <- findClass (referenceTypeName tysing)
                gk <- newGlobalRef lk
                deleteLocalRef lk
                return gk
          unsafeCast <$> newObjectArray sz klass

-- | The Swiss Army knife for calling Java methods. Give it an object or
-- any data type coercible to one, the name of a method, and a list of
-- arguments. Based on the type indexes of each argument, and based on the
-- return type, 'call' will invoke the named method using of the @call*Method@
-- family of functions in the JNI API.
--
-- When the method name is overloaded, use 'upcast' or 'unsafeCast'
-- appropriately on the class instance and/or on the arguments to invoke the
-- right method.
call
  :: forall a b ty1 ty2. (IsReferenceType ty1, Coercible a ty1, Coercible b ty2, Coerce.Coercible a (J ty1))
  => a -- ^ Any object or value 'Coercible' to one
  -> JNI.String -- ^ Method name
  -> [JValue] -- ^ Arguments
  -> IO b
{-# INLINE call #-}
call obj mname args = do
    let argsings = map jtypeOf args
        retsing = sing :: Sing ty2
        klass = unsafeDupablePerformIO $ do
                  lk <- findClass (referenceTypeName (sing :: Sing ty1))
                  gk <- newGlobalRef lk
                  deleteLocalRef lk
                  return gk
        method = unsafeDupablePerformIO $ getMethodID klass mname (methodSignature argsings retsing)
    case retsing of
      SPrim "boolean" -> unsafeUncoerce . coerce <$> callBooleanMethod obj method args
      SPrim "byte" -> unsafeUncoerce . coerce <$> callByteMethod obj method args
      SPrim "char" -> unsafeUncoerce . coerce <$> callCharMethod obj method args
      SPrim "short" -> unsafeUncoerce . coerce <$> callShortMethod obj method args
      SPrim "int" -> unsafeUncoerce . coerce <$> callIntMethod obj method args
      SPrim "long" -> unsafeUncoerce . coerce <$> callLongMethod obj method args
      SPrim "float" -> unsafeUncoerce . coerce <$> callFloatMethod obj method args
      SPrim "double" -> unsafeUncoerce . coerce <$> callDoubleMethod obj method args
      SVoid -> do
        callVoidMethod obj method args
        -- Anything uncoerces to the void type.
        return (unsafeUncoerce undefined)
      _ -> unsafeUncoerce . coerce <$> callObjectMethod obj method args

-- | Same as 'call', but for static methods.
callStatic
  :: forall a ty. Coercible a ty
  => JNI.String -- ^ Class name
  -> JNI.String -- ^ Method name
  -> [JValue] -- ^ Arguments
  -> IO a
{-# INLINE callStatic #-}
callStatic cname mname args = do
    let argsings = map jtypeOf args
        retsing = sing :: Sing ty
        klass = unsafeDupablePerformIO $ do
                  lk <- findClass
                          (referenceTypeName (SClass (JNI.toChars cname)))
                  gk <- newGlobalRef lk
                  deleteLocalRef lk
                  return gk
        method = unsafeDupablePerformIO $ getStaticMethodID klass mname (methodSignature argsings retsing)
    case retsing of
      SPrim "boolean" -> unsafeUncoerce . coerce <$> callStaticBooleanMethod klass method args
      SPrim "byte" -> unsafeUncoerce . coerce <$> callStaticByteMethod klass method args
      SPrim "char" -> unsafeUncoerce . coerce <$> callStaticCharMethod klass method args
      SPrim "short" -> unsafeUncoerce . coerce <$> callStaticShortMethod klass method args
      SPrim "int" -> unsafeUncoerce . coerce <$> callStaticIntMethod klass method args
      SPrim "long" -> unsafeUncoerce . coerce <$> callStaticLongMethod klass method args
      SPrim "float" -> unsafeUncoerce . coerce <$> callStaticFloatMethod klass method args
      SPrim "double" -> unsafeUncoerce . coerce <$> callStaticDoubleMethod klass method args
      SVoid -> do
        callStaticVoidMethod klass method args
        -- Anything uncoerces to the void type.
        return (unsafeUncoerce undefined)
      _ -> unsafeUncoerce . coerce <$> callStaticObjectMethod klass method args

-- | Get a static field.
getStaticField
  :: forall a ty. Coercible a ty
  => JNI.String -- ^ Class name
  -> JNI.String -- ^ Static field name
  -> IO a
{-# INLINE getStaticField #-}
getStaticField cname fname = do
  let retsing = sing :: Sing ty
      klass = unsafeDupablePerformIO $
                findClass (referenceTypeName (SClass (JNI.toChars cname)))
                  >>= newGlobalRef
      field = unsafeDupablePerformIO $ getStaticFieldID klass fname (signature retsing)
  case retsing of
    SPrim "boolean" -> unsafeUncoerce . coerce . w2b <$> getStaticBooleanField klass field
    SPrim "byte" -> unsafeUncoerce . coerce <$> getStaticByteField klass field
    SPrim "char" -> unsafeUncoerce . coerce <$> getStaticCharField klass field
    SPrim "short" -> unsafeUncoerce . coerce <$> getStaticShortField klass field
    SPrim "int" -> unsafeUncoerce . coerce <$> getStaticIntField klass field
    SPrim "long" -> unsafeUncoerce . coerce <$> getStaticLongField klass field
    SPrim "float" -> unsafeUncoerce . coerce <$> getStaticFloatField klass field
    SPrim "double" -> unsafeUncoerce . coerce <$> getStaticDoubleField klass field
    SVoid -> fail "getStaticField cannot yield an object of type void"
    _ -> unsafeUncoerce . coerce <$> getStaticObjectField klass field
  where
    w2b :: Word8 -> Bool
    w2b = toEnum . fromIntegral

-- | Inject a value (of primitive or reference type) to a 'JValue'. This
-- datatype is useful for e.g. passing arguments as a list of homogeneous type.
-- Synonym for 'coerce'.
jvalue :: Coercible a ty => a -> JValue
jvalue = coerce

-- | Classifies Java types according to whether they are base types (data) or
-- higher-order types (objects representing functions).
data Type a
  = Fun [Type a] (Type a) -- ^ Pure function
  | Act [Type a] (Type a) -- ^ IO action
  | Proc [Type a]         -- ^ Procedure (i.e void returning action)
  | Base a                -- ^ Any first-order type.

-- | Haskell functions are curried, but Java functions are not. This type family
-- maps Haskell types to an uncurried (non-inductive) type representation,
-- useful to select the right 'Reify' / 'Reflect' instance without overlap.
type family Uncurry (a :: *) :: Type * where
  Uncurry (Closure (a -> b -> c -> d -> IO ())) = 'Proc '[Uncurry a, Uncurry b, Uncurry c, Uncurry d]
  Uncurry (Closure (a -> b -> c -> IO ())) = 'Proc '[Uncurry a, Uncurry b, Uncurry c]
  Uncurry (Closure (a -> b -> IO ())) = 'Proc '[Uncurry a, Uncurry b]
  Uncurry (Closure (a -> IO ())) = 'Proc '[Uncurry a]
  Uncurry (IO ()) = 'Proc '[]
  Uncurry (Closure (a -> b -> c -> d -> IO e)) = 'Act '[Uncurry a, Uncurry b, Uncurry c, Uncurry d] (Uncurry e)
  Uncurry (Closure (a -> b -> c -> IO d)) = 'Act '[Uncurry a, Uncurry b, Uncurry c] (Uncurry d)
  Uncurry (Closure (a -> b -> IO c)) = 'Act '[Uncurry a, Uncurry b] (Uncurry c)
  Uncurry (Closure (a -> IO b)) = 'Act '[Uncurry a] (Uncurry b)
  Uncurry (Closure (IO a)) = 'Act '[] (Uncurry a)
  Uncurry (Closure (a -> b -> c -> d -> e)) = 'Fun '[Uncurry a, Uncurry b, Uncurry c, Uncurry d] (Uncurry e)
  Uncurry (Closure (a -> b -> c -> d)) = 'Fun '[Uncurry a, Uncurry b, Uncurry c] (Uncurry d)
  Uncurry (Closure (a -> b -> c)) = 'Fun '[Uncurry a, Uncurry b] (Uncurry c)
  Uncurry (Closure (a -> b)) = 'Fun '[Uncurry a] (Uncurry b)
  Uncurry a = 'Base a

-- | Map a Haskell type to the symbolic representation of a Java type.
type family Interp (a :: k) :: JType
type instance Interp ('Base a) = Interp a

-- | Extract a concrete Haskell value from the space of Java objects. That is to
-- say, unmarshall a Java object to a Haskell value. Unlike coercing, in general
-- reifying induces allocations and copies.
--
-- Instances of this class /must/ guarantee that the result is managed on the
-- Haskell heap. That is, the Haskell runtime has /global ownership/ of the
-- result.
class (Interp (Uncurry a) ~ ty, SingI ty, IsReferenceType ty)
      => Reify a ty where
  reify :: J ty -> IO a

-- | Inject a concrete Haskell value into the space of Java objects. That is to
-- say, marshall a Haskell value to a Java object. Unlike coercing, in general
-- reflection induces allocations and copies.
--
-- Instances of this class /must not/ claim global ownership.
class (Interp (Uncurry a) ~ ty, SingI ty, IsReferenceType ty)
      => Reflect a ty where
  reflect :: a -> IO (J ty)

#if ! (__GLASGOW_HASKELL__ == 800 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1)
reifyMVector
  :: Storable a
  => (JArray ty -> IO (Ptr a))
  -> (JArray ty -> Ptr a -> IO ())
  -> JArray ty
  -> IO (IOVector a)
reifyMVector mk finalize jobj0 = do
    -- jobj might be finalized before the finalizer of fptr runs.
    -- Therefore, we create a global reference without an attached
    -- finalizer.
    -- See https://ghc.haskell.org/trac/ghc/ticket/13439
    jobj <- newGlobalRefNonFinalized jobj0
    n <- getArrayLength jobj
    ptr <- mk jobj
    fptr <- newForeignPtr ptr $ finalize jobj ptr
                                  `finally` deleteGlobalRefNonFinalized jobj
    return (MVector.unsafeFromForeignPtr0 fptr (fromIntegral n))

reflectMVector
  :: Storable a
  => (Int32 -> IO (JArray ty))
  -> (JArray ty -> Int32 -> Int32 -> Ptr a -> IO ())
  -> IOVector a
  -> IO (JArray ty)
reflectMVector newfun fill mv = do
    let (fptr, n) = MVector.unsafeToForeignPtr0 mv
    jobj <- newfun (fromIntegral n)
    withForeignPtr fptr $ fill jobj 0 (fromIntegral n)
    return jobj
#endif

withStatic [d|
  type instance Interp (J ty) = ty

  -- Use this instance to claim global ownership of a Java object on the
  -- Haskell heap until the Haskell garbage collector determines that it is
  -- inaccessible. Objects that need to survive the dynamic scope delimited by
  -- the topmost Java frame on the call stack must have global ownership.
  instance (SingI ty, IsReferenceType ty) => Reify (J ty) ty where
    reify x = newGlobalRef x

  -- Use this instance to relinquish global ownership of a Java object. You
  -- /must not/ refer to the argument anywhere after a call to 'reflect'.
  instance (SingI ty, IsReferenceType ty) => Reflect (J ty) ty where
    reflect x = newLocalRef x

  type instance Interp () = 'Class "java.lang.Object"

  instance Reify () ('Class "java.lang.Object") where
    reify _ = return ()

  instance Reflect () ('Class "java.lang.Object") where
    reflect () = new []

  type instance Interp ByteString = 'Array ('Prim "byte")

  instance Reify ByteString ('Array ('Prim "byte")) where
    reify jobj = do
        n <- getArrayLength (unsafeCast jobj)
        bytes <- getByteArrayElements jobj
        -- TODO could use unsafePackCStringLen instead and avoid a copy if we knew
        -- that been handed an (immutable) copy via JNI isCopy ref.
        bs <- BS.packCStringLen (bytes, fromIntegral n)
        releaseByteArrayElements jobj bytes
        return bs

  instance Reflect ByteString ('Array ('Prim "byte")) where
    reflect bs = BS.unsafeUseAsCStringLen bs $ \(content, n) -> do
        arr <- newByteArray (fromIntegral n)
        setByteArrayRegion arr 0 (fromIntegral n) content
        return arr

  type instance Interp Bool = 'Class "java.lang.Boolean"

  instance Reify Bool ('Class "java.lang.Boolean") where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass
                         (referenceTypeName (SClass "java.lang.Boolean"))
              m <- getMethodID klass "booleanValue"
                     (methodSignature [] (SPrim "boolean"))
              deleteLocalRef klass
              return m
        callBooleanMethod jobj method []

  instance Reflect Bool ('Class "java.lang.Boolean") where
    reflect x = new [JBoolean (fromIntegral (fromEnum x))]

  type instance Interp CChar = 'Class "java.lang.Byte"

  instance Reify CChar ('Class "java.lang.Byte") where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Byte"))
              m <- getMethodID klass "byteValue"
                     (methodSignature [] (SPrim "byte"))
              deleteLocalRef klass
              return m
        callByteMethod jobj method []

  instance Reflect CChar ('Class "java.lang.Byte") where
    reflect x = Language.Java.new [JByte x]

  type instance Interp Int16 = 'Class "java.lang.Short"

  instance Reify Int16 ('Class "java.lang.Short") where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Short"))
              m <- getMethodID klass "shortValue"
                     (methodSignature [] (SPrim "short"))
              deleteLocalRef klass
              return m
        callShortMethod jobj method []

  instance Reflect Int16 ('Class "java.lang.Short") where
    reflect x = new [JShort x]

  type instance Interp Int32 = 'Class "java.lang.Integer"

  instance Reify Int32 ('Class "java.lang.Integer") where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass
                         (referenceTypeName (SClass "java.lang.Integer"))
              m <- getMethodID klass "intValue"
                     (methodSignature [] (SPrim "int"))
              deleteLocalRef klass
              return m
        callIntMethod jobj method []

  instance Reflect Int32 ('Class "java.lang.Integer") where
    reflect x = new [JInt x]

  type instance Interp Int64 = 'Class "java.lang.Long"

  instance Reify Int64 ('Class "java.lang.Long") where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Long"))
              m <- getMethodID klass "longValue"
                     (methodSignature [] (SPrim "long"))
              deleteLocalRef klass
              return m
        callLongMethod jobj method []

  instance Reflect Int64 ('Class "java.lang.Long") where
    reflect x = new [JLong x]

  type instance Interp Word16 = 'Class "java.lang.Character"

  instance Reify Word16 ('Class "java.lang.Character") where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass
                         (referenceTypeName (SClass "java.lang.Character"))
              m <- getMethodID klass "charValue"
                     (methodSignature [] (SPrim "char"))
              deleteLocalRef klass
              return m
        fromIntegral <$> callCharMethod jobj method []

  instance Reflect Word16 ('Class "java.lang.Character") where
    reflect x = new [JChar x]

  type instance Interp Double = 'Class "java.lang.Double"

  instance Reify Double ('Class "java.lang.Double") where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Double"))
              m <- getMethodID klass "doubleValue"
                     (methodSignature [] (SPrim "double"))
              deleteLocalRef klass
              return m
        callDoubleMethod jobj method []

  instance Reflect Double ('Class "java.lang.Double") where
    reflect x = new [JDouble x]

  type instance Interp Float = 'Class "java.lang.Float"

  instance Reify Float ('Class "java.lang.Float") where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- findClass (referenceTypeName (SClass "java.lang.Float"))
              m <- getMethodID klass "floatValue"
                     (methodSignature [] (SPrim "float"))
              deleteLocalRef klass
              return m
        callFloatMethod jobj method []

  instance Reflect Float ('Class "java.lang.Float") where
    reflect x = new [JFloat x]

  type instance Interp Text = 'Class "java.lang.String"

  instance Reify Text ('Class "java.lang.String") where
    reify jobj = do
        sz <- getStringLength jobj
        cs <- getStringChars jobj
        txt <- Text.fromPtr cs (fromIntegral sz)
        releaseStringChars jobj cs
        return txt

  instance Reflect Text ('Class "java.lang.String") where
    reflect x =
        Text.useAsPtr x $ \ptr len ->
          newString ptr (fromIntegral len)

-- Instances can't be compiled on GHC 8.0.1 due to
-- https://ghc.haskell.org/trac/ghc/ticket/12082.
#if ! (__GLASGOW_HASKELL__ == 800 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 1)
  type instance Interp (IOVector Int32) = 'Array ('Prim "int")

  instance Reify (IOVector Int32) ('Array ('Prim "int")) where
    reify = reifyMVector (getIntArrayElements) (releaseIntArrayElements)

  instance Reflect (IOVector Int32) ('Array ('Prim "int")) where
    reflect = reflectMVector (newIntArray) (setIntArrayRegion)

  type instance Interp (Vector Int32) = 'Array ('Prim "int")

  instance Reify (Vector Int32) ('Array ('Prim "int")) where
    reify = Vector.freeze <=< reify

  instance Reflect (Vector Int32) ('Array ('Prim "int")) where
    reflect = reflect <=< Vector.thaw
#endif

  type instance Interp [a] = 'Array (Interp (Uncurry a))

  instance Reify a ty => Reify [a] ('Array ty) where
    reify jobj = do
        n <- getArrayLength jobj
        forM [0..n-1] $ \i -> do
          jx <- getObjectArrayElement jobj i
          x  <- reify jx
          deleteLocalRef jx
          return x

  instance Reflect a ty => Reflect [a] ('Array ty) where
    reflect xs = do
      let klass = unsafeDupablePerformIO $ do
                    lk <- findClass (referenceTypeName (sing :: Sing ty))
                    gk <- newGlobalRef lk
                    deleteLocalRef lk
                    return gk
      let n = fromIntegral (length xs)
      array <- newObjectArray n klass
      forM_ (zip [0..n-1] xs) $ \(i, x) -> do
        jx <- reflect x
        setObjectArrayElement array i jx
        deleteLocalRef jx
      return (unsafeCast array)
  |]
