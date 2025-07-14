-- | High-level helper functions for interacting with Java objects, mapping them
-- to Haskell values and vice versa. The 'Reify' and 'Reflect' classes together
-- are to Java what "Foreign.Storable" is to C: they provide a means to
-- marshall/unmarshall Java objects from/to Haskell data types.
--
-- A typical pattern for wrapping Java API's using this module is:
--
-- @
-- {&#45;\# LANGUAGE DataKinds \#&#45;}
-- {&#45;\# LANGUAGE DeriveAnyClass \#&#45;}
-- module Object where
--
-- import Language.Java.Unsafe as J
--
-- newtype Object = Object ('J' (''Class' "java.lang.Object"))
--   deriving (J.Coercible, J.Interpretation, J.Reify, J.Reflect)
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
-- The functions in this module are considered unsafe, as opposed to those in
-- "Language.Java.Safe", which guarantee that local references are not leaked.
-- Functions with a 'VariadicIO' constraint in their context are variadic,
-- meaning that you can apply them to any number of arguments, provided they are
-- 'Coercible'.
--
-- __NOTE 1:__ To use any function in this module, you'll need an initialized
-- JVM in the current process, using 'withJVM' or otherwise.
--
-- __NOTE 2:__ Functions in this module memoize (cache) any implicitly performed
-- class and method lookups, for performance. This memoization is safe only when
-- no new named classes are defined at runtime.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.Java.Unsafe
  ( module Foreign.JNI.Types
  -- * JVM instance management
  , withJVM
  -- * JVM calls
  , classOf
  , getClass
  , setGetClassFunction
  , new
  , newArray
  , toArray
  , call
  , callStatic
  , getStaticField
  , VariadicIO
  -- * Reference management
  , push
  , pushWithSizeHint
  , Pop(..)
  , pop
  , popWithObject
  , popWithValue
  , withLocalRef
  -- * Coercions
  , CoercionFailure(..)
  , Coercible(..)
  , jvalue
  , jobject
  -- * Conversions
  , Interpretation(..)
  , Reify(..)
  , Reflect(..)
  , Nullable(..)
  , pattern Null
  , pattern NotNull
  , W8Bool(..)
  -- * Re-exports
  , sing
  ) where

import Control.Distributed.Closure.TH
import Control.Exception (Exception, throw, finally)
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadMask, bracket, onException)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import qualified Data.Choice as Choice
import qualified Data.Coerce as Coerce
import Data.Constraint (Dict(..))
import Data.Int
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, TypeRep, typeOf)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Kind (Type)
import Data.Singletons (SingI(..), SomeSing(..))
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import qualified Data.Vector.Storable as Vector
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as MVector
import Data.Vector.Storable.Mutable (IOVector)
import Foreign (Ptr, Storable, withForeignPtr, castPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C (CChar)
import Foreign.JNI hiding (throw)
import Foreign.JNI.Types
import qualified Foreign.JNI.String as JNI
import GHC.TypeLits (KnownSymbol, TypeError, symbolVal)
import qualified GHC.TypeLits as TypeError (ErrorMessage(..))
import Language.Java.Internal
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)


data Pop a where
  PopValue :: a -> Pop a
  PopObject
    :: (ty ~ Ty a, Coercible a, Coerce.Coercible a (J ty), IsReferenceType ty)
    => a
    -> Pop a

-- | Open a new scope for allocating (JNI) local references to JVM objects.
push :: (MonadCatch m, MonadIO m) => m (Pop a) -> m a
push = pushWithSizeHint 4

-- | Like 'push', but specify explicitly a minimum size for the frame. You
-- probably don't need this.
pushWithSizeHint :: forall a m. (MonadCatch m, MonadIO m) => Int32 -> m (Pop a) -> m a
pushWithSizeHint capacity m = do
    liftIO $ pushLocalFrame capacity
    m `onException` handler >>= \case
      PopValue x -> do
        _ <- liftIO $ popLocalFrame jnull
        return x
      PopObject x -> do
        liftIO $ Coerce.coerce <$> popLocalFrame (jobject x)
  where
    handler = liftIO $ popLocalFrame jnull

-- | Equivalent to 'popWithValue ()'.
pop :: Monad m => m (Pop ())
pop = return (PopValue ())

-- | Pop a frame and return a JVM object.
popWithObject
  :: (ty ~ Ty a, Coercible a, Coerce.Coercible a (J ty), IsReferenceType ty, Monad m)
  => a
  -> m (Pop a)
popWithObject x = return (PopObject x)

-- | Pop a frame and return a value. This value MUST NOT be an object reference
-- created in the popped frame. In that case use 'popWithObject' instead.
popWithValue :: Monad m => a -> m (Pop a)
popWithValue x = return (PopValue x)

-- | Create a local ref and delete it when the given action completes.
withLocalRef
  :: (MonadMask m, MonadIO m, Coerce.Coercible o (J ty))
  => m o -> (o -> m a) -> m a
withLocalRef m = bracket m (liftIO . deleteLocalRef)

-- Note [Class lookup memoization]
--
-- By using unsafeDupablePerformIO, we mark the lookup actions as pure. When the
-- body of the function is inlined within the calling context, the lookups
-- typically become closed expressions, therefore are CAF's that can be floated
-- to top-level by the GHC optimizer.

-- | Tag data types that can be coerced in O(1) time without copy to a Java
-- object or primitive type (i.e. have the same representation) by declaring an
-- instance of this type class for that data type.
class SingI (Ty a) => Coercible a where
  type Ty a :: JType
  coerce :: a -> JValue
  unsafeUncoerce :: JValue -> a

  default coerce
    :: Coerce.Coercible a (J (Ty a))
    => a
    -> JValue
  coerce x = JObject (Coerce.coerce x :: J (Ty a))

  default unsafeUncoerce
    :: Coerce.Coercible (J (Ty a)) a
    => JValue
    -> a
  unsafeUncoerce (JObject obj) = Coerce.coerce (unsafeCast obj :: J (Ty a))
  unsafeUncoerce _ =
      error "Cannot unsafeUncoerce: object expected but value of primitive type found."

-- | The identity instance.
instance SingI ty => Coercible (J ty) where
  type Ty (J ty) = ty

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

instance Coercible Bool where
  type Ty Bool = 'Prim "boolean"
  coerce x = JBoolean (fromIntegral (fromEnum x))
  unsafeUncoerce (JBoolean x) = toEnum (fromIntegral x)
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible CChar where
  type Ty CChar = 'Prim "byte"
  coerce = JByte
  unsafeUncoerce (JByte x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Char where
  type Ty Char = 'Prim "char"
  coerce x = JChar (fromIntegral (ord x))
  unsafeUncoerce (JChar x) = chr (fromIntegral x)
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Word16 where
  type Ty Word16 = 'Prim "char"
  coerce = JChar
  unsafeUncoerce (JChar x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Int16 where
  type Ty Int16 = 'Prim "short"
  coerce = JShort
  unsafeUncoerce (JShort x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Int32 where
  type Ty Int32 = 'Prim "int"
  coerce = JInt
  unsafeUncoerce (JInt x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Int64 where
  type Ty Int64 = 'Prim "long"
  coerce = JLong
  unsafeUncoerce (JLong x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Float where
  type Ty Float = 'Prim "float"
  coerce = JFloat
  unsafeUncoerce (JFloat x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible Double where
  type Ty Double = 'Prim "double"
  coerce = JDouble
  unsafeUncoerce (JDouble x) = x
  unsafeUncoerce val = withTypeRep (throw . CoercionFailure val)
instance Coercible () where
  type Ty () = 'Void
  coerce = error "Void value undefined."
  unsafeUncoerce _ = ()
instance Coercible (Choice.Choice a) where
  type Ty (Choice.Choice a) = 'Prim "boolean"
  coerce = coerce . Choice.toBool
  unsafeUncoerce = Choice.fromBool . unsafeUncoerce

-- | Inject a value (of primitive or reference type) to a 'JValue'. This
-- datatype is useful for e.g. passing arguments as a list of homogeneous type.
-- Synonym for 'coerce'.
jvalue :: (ty ~ Ty a, Coercible a) => a -> JValue
jvalue = coerce

-- | If @ty@ is a reference type, then it should be possible to get an object
-- from a value.
jobject :: (ty ~ Ty a, Coercible a, IsReferenceType ty) => a -> J ty
jobject x
  | JObject jobj <- coerce x = unsafeCast jobj
  | otherwise = error "impossible"

-- | Get the Java class of an object or anything 'Coercible' to one.
classOf
  :: forall a sym. (Ty a ~ 'Class sym, Coercible a, KnownSymbol sym)
  => a
  -> JNI.String
classOf x = JNI.fromChars (symbolVal (Proxy :: Proxy sym)) `const` coerce x

-- | @VariadicIO_ f@ constraints @f@ to be of the form
--
-- > a₁ -> ... -> aₙ -> IO b
--
-- for any value of @n@, where the context provides
--
-- > (Coercible a₁, ... , Coercible aₙ)
--
class VariadicIO_ f where
  -- | The singletons of the argument types of @f@.
  --
  -- > sings (Proxy (a₁ -> ... -> aₙ -> IO b) =
  -- >   [SomeSing (sing @a₁), ... , SomeSing (sing @aₙ)]
  --
  sings :: Proxy f -> [SomeSing JType]

  -- | @apply g a₁ ... aₙ = g [coerce a₁, ... , coerce aₙ]@
  apply :: ([JValue] -> IO (ReturnTypeIO f)) -> f

-- | The return type of a variadic function
--
-- In general,
--
-- > ReturnTypeIO (a₁ -> ... -> aₙ -> IO b) = b
--
-- We keep it as a standalone type family to enable
-- the definition of the catch-all @VariadicIO_ x@ instance.
type family ReturnTypeIO f :: Data.Kind.Type

-- | Document that a function is variadic
--
-- @VariadicIO f b@ constraints @f@ to be of the form
--
-- > a₁ -> ... -> aₙ -> IO b
--
-- for any value of @n@, where the context provides
--
-- > (Coercible a₁, ... , Coercible aₙ)
--
type VariadicIO f b = (ReturnTypeIO f ~ b, VariadicIO_ f)

type instance ReturnTypeIO (IO a) = a

instance VariadicIO_ (IO a) where
  sings _ = []
  apply f = f []

type instance ReturnTypeIO (a -> f) = ReturnTypeIO f

instance (Coercible a, VariadicIO_ f) => VariadicIO_ (a -> f) where
  sings _ = SomeSing (sing @(Ty a)) : sings @f Proxy
  apply f x = apply (\xs -> f (coerce x : xs))

-- All errors of the form "Could not deduce (VariadicIO_ x) from ..."
-- are replaced with the following type error.
instance
  {-# OVERLAPPABLE #-}
  TypeError (TypeError.Text "Expected: a₁ -> ... -> aₙ -> IO b" TypeError.:$$:
             TypeError.Text "Actual: " TypeError.:<>: TypeError.ShowType x) =>
  VariadicIO_ x where
  sings = undefined
  apply = undefined

-- | Creates a new instance of the class whose name is resolved from the return
-- type. For instance,
--
-- @
-- do x :: 'J' (''Class' "java.lang.Integer") <- new 42
--    return x
-- @
--
-- You can pass any number of 'Coercible' arguments to the constructor.
new
  :: forall a f sym.
     ( Ty a ~ 'Class sym
     , Coerce.Coercible a (J ('Class sym))
     , Coercible a
     , VariadicIO f a
     ) => f
{-# INLINE new #-}
new = apply $ \args -> Coerce.coerce <$> newJ @sym (sings @f Proxy) args

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
newArray :: forall ty. SingI ty => Int32 -> IO (J ('Array ty))
{-# INLINE newArray #-}
newArray sz = do
    let tysing = sing @ty
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
                lk <- getClass tysing
                gk <- newGlobalRef lk
                deleteLocalRef lk
                return gk
          unsafeCast <$> newObjectArray sz klass

-- | Creates an array from a list of references.
toArray
  :: forall ty. (SingI ty, IsReferenceType ty)
  => [J ty]
  -> IO (J ('Array ty))
toArray xs = do
    let n = fromIntegral (length xs)
    jxs <- newArray n
    zipWithM_ (setObjectArrayElement jxs) [0 .. n - 1] xs
    return jxs

-- | The Swiss Army knife for calling Java methods. Give it an object or any
-- data type coercible to one and any number of 'Coercible' arguments. Based on
-- the types of each argument, and based on the return type, 'call' will invoke
-- the named method using of the @call*Method@ family of functions in the JNI
-- API.
--
-- When the method name is overloaded, use 'upcast' or 'unsafeCast'
-- appropriately on the class instance and/or on the arguments to invoke the
-- right method.
--
-- Example:
--
-- @
-- call obj "frobnicate" x y z
-- @
call
  :: forall a b ty f.
  ( VariadicIO f b
  , ty ~ Ty a
  , IsReferenceType ty
  , Coercible a
  , Coercible b
  , Coerce.Coercible a (J ty)
  )
  => a
  -> JNI.String
  -> f
call obj mname = apply $ \args ->
    unsafeUncoerce <$>
    callToJValue
      (sing @(Ty b))
      (Coerce.coerce obj :: J ty)
      mname
      (sings @f Proxy)
      args

-- | Same as 'call', but for static methods.
--
-- Example:
--
-- @
-- callStatic "java.lang.Integer" "parseInt" jstr
-- @
callStatic
  :: forall a ty f.
     (ty ~ Ty a, Coercible a, VariadicIO f a)
  => JNI.String -- ^ Class name
  -> JNI.String -- ^ Method name
  -> f
{-# INLINE callStatic #-}
callStatic cname mname = apply $ \args ->
   unsafeUncoerce <$>
     callStaticToJValue (sing @ty) cname mname (sings @f Proxy) args

-- | Get a static field.
getStaticField
  :: forall a ty. (ty ~ Ty a, Coercible a)
  => JNI.String -- ^ Class name
  -> JNI.String -- ^ Static field name
  -> IO a
{-# INLINE getStaticField #-}
getStaticField cname fname =
    unsafeUncoerce <$> getStaticFieldAsJValue (sing @ty) cname fname

-- | The 'Interp' type family is used by both 'Reify' and 'Reflect'. In order to
-- benefit from @-XGeneralizedNewtypeDeriving@ of new instances, we make this an
-- /associated/ type family instead of a standalone one.
class (SingI (Interp a), IsReferenceType (Interp a)) => Interpretation (a :: k) where
  -- | Map a Haskell type to the symbolic representation of a Java type.
  type Interp a :: JType

-- | Extract a concrete Haskell value from the space of Java objects. That is to
-- say, unmarshall a Java object to a Haskell value. Unlike coercing, in general
-- reifying induces allocations and copies.
class Interpretation a => Reify a where
  -- | Invariant: The result and the argument share no direct JVM object
  -- references.
  reify :: J (Interp a) -> IO a

  default reify :: (Coercible a, Interp a ~ Ty a) => J (Interp a) -> IO a
  reify x = unsafeUncoerce . JObject <$> (newLocalRef x :: IO (J (Ty a)))

-- | Inject a concrete Haskell value into the space of Java objects. That is to
-- say, marshall a Haskell value to a Java object. Unlike coercing, in general
-- reflection induces allocations and copies.
class Interpretation a => Reflect a where
  -- | Invariant: The result and the argument share no direct JVM object
  -- references.
  reflect :: a -> IO (J (Interp a))

  default reflect :: (Coercible a, Interp a ~ Ty a) => a -> IO (J (Interp a))
  reflect x = newLocalRef (jobject x)

-- | A newtype wrapper for representing Java values that can be null
newtype Nullable a = Nullable (Maybe a)
  deriving (Eq, Ord, Show)

pattern Null :: Nullable a
pattern Null <- Nullable Nothing where
  Null = Nullable Nothing

pattern NotNull :: a -> Nullable a
pattern NotNull a <- Nullable (Just a) where
  NotNull a = Nullable (Just a)

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
    fptr <- newForeignPtr ptr $ submitToFinalizerThread $
      finalize jobj ptr `finally` deleteGlobalRefNonFinalized jobj
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

withStatic [d|
  instance (SingI ty, IsReferenceType ty) => Interpretation (J ty) where type Interp (J ty) = ty
  instance Interpretation (J ty) => Reify (J ty)
  instance Interpretation (J ty) => Reflect (J ty)

  -- Ugly work around the fact that java has no equivalent of the 'unit' type:
  -- We take an arbitrary serializable type to represent it.
  instance Interpretation () where type Interp () = 'Class "java.lang.Short"
  instance Reify () where reify _ = return ()
  instance Reflect () where reflect () = new (0 :: Int16)

  instance Interpretation ByteString where
    type Interp ByteString = 'Array ('Prim "byte")

  instance Reify ByteString where
    reify jobj = do
        n <- getArrayLength (unsafeCast jobj)
        bytes <- getByteArrayElements jobj
        -- TODO could use unsafePackCStringLen instead and avoid a copy if we knew
        -- that been handed an (immutable) copy via JNI isCopy ref.
        bs <- BS.packCStringLen (bytes, fromIntegral n)
        releaseByteArrayElements jobj bytes
        return bs

  instance Reflect ByteString where
    reflect bs = BS.unsafeUseAsCStringLen bs $ \(content, n) -> do
        arr <- newByteArray (fromIntegral n)
        setByteArrayRegion arr 0 (fromIntegral n) content
        return arr

  instance Interpretation Bool where
    type Interp Bool = 'Class "java.lang.Boolean"

  instance Reify Bool where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- getClass (SClass "java.lang.Boolean")
              m <- getMethodID klass "booleanValue"
                     (methodSignature [] (SPrim "boolean"))
              deleteLocalRef klass
              return m
        callBooleanMethod jobj method []

  instance Reflect Bool where
    reflect = new

  instance Interpretation CChar where
    type Interp CChar = 'Class "java.lang.Byte"

  instance Reify CChar where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- getClass (SClass "java.lang.Byte")
              m <- getMethodID klass "byteValue"
                     (methodSignature [] (SPrim "byte"))
              deleteLocalRef klass
              return m
        callByteMethod jobj method []

  instance Reflect CChar where
    reflect = Language.Java.Unsafe.new

  instance Interpretation Int16 where
    type Interp Int16 = 'Class "java.lang.Short"

  instance Reify Int16 where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- getClass (SClass "java.lang.Short")
              m <- getMethodID klass "shortValue"
                     (methodSignature [] (SPrim "short"))
              deleteLocalRef klass
              return m
        callShortMethod jobj method []

  instance Reflect Int16 where
    reflect = new

  instance Interpretation Int32 where
    type Interp Int32 = 'Class "java.lang.Integer"

  instance Reify Int32 where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- getClass (SClass "java.lang.Integer")
              m <- getMethodID klass "intValue"
                     (methodSignature [] (SPrim "int"))
              deleteLocalRef klass
              return m
        callIntMethod jobj method []

  instance Reflect Int32 where
    reflect = new

  instance Interpretation Int64 where
    type Interp Int64 = 'Class "java.lang.Long"

  instance Reify Int64 where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- getClass (SClass "java.lang.Long")
              m <- getMethodID klass "longValue"
                     (methodSignature [] (SPrim "long"))
              deleteLocalRef klass
              return m
        callLongMethod jobj method []

  instance Reflect Int64 where
    reflect = new

  instance Interpretation Word16 where
    type Interp Word16 = 'Class "java.lang.Character"

  instance Reify Word16 where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- getClass (SClass "java.lang.Character")
              m <- getMethodID klass "charValue"
                     (methodSignature [] (SPrim "char"))
              deleteLocalRef klass
              return m
        fromIntegral <$> callCharMethod jobj method []

  instance Reflect Word16 where
    reflect = new

  instance Interpretation Double where
    type Interp Double = 'Class "java.lang.Double"

  instance Reify Double where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- getClass (SClass "java.lang.Double")
              m <- getMethodID klass "doubleValue"
                     (methodSignature [] (SPrim "double"))
              deleteLocalRef klass
              return m
        callDoubleMethod jobj method []

  instance Reflect Double where
    reflect = new

  instance Interpretation Float where
    type Interp Float = 'Class "java.lang.Float"

  instance Reify Float where
    reify jobj = do
        let method = unsafeDupablePerformIO $ do
              klass <- getClass (SClass "java.lang.Float")
              m <- getMethodID klass "floatValue"
                     (methodSignature [] (SPrim "float"))
              deleteLocalRef klass
              return m
        callFloatMethod jobj method []

  instance Reflect Float where
    reflect = new

  instance Interpretation a => Interpretation (Nullable a) where
    type Interp (Nullable a) = Interp a

  instance Reify a => Reify (Nullable a) where
    reify jobj = if jobj == jnull then return Null else NotNull <$> reify jobj

  instance Reflect a => Reflect (Nullable a) where
    reflect Null = return jnull
    reflect (NotNull a) = reflect a

  instance Interpretation Text where
    type Interp Text = 'Class "java.lang.String"

  instance Reify Text where
    reify jobj = do
        sz <- getStringLength jobj
        cs <- getStringChars jobj
        txt <- Text.decodeUtf16LEWith (\_ _ -> Just '?') <$> BS.packCStringLen (castPtr cs, fromIntegral sz * 2)
        releaseStringChars jobj cs
        return txt

  instance Reflect Text where
    reflect x =
        BS.useAsCStringLen (Text.encodeUtf16LE x) $ \(ptr, len) ->
          newString (castPtr ptr) (fromIntegral len `div` 2)

  newtype W8Bool = W8Bool { fromW8Bool :: Word8 }
    deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Storable)

  instance Interpretation (IOVector W8Bool) where
    type Interp (IOVector W8Bool) = 'Array ('Prim "boolean")

  instance Reify (IOVector W8Bool) where
    reify = fmap (unsafeCoerce :: IOVector Word8 -> IOVector W8Bool) .
            reifyMVector getBooleanArrayElements releaseBooleanArrayElements

  instance Reflect (IOVector W8Bool) where
    reflect = reflectMVector newBooleanArray setBooleanArrayRegion .
              (unsafeCoerce :: IOVector W8Bool -> IOVector Word8)

  instance Interpretation (IOVector CChar) where
    type Interp (IOVector CChar) = 'Array ('Prim "byte")

  instance Reify (IOVector CChar) where
    reify = reifyMVector getByteArrayElements releaseByteArrayElements

  instance Reflect (IOVector CChar) where
    reflect = reflectMVector newByteArray setByteArrayRegion

  instance Interpretation (IOVector Word16) where
    type Interp (IOVector Word16) = 'Array ('Prim "char")

  instance Reify (IOVector Word16) where
    reify = reifyMVector getCharArrayElements releaseCharArrayElements

  instance Reflect (IOVector Word16) where
    reflect = reflectMVector newCharArray setCharArrayRegion

  instance Interpretation (IOVector Int16) where
    type Interp (IOVector Int16) = 'Array ('Prim "short")

  instance Reify (IOVector Int16) where
    reify = reifyMVector getShortArrayElements releaseShortArrayElements

  instance Reflect (IOVector Int16) where
    reflect = reflectMVector newShortArray setShortArrayRegion

  instance Interpretation (IOVector Int32) where
    type Interp (IOVector Int32) = 'Array ('Prim "int")

  instance Reify (IOVector Int32) where
    reify = reifyMVector (getIntArrayElements) (releaseIntArrayElements)

  instance Reflect (IOVector Int32) where
    reflect = reflectMVector (newIntArray) (setIntArrayRegion)

  instance Interpretation (IOVector Int64) where
    type Interp (IOVector Int64) = 'Array ('Prim "long")

  instance Reify (IOVector Int64) where
    reify = reifyMVector getLongArrayElements releaseLongArrayElements

  instance Reflect (IOVector Int64) where
    reflect = reflectMVector newLongArray setLongArrayRegion

  instance Interpretation (IOVector Float) where
    type Interp (IOVector Float) = 'Array ('Prim "float")

  instance Reify (IOVector Float) where
    reify = reifyMVector getFloatArrayElements releaseFloatArrayElements

  instance Reflect (IOVector Float) where
    reflect = reflectMVector newFloatArray setFloatArrayRegion

  instance Interpretation (IOVector Double) where
    type Interp (IOVector Double) = 'Array ('Prim "double")

  instance Reify (IOVector Double) where
    reify = reifyMVector (getDoubleArrayElements) (releaseDoubleArrayElements)

  instance Reflect (IOVector Double) where
    reflect = reflectMVector (newDoubleArray) (setDoubleArrayRegion)

  instance (SingI (Interp (IOVector a)), IsReferenceType (Interp (IOVector a)), Interpretation (IOVector a))
              => Interpretation (Vector a) where
    type Interp (Vector a) = Interp (IOVector a)

  instance (Storable a, Reify (IOVector a)) => Reify (Vector a) where
    reify = Vector.freeze <=< reify

  instance (Storable a, Reflect (IOVector a)) => Reflect (Vector a) where
    reflect = reflect <=< Vector.thaw

  instance Interpretation a => Interpretation [a] where
    type Interp [a] = 'Array (Interp a)

  instance Reify a => Reify [a] where
    reify jobj = do
        n <- getArrayLength jobj
        forM [0..n-1] $ \i -> do
          jx <- getObjectArrayElement jobj i
          x  <- reify jx
          deleteLocalRef jx
          return x

  instance Reflect a => Reflect [a] where
    reflect xs = do
      let n = fromIntegral (length xs)
      array <- newArray n :: IO (J ('Array (Interp a)))
      forM_ (zip [0..n-1] xs) $ \(i, x) -> do
        jx <- reflect x
        setObjectArrayElement array i jx
        deleteLocalRef jx
      return array
  |]
