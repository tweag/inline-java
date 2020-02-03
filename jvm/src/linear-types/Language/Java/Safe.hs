-- | A linear interface for functions in Language.Java
--
-- These are high-level helper functions for interacting with Java objects,
-- mapping them to Haskell values and vice versa. The 'Reify' and 'Reflect'
-- classes together are to Java what "Foreign.Storable" is to C: they
-- provide a means to marshall/unmarshall Java objects from/to Haskell data
-- types.
--
-- A typical pattern for wrapping Java API's using this module is:
--
-- @
-- {&#45;\# LANGUAGE DataKinds \#&#45;}
-- {&#45;\# LANGUAGE DeriveAnyClass \#&#45;}
-- module Object where
--
-- import Language.Java.Safe as J
--
-- newtype Object = Object ('J' (''Class' "java.lang.Object"))
--   deriving (J.Coercible, J.Interpretation, J.Reify, J.Reflect)
--
-- clone :: Object ->. Linear.IO Object
-- clone obj = J.'call' obj "clone" End
--
-- equals :: Object ->. Object ->. Linear.IO Bool
-- equals obj1 obj2 = J.'call' obj1 "equals" obj2 End
--
-- ...
-- @
--
-- To call Java methods using quasiquoted Java syntax instead, see
-- "Language.Java.Inline.Safe".
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Java.Safe
  ( module Foreign.JNI.Types.Safe
  -- * JVM instance management
  , withJVM
  -- * JVM calls
  , classOf
  , new
  , newArray
  , toArray
  , call
  , callStatic
  , getStaticField
  , End(..)
  , Variadic
  -- * Coercions
  , Coercible(..)
  , jvalue
  , jobject
  -- * Conversions
  , Interpretation(..)
  , Reify(..)
  , Reflect(..)
  , reify_
  -- * Re-exports
  , sing
  ) where

import Control.Exception (evaluate)
import Control.Monad.IO.Class.Linear (MonadIO, liftIO, liftIOU)
import Control.Monad.Linear hiding ((<$>))
import Data.ByteString (ByteString)
import qualified Data.Choice as Choice
import qualified Data.Coerce as Coerce
import Data.Int
import Data.Singletons (SingI(..), SomeSing(..))
import Data.Text (Text)
import Data.Typeable
import qualified Data.Unrestricted.Linear as Unrestricted
import Data.Vector.Storable (Vector)
import Data.Vector.Storable.Mutable (IOVector)
import Data.Word
import Foreign.C (CChar)
import qualified Foreign.JNI as JNI
import Foreign.JNI.Safe
import Foreign.JNI.Types.Safe
import qualified Foreign.JNI.String as JNI
import GHC.TypeLits (KnownSymbol, TypeError, symbolVal)
import qualified GHC.TypeLits as TypeError (ErrorMessage(..))

import qualified Language.Java as Java
import qualified Language.Java.Internal as Java
import Prelude ((.))
import qualified Prelude
import Prelude.Linear hiding ((.))
import qualified Unsafe.Linear as Unsafe


-- | A linear variant of "Java.Coercible".
--
-- All types that wrap tracked references can implement
-- an instance of this class.
class SingI (Ty a) => Coercible a where
  type Ty a :: JType
  coerce :: a ->. JValue
  unsafeUncoerce :: JValue ->. a

  default coerce
    :: Coerce.Coercible a (J (Ty a))
    => a
    ->. JValue
  coerce x = JObject (Unsafe.toLinear Coerce.coerce x :: J (Ty a))

  default unsafeUncoerce
    :: Coerce.Coercible (J (Ty a)) a
    => JValue
    ->. a
  unsafeUncoerce = Unsafe.toLinear $ \case
    JObject obj -> Coerce.coerce (unsafeCast obj :: J (Ty a))
    v -> error Prelude.$
      "Cannot unsafeUncoerce: object expected but value of primitive type found.: "
      ++ show v

instance SingI ty => Coercible (J ty) where
  type Ty (J ty) = ty

withTypeRep :: Typeable a => (TypeRep -> a) -> a
withTypeRep f = let x = f (typeOf x) in x

coercePrim :: Java.Coercible a => a ->. JValue
coercePrim x = JValue (Unsafe.toLinear Java.coerce x)

unsafeUncoercePrim :: (Typeable a, Java.Coercible a) => JValue ->. a
unsafeUncoercePrim = Unsafe.toLinear $ \case
    JValue v -> Java.unsafeUncoerce v
    val -> withTypeRep
      (\r -> error ("unsafeUncoercePrim can't uncoerce a reference: "
                      ++ show (val, r)
                   )
      )

instance Coercible Bool where
  type Ty Bool = Java.Ty Bool
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible CChar where
  type Ty CChar = Java.Ty CChar
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Char where
  type Ty Char = Java.Ty Char
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Word16 where
  type Ty Word16 = Java.Ty Word16
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Int16 where
  type Ty Int16 = Java.Ty Int16
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Int32 where
  type Ty Int32 = Java.Ty Int32
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Int64 where
  type Ty Int64 = Java.Ty Int64
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Float where
  type Ty Float = Java.Ty Float
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible Double where
  type Ty Double = Java.Ty Double
  coerce = coercePrim
  unsafeUncoerce = unsafeUncoercePrim
instance Coercible () where
  type Ty () = Java.Ty ()
  coerce = error "Void value undefined."
  unsafeUncoerce = Unsafe.toLinear (const ())
instance Coercible (Choice.Choice a) where
  type Ty (Choice.Choice a) = Java.Ty Bool
  coerce c = coerce (Unsafe.toLinear Choice.toBool c)
  unsafeUncoerce v = Unsafe.toLinear Choice.fromBool (unsafeUncoerce v)

instance (IsPrimitiveType (Ty a), Java.Coercible a, Typeable a)
         => Coercible (Unrestricted a) where
  type Ty (Unrestricted a) = Java.Ty a
  coerce (Unrestricted a) = JValue (Java.coerce a)
  unsafeUncoerce = Unsafe.toLinear $ \v ->
    Unsafe.toLinear (Unrestricted $!) (unsafeUncoercePrim v)

instance (IsReferenceType (Java.Ty a), Java.Coercible a, Typeable a)
         => Coercible (UnsafeUnrestrictedReference a) where
  type Ty (UnsafeUnrestrictedReference a) = Java.Ty a
  coerce (UnsafeUnrestrictedReference a) = JValue (Java.coerce a)
  unsafeUncoerce = Unsafe.toLinear $ \case
    JObject j ->
      UnsafeUnrestrictedReference (Java.unsafeUncoerce (Java.JObject (unJ j)))
    v -> withTypeRep
      (\r -> error ("unsafeUncoerce: unexpected primitive type for: "
                      ++ show (v, r)
                   )
      )

-- | Get the Java class of an object or anything 'Coercible' to one.
classOf
  :: forall a sym. (Ty a ~ 'Class sym, Coercible a, KnownSymbol sym)
  => a
  ->. (a, JNI.String)
classOf = Unsafe.toLinear $ \x -> (,) x $
  JNI.fromChars (symbolVal (Proxy :: Proxy sym)) `const` coerce x

-- | A sentinel value to end the list of arguments in variadic
-- functions
data End = End

-- | @Variadic_ f@ constraints @f@ to be of the form
--
-- > f :: a₁ ->. ... ->. aₙ ->. End -> ReturnType f
--
-- for any value of @n@, where the context provides
--
-- > (Coercible a₁, ... , Coercible aₙ)
--
class Variadic_ f where
  -- | The singletons of the argument types of @f@.
  --
  -- > sings (Proxy (a₁ ->. ... ->. aₙ ->. End -> r) =
  -- >   [SomeSing (sing @a₁), ... , SomeSing (sing @aₙ)]
  --
  sings :: Proxy f -> [SomeSing JType]

  -- | @apply g a₁ ... aₙ End = g [coerce a₁, ... , coerce aₙ]@
  apply :: ([JValue] ->. ReturnType f) ->. f

instance Variadic_ (End -> r) where
  sings _ = []
  apply f End = f []

instance (Coercible a, Variadic_ f) => Variadic_ (a ->. f) where
  sings _ = SomeSing (sing :: Sing (Ty a)) : sings @f Proxy
  apply f x = apply (\xs -> f (coerce x : xs))

-- All errors of the form "Could not deduce (Variadic_ x) from ..."
-- are replaced with the following type error.
instance
  {-# OVERLAPPABLE #-}
  TypeError ('TypeError.Text "Expected: a₁ ->. ... ->. aₙ ->. End -> r" 'TypeError.:$$:
             'TypeError.Text "Actual: " 'TypeError.:<>: 'TypeError.ShowType x) =>
  Variadic_ x where
  sings = undefined
  apply = undefined

-- | The return type of a variadic function
--
-- In general,
--
-- > ReturnType (a₁ ->. ... ->. aₙ ->. End -> r) = r
--
-- We keep it as a standalone type family to enable
-- the definition of the catch-all @Variadic_ x@ instance.
type family ReturnType f :: * where
  ReturnType (End -> r) = r
  ReturnType (a ->. f) = ReturnType f

-- | @VariadicIO f b@ constraints @f@ to be of the form
--
-- > a₁ ->. ... ->. aₙ ->. End -> IO b
--
-- for any value of @n@, where the context provides
--
-- > (Coercible a₁, ... , Coercible aₙ)
--
type Variadic f r = (ReturnType f ~ r, Variadic_ f)

-- | Creates a new instance of the class whose name is resolved from the return
-- type. For instance,
--
-- @
-- do x :: 'J' (''Class' "java.lang.Integer") <- new (42 :: Int32) End
--    return x
-- @
new
  :: forall a sym m f.
     (Ty a ~ 'Class sym, Coercible a, MonadIO m, Variadic f (m a))
  => f
{-# INLINE new #-}
new = apply $
    Unsafe.toLinear $ \args -> fmap unsafeUncoerce $ liftIO Prelude.$ do
    JObject . J <$> Java.newJ @sym (sings @f Proxy) (toJNIJValues args)
      Prelude.<* deleteLinearJObjects args

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
newArray :: (MonadIO m, SingI ty) => Int32 -> m (J ('Array ty))
{-# INLINE newArray #-}
newArray sz = liftIO (J <$> Java.newArray sz)

-- | Creates an array from a list of references.
toArray
  :: (SingI ty, IsReferenceType ty, MonadIO m)
  => [J ty]
  ->. m ([J ty], J ('Array ty))
toArray = Unsafe.toLinear $ \xs ->
  liftIO ((,) xs . J <$> Java.toArray (Coerce.coerce xs))

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
  :: forall a b ty1 ty2 m f.
     ( ty1 ~ Ty a
     , ty2 ~ Ty b
     , IsReferenceType ty1
     , Coercible a
     , Coercible b
     , Coerce.Coercible a (J ty1)
     , MonadIO m
     , Variadic f (m b)
     )
  => a -- ^ Any object or value 'Coercible' to one
  ->. JNI.String -- ^ Method name
  -> f
{-# INLINE call #-}
call = Unsafe.toLinear $ \obj mname -> apply $ Unsafe.toLinear $ \args -> do
    liftIO Prelude.$ strictUnsafeUncoerce Prelude.$ do
      fromJNIJValue <$>
        Java.callToJValue
          @ty1
          (sing :: Sing ty1)
          (Coerce.coerce obj)
          mname
          (sings @f Proxy)
          (toJNIJValues args)
        Prelude.<* deleteLinearJObjects args

strictUnsafeUncoerce :: Coercible a => IO JValue -> IO a
strictUnsafeUncoerce m = m Prelude.>>= \x -> evaluate (unsafeUncoerce x)

fromJNIJValue :: Java.JValue -> JValue
fromJNIJValue = \case
    Java.JObject j -> JObject (J j)
    v -> JValue v

-- | Same as 'call', but for static methods.
callStatic
  :: forall a ty m f. (ty ~ Ty a, Coercible a, MonadIO m, Variadic f (m a))
  => JNI.String -- ^ Class name
  -> JNI.String -- ^ Method name
  -> f
{-# INLINE callStatic #-}
callStatic cname mname = apply $ Unsafe.toLinear $ \args -> do
    liftIO Prelude.$ strictUnsafeUncoerce Prelude.$
      fromJNIJValue <$>
      Java.callStaticToJValue
        (sing :: Sing ty)
        cname mname
        (sings @f Proxy)
        (toJNIJValues args)
        Prelude.<* deleteLinearJObjects args

-- | Get a static field.
getStaticField
  :: forall a ty m. (ty ~ Ty a, Coercible a, MonadIO m)
  => JNI.String -- ^ Class name
  -> JNI.String -- ^ Static field name
  -> m a
getStaticField cname fname =
    liftIO Prelude.$ strictUnsafeUncoerce Prelude.$
      fromJNIJValue <$>
        Java.getStaticFieldAsJValue (sing :: Sing ty) cname fname

-- | Inject a value (of primitive or reference type) to a 'JValue'. This
-- datatype is useful for e.g. passing arguments as a list of homogeneous type.
-- Synonym for 'coerce'.
jvalue :: (ty ~ Ty a, Coercible a) => a ->. JValue
jvalue = coerce

-- | If @ty@ is a reference type, then it should be possible to get an object
-- from a value.
jobject :: (ty ~ Ty a, Coercible a, IsReferenceType ty) => a ->. J ty
jobject = Unsafe.toLinear $ \x ->
  case coerce x of
    JObject jobj -> unsafeCast jobj
    _ -> error "impossible"

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
  reify :: MonadIO m => J (Interp a) ->. m (J (Interp a), Unrestricted a)

  default reify
    :: (Java.Coercible a, Interp a ~ Java.Ty a, MonadIO m)
    => J (Interp a)
    ->. m (J (Interp a), Unrestricted a)
  reify = Unsafe.toLinear $ \x -> fmap ((,) x) $
      liftIOU Prelude.$
        Java.unsafeUncoerce . Java.JObject <$> JNI.newLocalRef (unJ x)

reify_ :: (Reify a, MonadIO m) => J (Interp a) ->. m (Unrestricted a)
reify_ _j = reify _j >>= \(_j, a) -> a <$ deleteLocalRef _j

-- | Inject a concrete Haskell value into the space of Java objects. That is to
-- say, marshall a Haskell value to a Java object. Unlike coercing, in general
-- reflection induces allocations and copies.
class Interpretation a => Reflect a where
  -- | Invariant: The result and the argument share no direct JVM object
  -- references.
  reflect :: MonadIO m => a -> m (J (Interp a))

  default reflect
    :: (Java.Coercible a, Interp a ~ Java.Ty a, MonadIO m)
    => a
    -> m (J (Interp a))
  reflect x = liftIO (J <$> JNI.newLocalRef (Java.jobject x))

instance (SingI ty, IsReferenceType ty) => Interpretation (Java.J ty) where type Interp (Java.J ty) = ty
instance Interpretation (Java.J ty) => Reify (Java.J ty)
instance Interpretation (Java.J ty) => Reflect (Java.J ty)

javaReify
  :: (Java.Interp a ~ Interp a, Java.Reify a, MonadIO m)
  => J (Interp a)
  ->. m (J (Interp a), Unrestricted a)
javaReify = Unsafe.toLinear $ \j ->
    liftIO ((,) j . Unrestricted <$> Java.reify (unJ j))

javaReflect
  :: (Java.Interp a ~ Interp a, Java.Reflect a, MonadIO m)
  => a
  -> m (J (Interp a))
javaReflect a = fmap J $ liftIO (Java.reflect a)

instance Interpretation () where type Interp () = Java.Interp ()
instance Reify () where reify = javaReify
instance Reflect () where reflect = javaReflect

instance Interpretation ByteString where type Interp ByteString = Java.Interp ByteString
instance Reify ByteString where reify = javaReify
instance Reflect ByteString where reflect = javaReflect

instance Interpretation Bool where type Interp Bool = Java.Interp Bool
instance Reify Bool where reify = javaReify
instance Reflect Bool where reflect = javaReflect

instance Interpretation CChar where type Interp CChar = Java.Interp CChar
instance Reify CChar where reify = javaReify
instance Reflect CChar where reflect = javaReflect

instance Interpretation Int16 where type Interp Int16 = Java.Interp Int16
instance Reify Int16 where reify = javaReify
instance Reflect Int16 where reflect = javaReflect

instance Interpretation Word16 where type Interp Word16 = Java.Interp Word16
instance Reify Word16 where reify = javaReify
instance Reflect Word16 where reflect = javaReflect

instance Interpretation Int32 where type Interp Int32 = Java.Interp Int32
instance Reify Int32 where reify = javaReify
instance Reflect Int32 where reflect = javaReflect

instance Interpretation Int64 where type Interp Int64 = Java.Interp Int64
instance Reify Int64 where reify = javaReify
instance Reflect Int64 where reflect = javaReflect

instance Interpretation Float where type Interp Float = Java.Interp Float
instance Reify Float where reify = javaReify
instance Reflect Float where reflect = javaReflect

instance Interpretation Double where type Interp Double = Java.Interp Double
instance Reify Double where reify = javaReify
instance Reflect Double where reflect = javaReflect

instance Interpretation Text where type Interp Text = Java.Interp Text
instance Reify Text where reify = javaReify
instance Reflect Text where reflect = javaReflect

instance Interpretation (IOVector Word16) where
  type Interp (IOVector Word16) = Java.Interp (IOVector Word16)
instance Reify (IOVector Word16) where reify = javaReify
instance Reflect (IOVector Word16) where reflect = javaReflect

instance Interpretation (IOVector Int16) where
  type Interp (IOVector Int16) = Java.Interp (IOVector Int16)
instance Reify (IOVector Int16) where reify = javaReify
instance Reflect (IOVector Int16) where reflect = javaReflect

instance Interpretation (IOVector Int32) where
  type Interp (IOVector Int32) = Java.Interp (IOVector Int32)
instance Reify (IOVector Int32) where reify = javaReify
instance Reflect (IOVector Int32) where reflect = javaReflect

instance Interpretation (IOVector Int64) where
  type Interp (IOVector Int64) = Java.Interp (IOVector Int64)
instance Reify (IOVector Int64) where reify = javaReify
instance Reflect (IOVector Int64) where reflect = javaReflect

instance Interpretation (IOVector Float) where
  type Interp (IOVector Float) = Java.Interp (IOVector Float)
instance Reify (IOVector Float) where reify = javaReify
instance Reflect (IOVector Float) where reflect = javaReflect

instance Interpretation (IOVector Double) where
  type Interp (IOVector Double) = Java.Interp (IOVector Double)
instance Reify (IOVector Double) where reify = javaReify
instance Reflect (IOVector Double) where reflect = javaReflect

instance (SingI (Interp (Vector a)), IsReferenceType (Interp (Vector a)))
  => Interpretation (Vector a) where
  type Interp (Vector a) = Java.Interp (Vector a)
instance Java.Reify (Vector a) => Reify (Vector a) where
  reify = javaReify
instance Java.Reflect (Vector a) => Reflect (Vector a) where
  reflect = javaReflect

instance Interpretation a => Interpretation [a] where
  type Interp [a] = 'Array (Interp a)

instance Reify a => Reify [a] where
  reify _jobj =
      getArrayLength _jobj >>= \(_jobj, Unrestricted n) ->
      foldM
        (\(_jobj, uxs) i ->
          getObjectArrayElement _jobj i >>= \(_jobj, jx) ->
          reify_ jx >>= \ux ->
          return (_jobj, Unrestricted.lift2 (:) ux uxs)
        )
        (_jobj, Unrestricted []) [n-1,n-2..0]

instance Reflect a => Reflect [a] where
  reflect xs =
      let n = fromIntegral (length xs)
       in newArray n >>= \array ->
      foldM
        (\array0 (Unrestricted (i, x)) ->
            reflect x >>= \jx ->
            setObjectArrayElement_ array0 i jx
        )
        array (map Unrestricted (zip [0..n-1] xs))
