-- | Internal functions to invoke JNI methods
--
-- The functions in this module avoid using
-- 'Language.Java.Coercible' so they can be reused in interfaces which
-- use other ways to convert between Haskell and Java values.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Java.Internal
  ( newJ
  , callToJValue
  , callStaticToJValue
  , getStaticFieldAsJValue
  , getClass
  , setGetClassFunction
  -- * Template Haskell
  , maxVariadicArgs
  , mkVariadic
  ) where

import Data.IORef
import Data.Singletons (SingI(..), SomeSing(..))
import Data.Traversable (for)
import Foreign.JNI hiding (throw)
import Foreign.JNI.Types
import qualified Foreign.JNI.String as JNI
import qualified Language.Haskell.TH as TH
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

-- | Sets the function to use for loading classes.
--
-- 'findClass' is used by default.
--
setGetClassFunction
  :: (forall ty. IsReferenceType ty => Sing (ty :: JType) -> IO JClass)
  -> IO ()
setGetClassFunction f = writeIORef getClassFunctionRef $ GetClassFun f

-- | Yields a class referece. It behaves as 'findClass' unless
-- 'setGetClassFunction' is used.
getClass :: IsReferenceType ty => Sing (ty :: JType) -> IO JClass
getClass s = readIORef getClassFunctionRef >>= \(GetClassFun f) -> f s

newtype GetClassFun =
    GetClassFun (forall ty. IsReferenceType ty =>
                   Sing (ty :: JType) -> IO JClass
                )

{-# NOINLINE getClassFunctionRef #-}
getClassFunctionRef :: IORef GetClassFun
getClassFunctionRef =
    unsafePerformIO $ newIORef (GetClassFun (findClass . referenceTypeName))

newJ
  :: forall sym ty.
     ( ty ~ 'Class sym
     , SingI ty
     )
  => [SomeSing JType] -- ^ Singletons of argument types
  -> [JValue]
  -> IO (J ty)
{-# INLINE newJ #-}
newJ argsings args = do
    let voidsing = sing :: Sing 'Void
        klass = unsafeDupablePerformIO $ do
          lk <- getClass (sing :: Sing ('Class sym))
          gk <- newGlobalRef lk
          deleteLocalRef lk
          return gk
    unsafeCast <$> newObject klass (methodSignature argsings voidsing) args

callToJValue
  :: forall ty1 k. (IsReferenceType ty1, SingI ty1)
  => Sing (k :: JType)
  -> J ty1 -- ^ Any object
  -> JNI.String -- ^ Method name
  -> [SomeSing JType] -- ^ Singletons of argument types
  -> [JValue] -- ^ Arguments
  -> IO JValue
{-# INLINE callToJValue #-}
callToJValue retsing obj mname argsings args = do
    let klass = unsafeDupablePerformIO $ do
                  lk <- getClass (sing :: Sing ty1)
                  gk <- newGlobalRef lk
                  deleteLocalRef lk
                  return gk
        method = unsafeDupablePerformIO $ getMethodID klass mname (methodSignature argsings retsing)
    case retsing of
      SPrim "boolean" -> JBoolean . fromIntegral . fromEnum <$>
                           callBooleanMethod obj method args
      SPrim "byte" -> JByte <$> callByteMethod obj method args
      SPrim "char" -> JChar <$> callCharMethod obj method args
      SPrim "short" -> JShort <$> callShortMethod obj method args
      SPrim "int" -> JInt <$> callIntMethod obj method args
      SPrim "long" -> JLong <$> callLongMethod obj method args
      SPrim "float" -> JFloat <$> callFloatMethod obj method args
      SPrim "double" -> JDouble <$> callDoubleMethod obj method args

      SVoid -> do
        callVoidMethod obj method args
        -- The void result is not inspected.
        return (error "inspected output of method returning void")
      _ -> JObject <$> callObjectMethod obj method args

callStaticToJValue
  :: Sing (k :: JType)
  -> JNI.String -- ^ Class name
  -> JNI.String -- ^ Method name
  -> [SomeSing JType] -- ^ Singletons of argument types
  -> [JValue] -- ^ Arguments
  -> IO JValue
{-# INLINE callStaticToJValue #-}
callStaticToJValue retsing cname mname argsings args = do
    let klass = unsafeDupablePerformIO $ do
                  lk <- getClass (SClass (JNI.toChars cname))
                  gk <- newGlobalRef lk
                  deleteLocalRef lk
                  return gk
        method = unsafeDupablePerformIO $ getStaticMethodID klass mname (methodSignature argsings retsing)
    case retsing of
      SPrim "boolean" -> JBoolean . fromIntegral . fromEnum <$>
                           callStaticBooleanMethod klass method args
      SPrim "byte" -> JByte <$> callStaticByteMethod klass method args
      SPrim "char" -> JChar <$> callStaticCharMethod klass method args
      SPrim "short" -> JShort <$> callStaticShortMethod klass method args
      SPrim "int" -> JInt <$> callStaticIntMethod klass method args
      SPrim "long" -> JLong <$> callStaticLongMethod klass method args
      SPrim "float" -> JFloat <$> callStaticFloatMethod klass method args
      SPrim "double" -> JDouble <$> callStaticDoubleMethod klass method args
      SVoid -> do
        callStaticVoidMethod klass method args
        -- The void result is not inspected.
        return (error "inspected output of method returning void")
      _ -> JObject <$> callStaticObjectMethod klass method args

getStaticFieldAsJValue
  :: Sing (ty :: JType)
  -> JNI.String -- ^ Class name
  -> JNI.String -- ^ Static field name
  -> IO JValue
{-# INLINE getStaticFieldAsJValue #-}
getStaticFieldAsJValue retsing cname fname = do
  let klass = unsafeDupablePerformIO $ do
                lk <- getClass (SClass (JNI.toChars cname))
                gk <- newGlobalRef lk
                deleteLocalRef lk
                return gk
      field = unsafeDupablePerformIO $ getStaticFieldID klass fname (signature retsing)
  case retsing of
    SPrim "boolean" -> JBoolean <$> getStaticBooleanField klass field
    SPrim "byte" -> JByte <$> getStaticByteField klass field
    SPrim "char" -> JChar <$> getStaticCharField klass field
    SPrim "short" -> JShort <$> getStaticShortField klass field
    SPrim "int" -> JInt <$> getStaticIntField klass field
    SPrim "long" -> JLong <$> getStaticLongField klass field
    SPrim "float" -> JFloat <$> getStaticFloatField klass field
    SPrim "double" -> JDouble <$> getStaticDoubleField klass field
    SVoid -> fail "getStaticField cannot yield an object of type void"
    _ -> JObject <$> getStaticObjectField klass field

-- | The maximum supported number of arguments to variadic functions.
maxVariadicArgs :: Int
maxVariadicArgs = 32

-- | Generate variadic function type class instances.
mkVariadic
  :: -- Return type
     TH.TypeQ
     -- context, action type, argument patterns, argument type singletons, arguments
  -> (TH.TypeQ -> TH.TypeQ -> [TH.PatQ] -> TH.ExpQ -> TH.ExpQ -> TH.DecsQ)
  -> TH.DecsQ
mkVariadic retty k = fmap concat $ for [0..maxVariadicArgs] $ \n -> do
    let -- Coercible type class and Ty associated type defined in downstream module.
        coercible = TH.conT (TH.mkName "Coercible")
        coercibleTy = TH.conT (TH.mkName "Ty")
        xs = [TH.mkName ("x" ++ show m) | m <- [1..n]]
        tyvars = [TH.varT (TH.mkName ("a" ++ show m)) | m <- [1..n]]
        argsings =
          TH.listE [[| SomeSing (sing :: Sing ($coercibleTy $tyvar)) |] | tyvar <- tyvars]
        args = TH.listE [[| coerce $(TH.varE x) |] | x <- xs]
        typ = foldr (TH.appT . TH.appT TH.arrowT) [t| IO $retty|] tyvars
        constraints =
          [[t| $coercible $tyvar |] | tyvar <- tyvars] ++
          [[t| $coercible $retty |]]
        ctx = foldl TH.appT (TH.tupleT (length constraints)) constraints
    k ctx typ (map TH.varP xs) argsings args
