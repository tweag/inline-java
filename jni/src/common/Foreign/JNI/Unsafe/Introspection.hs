{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Foreign.JNI.Unsafe.Introspection
 ( getClassName
 , getSignatures
 , toText
 , isInstanceOf
 , classGetNameMethod
 ) where

import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Data.Singletons
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Foreign as Text
import Foreign.JNI.Unsafe.Internal
import Foreign.JNI.Types
import qualified Foreign.JNI.String as JNI
import Prelude hiding (String)
import System.IO.Unsafe (unsafePerformIO)

-- | The "Class" class.
kclass :: JClass
{-# NOINLINE kclass #-}
kclass = unsafePerformIO $
  findClass $ referenceTypeName $ sing @('Class "java.lang.Class")

-- | The "Method" class
kmethod :: JClass
{-# NOINLINE kmethod #-}
kmethod = unsafePerformIO $
  findClass $ referenceTypeName $ sing @('Class "java.lang.reflect.Method")

-- | Class.getMethods
classGetMethodsMethod :: JMethodID
{-# NOINLINE classGetMethodsMethod #-}
classGetMethodsMethod = unsafePerformIO $
  getMethodID kclass (JNI.fromChars "getMethods") $
    methodSignature [] (SArray $ sing @('Class "java.lang.reflect.Method"))

-- | Method.toString
methodToStringMethod :: JMethodID
{-# NOINLINE methodToStringMethod #-}
methodToStringMethod = unsafePerformIO $
  getMethodID kmethod (JNI.fromChars "toString") $
    methodSignature [] (sing @('Class "java.lang.String"))

-- | Method.getName
methodGetNameMethod :: JMethodID
{-# NOINLINE methodGetNameMethod #-}
methodGetNameMethod = unsafePerformIO $
  getMethodID kmethod (JNI.fromChars "getName") $
    methodSignature [] (sing @('Class "java.lang.String"))

-- | Class.getName
classGetNameMethod :: JMethodID
{-# NOINLINE classGetNameMethod #-}
classGetNameMethod = unsafePerformIO $
  getMethodID kclass (JNI.fromChars "getName") $
    methodSignature [] (sing @('Class "java.lang.String"))

-- | Class.isInstance
classIsInstanceMethod :: JMethodID
{-# NOINLINE classIsInstanceMethod #-}
classIsInstanceMethod = unsafePerformIO $ do
  getMethodID kclass (JNI.fromChars "isInstance") $
    methodSignature [SomeSing $ sing @('Class "java.lang.Object")] (sing @('Prim "boolean"))

-- | @getSignatures c methodName@ yields the Java signatures of overloadings of
-- methods called @methodName@ in class @c@.
getSignatures :: JClass -> JNI.String -> IO [Text.Text]
getSignatures c methodName = do
  let methodNameTxt = Text.decodeUtf8 $ JNI.toByteString methodName
  array :: JObjectArray <- unsafeCast <$> callObjectMethod c classGetMethodsMethod []
  l <- getArrayLength array
  names <- forM [0 .. l - 1] $ \i -> do
    ithObj :: JObject <- getObjectArrayElement array i
    jName :: JString <- unsafeCast <$> callObjectMethod ithObj methodGetNameMethod []
    name <- toText jName
    if name == methodNameTxt
      then do
        jMethodName :: JString <- unsafeCast <$> callObjectMethod ithObj methodToStringMethod []
        Just <$> toText jMethodName
      else return Nothing
  return $ catMaybes names

-- | Turns a JString into Text.
toText :: JString -> IO Text.Text
toText obj = do
  cs <- getStringChars obj
  sz <- fromIntegral <$> getStringLength obj
  txt <- Text.fromPtr cs sz
  releaseStringChars obj cs
  return txt

-- | @getClassName c@ yields the name of class @c@
getClassName :: JClass -> IO Text.Text
getClassName c = do
    jName :: JString <- unsafeCast <$> callObjectMethod c classGetNameMethod []
    toText jName

isInstanceOf :: JClass -> JObject -> IO Bool
isInstanceOf klass obj =
  callBooleanMethod klass classIsInstanceMethod [JObject obj]
