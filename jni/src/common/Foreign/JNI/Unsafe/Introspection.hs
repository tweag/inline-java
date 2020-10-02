{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Foreign.JNI.Unsafe.Introspection
 ( getClassName
 , getSignatures
 , toString
 , isInstanceOf
 , classGetNameMethod
 ) where

import Control.Monad (forM)
import Data.Foldable (fold)
import Data.Maybe (catMaybes)
import Data.ByteString.Builder (toLazyByteString, word16LE)
import Data.ByteString.Lazy (toStrict)
import Data.Singletons
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf16LE)
import Foreign.JNI.Unsafe.Internal
import Foreign.JNI.Types
import qualified Foreign.JNI.String as JNI
import Foreign.Marshal.Array
import Prelude hiding (String)

-- | The "Class" class.
kclass :: IO JClass
kclass = findClass $ referenceTypeName $ sing @('Class "java.lang.Class")

-- | The "Method" class
kmethod :: IO JClass
kmethod = findClass $ referenceTypeName $ sing @('Class "java.lang.reflect.Method")

-- | Class.getMethods
classGetMethodsMethod :: IO JMethodID
classGetMethodsMethod = do
  klass <- kclass
  let sig = methodSignature [] (SArray $ sing @('Class "java.lang.reflect.Method"))
  getMethodID klass (JNI.fromChars "getMethods") sig

-- | Method.toString
methodToStringMethod :: IO JMethodID
methodToStringMethod = do
  klass <- kmethod
  let sig = methodSignature [] (sing @('Class "java.lang.String"))
  getMethodID klass (JNI.fromChars "toString") sig

-- | Method.getName
methodGetNameMethod :: IO JMethodID
methodGetNameMethod = do
  klass <- kmethod
  let sig = methodSignature [] (sing @('Class "java.lang.String"))
  getMethodID klass (JNI.fromChars "getName") sig

-- | Class.getName
classGetNameMethod :: IO JMethodID
classGetNameMethod = do
  klass <- kclass
  let sig = methodSignature [] (sing @('Class "java.lang.String"))
  getMethodID klass (JNI.fromChars "getName") sig

-- | Class.isInstance
classIsInstanceMethod :: IO JMethodID
classIsInstanceMethod = do
  klass <- kclass
  let sig = methodSignature [SomeSing $ sing @('Class "java.lang.Object")] (sing @('Prim "boolean"))
  getMethodID klass (JNI.fromChars "isInstance") sig

-- | @getSignatures c methodName@ yields the Java signatures of overloadings of
-- methods called @methodName@ in class @c@.
getSignatures :: JClass -> JNI.String -> IO [JNI.String]
getSignatures c methodName = do
  id <- classGetMethodsMethod
  array :: JObjectArray <- unsafeCast <$> callObjectMethod c id []
  l <- getArrayLength array
  names <- forM [0 .. l - 1] $ \i -> do
    ithObj :: JObject <- getObjectArrayElement array i
    id <- methodGetNameMethod
    jName :: JString <- unsafeCast <$> callObjectMethod ithObj id []
    name <- toString jName
    if name == methodName
      then do
        id <- methodToStringMethod
        jMethodName :: JString <- unsafeCast <$> callObjectMethod ithObj id []
        Just <$> toString jMethodName
      else return Nothing
  return $ catMaybes names

-- | Turns a JString into a JNI String.
-- Tricky, because Java encodes string in utf-16
toString :: JString -> IO JNI.String
toString obj = do
  chars <- getStringChars obj
  length <- fromIntegral <$> getStringLength obj
  words <- peekArray length chars
  return $ JNI.fromChars $ unpack $ decodeUtf16LE $ toStrict $ toLazyByteString $ fold $ word16LE <$> words

-- | @getClassName c@ yields the name of class @c@
getClassName :: JClass -> IO JNI.String
getClassName c = do
    id <- classGetNameMethod
    jName :: JString <- unsafeCast <$> callObjectMethod c id []
    toString jName

isInstanceOf :: JClass -> JObject -> IO Bool
isInstanceOf klass obj = do
  id <- classIsInstanceMethod
  callBooleanMethod klass id [JObject obj]
