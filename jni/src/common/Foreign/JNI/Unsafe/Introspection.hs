{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
getSignatures :: JClass -> JNI.String -> IO [JNI.String]
getSignatures c methodName = do
  array :: JObjectArray <- unsafeCast <$> callObjectMethod c classGetMethodsMethod []
  l <- getArrayLength array
  names <- forM [0 .. l - 1] $ \i -> do
    ithObj :: JObject <- getObjectArrayElement array i
    jName :: JString <- unsafeCast <$> callObjectMethod ithObj methodGetNameMethod []
    name <- toString jName
    if name == methodName
      then do
        jMethodName :: JString <- unsafeCast <$> callObjectMethod ithObj methodToStringMethod []
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
    jName :: JString <- unsafeCast <$> callObjectMethod c classGetNameMethod []
    toString jName

isInstanceOf :: JClass -> JObject -> IO Bool
isInstanceOf klass obj =
  callBooleanMethod klass classIsInstanceMethod [JObject obj]
