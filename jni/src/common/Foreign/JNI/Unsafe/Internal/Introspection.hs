{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Foreign.JNI.Unsafe.Internal.Introspection
 ( getClassName
 , getSignatures
 , toText
 , classGetNameMethod
 , showException
 ) where

import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Coerce
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
import GHC.ForeignPtr (ForeignPtr)

-- | The "Class" class.
kclass :: JClass
{-# NOINLINE kclass #-}
kclass = unsafePerformIO $ bracket
  (findClass $ referenceTypeName $ sing @('Class "java.lang.Class"))
  deleteLocalRef
  newGlobalRef

-- | The "Method" class
kmethod :: JClass
{-# NOINLINE kmethod #-}
kmethod = unsafePerformIO $ bracket
  (findClass $ referenceTypeName $ sing @('Class "java.lang.reflect.Method"))
  deleteLocalRef
  newGlobalRef

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

withDeleteLocalRef :: Coercible o (ForeignPtr (J ty)) => IO o -> (o -> IO c) -> IO c
withDeleteLocalRef grab action = bracket grab deleteLocalRef action

-- | @getSignatures c methodName@ yields the Java signatures of overloadings of
-- methods called @methodName@ in class @c@.
getSignatures :: JClass -> JNI.String -> IO [Text.Text]
getSignatures c methodName = withDeleteLocalRef
  (unsafeCast <$> callObjectMethod c classGetMethodsMethod []) $
  \(array :: JObjectArray) -> do
    l <- getArrayLength array
    let methodNameTxt = Text.decodeUtf8 $ JNI.toByteString methodName
    names <- forM [0 .. l - 1] $ \i -> withDeleteLocalRef
      (getObjectArrayElement array i) $
      \(ithObj :: JObject) -> withDeleteLocalRef
        (unsafeCast <$> callObjectMethod ithObj methodGetNameMethod []) $
        \(jName :: JString) -> do
          name <- toText jName
          if name == methodNameTxt
            then withDeleteLocalRef
              (unsafeCast <$> callObjectMethod ithObj methodToStringMethod []) $
              \(jMethodName :: JString) -> Just <$> toText jMethodName
            else return Nothing
    return $ catMaybes names

-- | Turns a JString into Text.
toText :: JString -> IO Text.Text
toText obj = bracket
  (getStringChars obj)
  (releaseStringChars obj) $
  \cs -> do
    sz <- fromIntegral <$> getStringLength obj
    txt <- Text.fromPtr cs sz
    return txt

-- | @getClassName c@ yields the name of class @c@
getClassName :: JClass -> IO Text.Text
getClassName c = withDeleteLocalRef
    (unsafeCast <$> callObjectMethod c classGetNameMethod []) $
    \(jName :: JString) -> toText jName

-- | The "Throwable" interface.
iThrowable :: JClass
{-# NOINLINE iThrowable #-}
iThrowable = unsafePerformIO $ withDeleteLocalRef
  (findClass $ referenceTypeName $ sing @('Class "java.lang.Throwable"))
  newGlobalRef

-- | Throwable.printStackTrace(PrintWriter s)
throwablePrintStackTraceMethod :: JMethodID
{-# NOINLINE throwablePrintStackTraceMethod #-}
throwablePrintStackTraceMethod = unsafePerformIO $
  getMethodID iThrowable (JNI.fromChars "printStackTrace") $
    methodSignature [SomeSing (sing :: Sing ('Class "java.io.PrintWriter"))] (sing :: Sing 'Void)

-- | The "StringWriter" class.
kStringWriter :: JClass
{-# NOINLINE kStringWriter #-}
kStringWriter = unsafePerformIO $ withDeleteLocalRef
  (findClass $ referenceTypeName $ sing @('Class "java.io.StringWriter"))
  newGlobalRef

-- | The "PrintWriter" class.
kPrintWriter :: JClass
{-# NOINLINE kPrintWriter #-}
kPrintWriter = unsafePerformIO $ withDeleteLocalRef
  (findClass $ referenceTypeName $ sing @('Class "java.io.PrintWriter"))
  newGlobalRef

-- | StringWriter.toString
stringWriterToStringMethod :: JMethodID
{-# NOINLINE stringWriterToStringMethod #-}
stringWriterToStringMethod = unsafePerformIO $
  getMethodID kStringWriter (JNI.fromChars "toString") $
    methodSignature [] (sing @('Class "java.lang.String"))

-- | Equivalent Java code:
-- StringWriter stringWriter = new StringWriter();
-- PrintWriter printWriter = new PrintWriter(stringWriter);
-- e.printStackTrace(printWriter);
-- return stringWriter.toString();
showException :: JVMException -> IO Text.Text
showException (JVMException e) = withDeleteLocalRef
  (newObject
    kStringWriter
    (methodSignature [] (sing :: Sing 'Void))
    [])
  $ \stringWriter -> withDeleteLocalRef
    (newObject
      kPrintWriter
      (methodSignature
        [SomeSing (sing :: Sing ('Class "java.io.Writer"))]
        (sing :: Sing 'Void))
      [JObject stringWriter])
    $ \printWriter -> do
      _ <- callVoidMethod
        e
        throwablePrintStackTraceMethod
        [JObject printWriter]
      withDeleteLocalRef
        (unsafeCast <$> callObjectMethod
          stringWriter
          stringWriterToStringMethod
          [])
        toText
