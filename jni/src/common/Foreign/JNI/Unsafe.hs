{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Low-level bindings to the Java Native Interface (JNI).
--
-- Read the
-- <https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html JNI spec>
-- for authoritative documentation as to what each of the functions in
-- this module does. The names of the bindings in this module were chosen to
-- match the names of the functions in the JNI spec.
--
-- All bindings in this module access the JNI via a thread-local variable of
-- type @JNIEnv *@. If the current OS thread has not yet been "attached" to the
-- JVM, it needs to be attached. See 'JNI.runInAttachedThread'.
--
-- The 'String' type in this module is the type of JNI strings. See
-- "Foreign.JNI.String".
--
-- The functions in this module are considered unsafe in opposition
-- to those in "Foreign.JNI.Safe", which ensure that local references are not
-- leaked.

-- Reexports definitions from "Foreign.JNI.Unsafe.Internal".
module Foreign.JNI.Unsafe
  ( module Foreign.JNI.Unsafe.Internal
  , NoSuchMethod(..)
  , getMethodID
  , getStaticMethodID
  ) where

import Control.Exception (Exception, bracket, catch, throwIO)
import Data.Singletons (sing)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.JNI.Types
import Foreign.JNI.Internal (MethodSignature(..))
import qualified Foreign.JNI.String as JNI
import qualified Foreign.JNI.Unsafe.Internal as Internal
import Foreign.JNI.Unsafe.Internal hiding (getMethodID, getStaticMethodID)
import Foreign.JNI.Unsafe.Internal.Introspection
import System.IO.Unsafe (unsafePerformIO)

-- | Throws when a method can't be found
data NoSuchMethod = NoSuchMethod
  { noSuchMethodClassName :: Text.Text
  , noSuchMethodName ::  Text.Text
  , noSuchMethodSignature :: Text.Text
  , noSuchMethodCandidates :: [Text.Text]
  } deriving Exception

instance Show NoSuchMethod where
  show exn = Text.unpack $ Text.unwords $
    case noSuchMethodCandidates exn of
      [] ->
        [ "No method named", noSuchMethodName exn
        , "was found in class", noSuchMethodClassName exn
        , ".\nWas there a mispelling ?"
        ]
      sigs ->
        [ "No method named", noSuchMethodName exn
        , "has signature", noSuchMethodSignature exn
        , "in class", noSuchMethodClassName exn
        , ".\nThe candidate methods are:"
        , Text.unlines sigs
        ]

getMethodID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> MethodSignature -- ^ JNI signature
  -> IO JMethodID
getMethodID cls method sig = catch
  (Internal.getMethodID cls method sig)
  (handleJVMException cls method sig)

getStaticMethodID
  :: JClass -- ^ A class object as returned by 'findClass'
  -> JNI.String -- ^ Field name
  -> MethodSignature -- ^ JNI signature
  -> IO JMethodID
getStaticMethodID cls method sig = catch
  (Internal.getStaticMethodID cls method sig)
  (handleJVMException cls method sig)

-- | The "NoSuchMethodError" class.
kexception :: JClass
{-# NOINLINE kexception #-}
kexception = unsafePerformIO $ bracket
  (findClass $ referenceTypeName $ sing @('Class "java.lang.NoSuchMethodError"))
  deleteLocalRef
  newGlobalRef

-- | When the exception is an instance of java.lang.NoSuchMethodError,
-- throw a verbose exception suggesting possible corrections.
-- If it isn't, throw it as is.
handleJVMException
  :: JClass
  -> JNI.String
  -> MethodSignature
  -> JVMException
  -> IO a
handleJVMException cls method (MethodSignature sig) (JVMException e) =
  isInstanceOf (upcast e) kexception >>= \case
    True -> do
      noSuchMethodCandidates <- getSignatures cls method
      noSuchMethodClassName <- getClassName cls
      throwIO $ NoSuchMethod
        { noSuchMethodClassName
        , noSuchMethodName = Text.decodeUtf8 (JNI.toByteString method)
        , noSuchMethodSignature = Text.decodeUtf8 (JNI.toByteString sig)
        , noSuchMethodCandidates
        }
    False -> throwIO $ JVMException e
