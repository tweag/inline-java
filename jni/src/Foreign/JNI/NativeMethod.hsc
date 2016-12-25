-- | Bindings to the JNINativeMethod struct.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Foreign.JNI.NativeMethod where

import qualified Data.ByteString.Unsafe as BS
import qualified Foreign.JNI.String as JNI
import Foreign.Ptr (FunPtr)
import Foreign.Storable (Storable(..))

#include <jni.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

data JNINativeMethod = forall a. JNINativeMethod
  { jniNativeMethodName :: JNI.String
  , jniNativeMethodSignature :: JNI.String
  , jniNativeMethodFunPtr :: FunPtr a
  }

instance Storable JNINativeMethod where
  sizeOf _ = #{size JNINativeMethod}
  alignment _ = #{alignment JNINativeMethod}
  peek ptr = do
      name <- BS.unsafePackCString =<< #{peek JNINativeMethod, name} ptr
      sig <- BS.unsafePackCString =<< #{peek JNINativeMethod, signature} ptr
      fptr <- #{peek JNINativeMethod, fnPtr} ptr
      return $
        JNINativeMethod
          (JNI.unsafeFromByteString name)
          (JNI.unsafeFromByteString sig)
          fptr
  poke ptr JNINativeMethod{..} = do
      JNI.withString jniNativeMethodName $ #{poke JNINativeMethod, name} ptr
      JNI.withString jniNativeMethodSignature $ #{poke JNINativeMethod, signature} ptr
      #{poke JNINativeMethod, fnPtr} ptr jniNativeMethodFunPtr
