module Foreign.JNI.Internal where

import qualified Foreign.JNI.String as JNI

-- | A reference type name is not just any 'JNI.String', but a fully qualified
-- identifier well-formed by construction.
newtype ReferenceTypeName = ReferenceTypeName JNI.String
  deriving (Eq, Ord)

instance Show ReferenceTypeName where
  show (ReferenceTypeName str) = show str

-- | A string representing a signature, well-formed by construction.
newtype Signature = Signature JNI.String
  deriving (Eq, Ord)

instance Show Signature where
  show (Signature str) = show str

-- | A string representing a signature, well-formed by construction.
newtype MethodSignature = MethodSignature JNI.String
  deriving (Eq, Ord)

instance Show MethodSignature where
  show (MethodSignature str) = show str
