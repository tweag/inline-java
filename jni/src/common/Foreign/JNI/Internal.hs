{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Foreign.JNI.Internal where

import Data.Text (Text)
import qualified Data.Text as Text
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

-- | A string representing a method signature, well-formed by construction.
newtype MethodSignature = MethodSignature JNI.String
  deriving (Eq, Ord)

instance Show MethodSignature where
  show (MethodSignature str) = show str

jniMethodToJavaSignature :: Text -> Either String ([Text], Text)
jniMethodToJavaSignature sig = do
    (argTypes, rest1) <- dropChar '(' sig >>= many toJavaType
    (returnType, rest2) <- dropChar ')' rest1 >>= toJavaType
    if Text.null rest2 then return (argTypes, returnType)
    else Left $ "Unexpected suffix " ++ show rest2
  where
    dropChar c t = case Text.uncons t of
      Just (c', t') | c == c' -> Right t'
      _ -> Left $ unwords
              [ "Expected ", show c, "but found", show t]
    many p = go id
      where
        go acc t = case p t of
          Left _ -> Right (acc [], t)
          Right (x, t') -> go (acc . (x:)) t'
    toJavaType t = case Text.uncons t of
      Just (c, t') ->
        case c of
          'L' -> case Text.breakOn ";" t' of
            (jtype, rest) -> Right (Text.map substSlash jtype, Text.drop 1 rest)
          '[' ->
            fmap (\(jtype, rest) -> (jtype <> "[]", rest)) (toJavaType t')
          'Z' -> Right ("boolean", t')
          'B' -> Right ("byte", t')
          'C' -> Right ("char", t')
          'S' -> Right ("short", t')
          'I' -> Right ("int", t')
          'J' -> Right ("long", t')
          'F' -> Right ("float", t')
          'D' -> Right ("double", t')
          'V' -> Right ("void", t')
          _ -> Left $ "Unexpected char " ++ show c ++ " at " ++ show t'
      Nothing ->
        Left "Unexpected empty text"
    substSlash = \case
      '/' -> '.'
      c -> c
