module Main where

import Foreign.JNI (withJVM)
import qualified Spec
import Test.Hspec

main :: IO ()
main = withJVM [] $ hspec Spec.spec
