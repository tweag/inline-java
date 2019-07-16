module Main where

import Language.Java (withJVM)
import qualified SafeSpec
import qualified Spec
import Test.Hspec

main :: IO ()
main = withJVM [] $ hspec $ do
    Spec.spec
    SafeSpec.spec
