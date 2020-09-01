module Main where

import Control.Concurrent (runInBoundThread)
import Foreign.JNI
import qualified SafeSpec
import qualified Spec
import Test.Hspec

main :: IO ()
main = withJVM [] $ hspec $
    around_ (runInBoundThread . runInAttachedThread) $ do
      Spec.spec
      SafeSpec.spec
