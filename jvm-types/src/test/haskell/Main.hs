module Main where

import Control.Concurrent (runInBoundThread)
import Control.Exception (handle, throwIO)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Foreign.JNI
import Language.Java.Inline (loadJavaWrappers)
import qualified Spec
import Test.Hspec
import System.IO (stderr)


main :: IO ()
main = do
    let classpath = "jvm-batching/libjar.jar:jvm-types/libjar.jar"
    withJVM [Text.encodeUtf8 $ Text.pack $ "-Djava.class.path=" ++ classpath] $ do
      loadJavaWrappers -- causes classes in jvm-batching.jar to be loaded
                       -- since they are used in java quotations
      hspec $ around_ (runInBoundThread . runInAttachedThread . printExceptions) Spec.spec
  where
    printExceptions = handle $ \e ->
      showException e >>= Text.hPutStrLn stderr >> throwIO e
