module Main where

import Data.List (intersperse)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.Java (withJVM)
import Language.Java.Streaming.Jars (getJars)
import qualified Spec
import Test.Hspec

main :: IO ()
main = do
    jars <- getJars
    let classpath = concat $ intersperse ":" jars
    withJVM [Text.encodeUtf8 $ Text.pack $ "-Djava.class.path=" ++ classpath] $
      hspec Spec.spec
