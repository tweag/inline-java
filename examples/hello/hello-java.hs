{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Foreign.JNI (withJVM)
import Language.Java (reify, reflect)
import Language.Java.Inline

main :: IO ()
main = withJVM [] $ do
    text <- reflect (Text.pack "Java: Hello!")
    jarray <- [java| {
      System.out.println($text);
      return new String[] {"Haskell:", "Hello", "Java!" };
      } |]
    xs <- reify jarray
    Text.putStrLn (Text.unwords xs)
