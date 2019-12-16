# jvm: Call any JVM function from Haskell

[![jvm on Stackage LTS](http://stackage.org/package/jvm/badge/lts)](http://stackage.org/lts/package/jvm)
[![jvm on Stackage Nightly](http://stackage.org/package/jvm/badge/nightly)](http://stackage.org/nightly/package/jvm)

This package enables calling any JVM function from Haskell. If you'd
like to call JVM methods using Java syntax and hence get the Java
compiler to scope check and type check all your foreign calls, see
[inline-java][inline-java], which builds on top of this package.

[inline-java]: https://github.com/tweag/inline-java#readme

# Example

Graphical Hello World using Java Swing:

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Text (Text)
import Language.Java

newtype JOptionPane = JOptionPane (J ('Class "javax.swing.JOptionPane"))
  deriving Coercible

main :: IO ()
main = withJVM [] $ do
    message <- reflect ("Hello World!" :: Text)
    callStatic
      (classOf (undefined :: JOptionPane))
      "showMessageDialog"
      nullComponent
      (upcast message)
  where
    nullComponent :: J ('Class "java.awt.Component")
    nullComponent = jnull
```
