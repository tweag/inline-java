# inline-java: Call any JVM function from Haskell

[![Circle CI](https://circleci.com/gh/tweag/inline-java.svg?style=svg)](https://circleci.com/gh/tweag/inline-java)

**NOTE: you'll need GHC >= 8.0.2 to compile and use this package. Use**
```
stack --nix --stack-yaml stack-HEAD.yaml build
```
**ahead of a new GHC release to build using GHC HEAD.**

The Haskell standard includes a native foreign function interface
(FFI). It can be a pain to use and in any case only C support is
implemented in GHC. `inline-java` lets you call any JVM function
directly, from Haskell, without the need to write your own foreign
import declarations using the FFI. In the style of `inline-c` for C and
`inline-r` for calling R, `inline-java` lets you name any function to
call inline in your code.

# Example

Graphical Hello World using Java Swing:

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Int
import Data.Text (Text)
import Language.Java
import Language.Java.Inline

main :: IO Int32
main = withJVM [] $ do
    -- Extra type annotation workaround for current GHC HEAD.
    message :: J ('Class "java.lang.String") <- reflect ("Hello World!" :: Text)
    [java| { javax.swing.JOptionPane.showMessageDialog(null, $message);
             return 0; } |]
```

## License

Copyright (c) 2015-2016 EURL Tweag.

All rights reserved.

inline-java is free software, and may be redistributed under the terms
specified in the [LICENSE](LICENSE) file.

## About

![Tweag I/O](http://i.imgur.com/0HK8X4y.png)

inline-java is maintained by [Tweag I/O](http://tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).
