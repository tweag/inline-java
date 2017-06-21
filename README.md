# inline-java: Call any JVM function from Haskell

[![CircleCI](https://circleci.com/gh/tweag/inline-java.svg?style=svg)](https://circleci.com/gh/tweag/inline-java)

**NOTE: you'll need GHC >= 8.0.2 to compile and use this package.**

The Haskell standard includes a native foreign function interface
(FFI). Using it can be a bit involved and only C support is
implemented in GHC. `inline-java` lets you call any JVM function
directly, from Haskell, without the need to write your own foreign
import declarations using the FFI. In the style of `inline-c` for
C and `inline-r` for calling R, `inline-java` lets you name any
function to call inline in your code. It is implemented on top of the
[jni][jni] and [jvm][jvm] packages.

[jni]: jni/
[jvm]: jvm/

## Example

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
    message <- reflect ("Hello World!" :: Text)
    rc <- [java| { javax.swing.JOptionPane.showMessageDialog(null, $message);
                   return 0; } |]
    reify rc
```

## Building it

**Requirements:**
* the [Stack][stack] build tool (version 1.2 or above);
* either, the [Nix][nix] package manager,
* or, OpenJDK, Gradle and Spark (version 1.6) installed from your distro.

On OS X, you'll need to install the [Legacy Java SE 6][osx-java-se]
runtime when prompted, even when using Nix.

To build:

```
$ stack build
```

You can optionally get Stack to download a JDK in a local sandbox
(using [Nix][nix]) for good build results reproducibility. **This is
the recommended way to build inline-java.** Alternatively, you'll need
it installed through your OS distribution's package manager for the
next steps (and you'll need to tell Stack how to find the JVM header
files and shared libraries).

To use Nix, set the following in your `~/.stack/config.yaml` (or pass
`--nix` to all Stack commands, see the [Stack manual][stack-nix] for
more):

```yaml
nix:
  enable: true
```

[stack]: https://github.com/commercialhaskell/stack
[stack-nix]: https://docs.haskellstack.org/en/stable/nix_integration/#configuration
[nix]: http://nixos.org/nix
[osx-java-se]: https://support.apple.com/kb/dl1572?locale=fr_FR

## License

Copyright (c) 2015-2016 EURL Tweag.

All rights reserved.

inline-java is free software, and may be redistributed under the terms
specified in the [LICENSE](LICENSE) file.

## Sponsors

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![Tweag I/O](http://i.imgur.com/0HK8X4y.png)](http://tweag.io)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![LeapYear](http://i.imgur.com/t9VxRHn.png)](http://leapyear.io)

inline-java is maintained by [Tweag I/O](http://tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).
