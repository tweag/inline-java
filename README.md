# inline-java: Call any JVM function from Haskell

[![CircleCI](https://circleci.com/gh/tweag/inline-java.svg?style=svg)](https://circleci.com/gh/tweag/inline-java)
[![Build status](https://badge.buildkite.com/143d77b1eec06bb865d694dbe685f2ed7712caa12852c8808e.svg)](https://buildkite.com/tweag-1/inline-java)

The Haskell standard includes a native foreign function interface
(FFI). Using it can be a bit involved and only C support is
implemented in GHC. `inline-java` lets you call any JVM function
directly, from Haskell, without the need to write your own foreign
import declarations using the FFI. In the style of `inline-c` for
C and `inline-r` for calling R, `inline-java` lets you name any
function to call inline in your code. It is implemented on top of the
[jni][jni] and [jvm][jvm] packages using a [GHC Core plugin][ghc-plugins]
to orchestrate compilation and loading of the inlined Java snippets.

[jni]: jni/
[jvm]: jvm/
[ghc-plugins]: https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/extending_ghc.html#core-plugins-in-more-detail

## Example

Graphical Hello World using Java Swing:

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Language.Java
import Language.Java.Inline

main :: IO ()
main = withJVM [] $ do
    message <- reflect ("Hello World!" :: Text)
    [java| {
      javax.swing.JOptionPane.showMessageDialog(null, $message);
      } |]
```

## Building it

**Requirements:**
* the [Stack][stack] build tool (version 1.2 or above);
* either, the [Nix][nix] package manager,
* or, OpenJDK and Gradle installed from your distro.

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

## Building the safe interface

There is [an experimental interface][safe-interface] which catches
common memory management mistakes at compile time. This interface
currently needs a [fork][linear-types-ghc] of GHC which supports the
[LinearTypes][linear-types-proposal] language extension. Both the GHC
fork and the safe interface can be built with:

```
$ stack --nix --stack-yaml stack-linear.yaml build inline-java
```

For examples of how to use the safe interface you can check
the [directory server][directory-server] or the
[wizzardo-http benchmark][wizzardo-http-benchmark].


[directory-server]: examples/directory-server
[linear-types-ghc]: https://github.com/tweag/ghc/tree/linear-types#ghc-branch-with-linear-types
[linear-types-proposal]: https://github.com/tweag/ghc-proposals/blob/linear-types2/proposals/0000-linear-types.rst
[safe-inline-java]: https://github.com/tweag/inline-java/blob/master/src/linear-types/Language/Java/Inline/Safe.hs
[wizzardo-http-benchmark]: benchmarks/wizzardo-http

## Further reading

Check the [tutorial][inline-java-tutorial] on how to use `inline-java`.
If you want to know more about how it is implemented, look at
[our post][inline-java-plugin] on the plugin implementation.

[inline-java-tutorial]: https://www.tweag.io/posts/2017-09-15-inline-java-tutorial.html
[inline-java-plugin]: https://www.tweag.io/posts/2017-09-22-inline-java-ghc-plugin.html

## Debugging

The generated java output can be dumped to stderr by passing to GHC
```
-fplugin-opt=Language.Java.Inline.Plugin:dump-java
```

If `-ddump-to-file` is in effect (as when using `stack`), the java code
is dumped to `<module>.dump-java` instead.

## License

Copyright (c) 2015-2016 EURL Tweag.

All rights reserved.

inline-java is free software, and may be redistributed under the terms
specified in the [LICENSE](LICENSE) file.

## Sponsors

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[<img src="https://www.tweag.io/img/tweag-med.png" height="65">](http://tweag.io)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![LeapYear](http://i.imgur.com/t9VxRHn.png)](http://leapyear.io)

inline-java is maintained by [Tweag I/O](https://www.tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).
