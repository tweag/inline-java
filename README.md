# inline-java: Call any JVM function from Haskell

[![CircleCI](https://circleci.com/gh/tweag/inline-java.svg?style=svg)](https://circleci.com/gh/tweag/inline-java)
[![Build status](https://badge.buildkite.com/143d77b1eec06bb865d694dbe685f2ed7712caa12852c8808e.svg?branch=master)](https://buildkite.com/tweag-1/inline-java)

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
* the [Bazel][bazel] build tool, and
* the [Nix][nix] package manager.

To build:

```
$ nix-shell --pure --run "bazel build //..."
```

To test:

```
$ nix-shell --pure --run "bazel test //..."
```

[bazel]: https://bazel.build
[nix]: http://nixos.org/nix

## Using the safe interface

There is [an experimental interface][safe-inline-java] which catches
common memory management mistakes at compile time via the
[LinearTypes][linear-types-proposal] language extension.

For examples of how to use the safe interface you can check the
[tests][tests], the [directory server][directory-server] example and the
[wizzardo-http benchmark][wizzardo-http-benchmark].

[directory-server]: examples/directory-server
[linear-types-ghc]: https://github.com/tweag/ghc/tree/linear-types#ghc-branch-with-linear-types
[linear-types-proposal]: https://github.com/tweag/ghc-proposals/blob/linear-types2/proposals/0000-linear-types.rst
[safe-inline-java]: https://github.com/tweag/inline-java/blob/master/src/linear-types/Language/Java/Inline/Safe.hs
[tests]: tests/linear-types/Language/Java/Inline/SafeSpec.hs
[wizzardo-http-benchmark]: benchmarks/wizzardo-http

## Further reading

Check the [tutorial][inline-java-tutorial] on how to use `inline-java`.
If you want to know more about how it is implemented, look at
[our post][inline-java-plugin] on the plugin implementation.

There is also a post which gives an overview of the
[safe interface][safe-interface-post].

[inline-java-tutorial]: https://www.tweag.io/posts/2017-09-15-inline-java-tutorial.html
[inline-java-plugin]: https://www.tweag.io/posts/2017-09-22-inline-java-ghc-plugin.html
[safe-interface-post]: https://www.tweag.io/blog/2020-02-06-safe-inline-java

## Debugging

The generated java output can be dumped to stderr by passing to GHC
```
-fplugin-opt=Language.Java.Inline.Plugin:dump-java
```

If `-ddump-to-file` is in effect, the java code is dumped to
`<module>.dump-java` instead.

## Troubleshooting

### Build-time error `package or class Blah does not exist`

`inline-java` is going to invoke the `javac` compiler, and any classes
used in `java` quotations need to be reachable via the `CLASSPATH`
environment variable. For instance,
```
CLASSPATH=/path/to/my.jar:/some/other/path ghc --make program.hs
```

### Run-time error `ThreadNotAttached`

Haskell threads need to be attached to the JVM before making JNI calls.
`Foreign.JNI.withJVM` attaches the calling thread, and other threads
can be attached with `Foreign.JNI.runInAttachedThread`. When the JVM
calls into Haskell, the thread is already attached.

### Run-time error `ThreadNotBound`

JNI calls need to be done from bound threads. The thread invoking the
`main` function of a program is bound. Threads created with `forkOS`
are bound. In other threads, `Control.Concurrent.runInBoundThread`
can be used to run a computation in a bound thread.

### Run-time error `java.lang.NoClassDefFoundError`

Classes might not be found at runtime if they are not in a folder or
jar listed in the parameter `-Djava.class.path=<classpath>` passed
to `withJVM`.

```Haskell
withJVM ["-Djava.class.path=/path/to/my.jar:/some/other/path"] $ do
  ...
```

Additionally, classes might not be found if a thread other than the one
calling `main` is trying to use them. One solution is to have the thread
calling `main` load all the classes in advance. Then the classes will
be available in the JVM for other threads that need them.
Calling `Language.Java.Inline.loadJavaWrappers` will have the effect of
loading all classes needed for `java` quotations, which will suffice in
many cases.

Another option is to set the context class loader of other threads,
so they earn the ability to load classes on their own. This might
work when the thread was attached to the JVM via the JNI, and
the context class loader is just `null`.

```Haskell
loader <- [java| Thread.currentThread().getContextClassLoader() |]
            `Language.Java.withLocalRef` Foreign.JNI.newGlobalRef
...
forkOS $ runInAttachedThread $ do
  [java| { Thread.currentThread().setContextClassLoader($loader); } |]
  ...
```

### Run-time error `JVMException`

Any java exception that goes from Java to Haskell will be wrapped
as a value of type `JVMException` with a reference to the Java object
representing the exception. The message and the stack trace of the
exception can be retrieved from the exception object with more JNI
calls, e.g.

```Haskell
\(JVMException e) -> [java| { $e.printStackTrace(); } |]
```
or with `JNI.Foreign.showException`.

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

inline-java is maintained by [Tweag I/O](https://www.tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).
