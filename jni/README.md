# jni: Complete JNI raw bindings

[![jni on Stackage LTS](http://stackage.org/package/jni/badge/lts)](http://stackage.org/lts/package/jni)
[![jni on Stackage Nightly](http://stackage.org/package/jni/badge/nightly)](http://stackage.org/nightly/package/jni)

This package includes Haskell bindings to the
[Java Native Interface (JNI)][jni]. This package offers mostly
unopinionated low-level bindings, which closely maps to the C API. For
higher-level more opinionated bindings see the [jvm][haskell-jvm] and
[inline-java][inline-java] packages.

[jni]: https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html
[haskell-jvm]: https://github.com/tweag/inline-java/tree/master/jvm
[inline-java]: https://github.com/tweag/inline-java/

## Building

The recommended build method is with Stack and Nix:

```
$ stack --nix build
```

Without Nix, you must help Stack find the JDK. Set the `JDK_LIBDIR`
and `JNI_H` environment variables appropriately, then issue:

```
stack --install-ghc \
   --extra-lib-dirs="$JDK_LIBDIR"/jre/lib/server \
   --extra-include-dirs="$JNI_H" \
   build
```
