# jvm-batching

Provides batched marshalling of values between Java and Haskell.

# Using it as a dependency

Add `jvm-batching` to the list of dependencies in your .cabal file.
Then edit the `Setup.hs` file to add the `jvm-batching.jar` to the
classpath.

```Haskell
import Distribution.Simple
import Language.Java.Inline.Cabal
import qualified Language.Java.Batching.Jars

main = do
    jars <- Language.Java.Batching.Jars.getJars
    defaultMainWithHooks (addJarsToClasspath jars simpleUserHooks)
```

Add a `custom-setup` stanza to your .cabal file.

```
custom-setup
  setup-depends:
    base,
    Cabal,
    inline-java,
    jvm-batching
```
