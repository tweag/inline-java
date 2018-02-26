# jvm-streaming

Expose Java iterators as streams from the streaming package.

## Using it as a dependency

Add `jvm-streaming` to the list of dependencies in your .cabal file.
Then edit the `Setup.hs` file to add some necessary jars to the
classpath.

```Haskell
import Distribution.Simple
import Language.Java.Inline.Cabal
import qualified Language.Java.Streaming.Jars

main = do
    jars <- Language.Java.Streaming.Jars.getJars
    defaultMainWithHooks (addJarsToClasspath jars simpleUserHooks)
```

Add a `custom-setup` stanza to your .cabal file.

```
custom-setup
  setup-depends:
    base,
    Cabal >= 1.24,
    inline-java,
    jvm-streaming
```

## Layout of source directories

This is a multi-language package. We use
Maven's [standard directory layout][maven-sdl] to organize source code
in multiple languages side-by-side.

[maven-sdl]: https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html
