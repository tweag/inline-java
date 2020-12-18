# jvm-batching

Provides batched marshalling of values between Java and Haskell.

It provides reify and reflect instances for vectors, which marshal
values in batches, which is more efficient than marshalling values
one at a time.

```Haskell
instance (Interpretation a, BatchReify a) => Reify (V.Vector a) where
  ...

instance (Interpretation a, BatchReflect a) => Reflect (V.Vector a) where
  ...
```

See the documentation in
[Language.Hava.Batching](src/main/haskell/Language/Java/Batching.hs)
for an overview on how the implementation works.

## Using it as a dependency

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

## Layout of source directories

This is a multi-language package. We use
Maven's [standard directory layout][maven-sdl] to organize source code
in multiple languages side-by-side.

[maven-sdl]: https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html

## Create Maven artifacts

We use Gradle to generate and sign all artifacts required to publish
jvm-batching in Maven Central.

### Get the latest version of Gradle

Get SDKMAN! (https://sdkman.io/)
$ curl -s "https://get.sdkman.io" | bash

Install Gradle (https://gradle.org/)
$ sdk install gradle

### (Only once) Create and register a GPG key

$ gpg --quick-generate-key jvm-batching

Get the 8-length alphanumeric ID of your key through:

$ gpg --list-keys --keyid-format SHORT

Export your key with:
$ gpg --export-secret-keys -o ~/.gnupg/secret.gpg

Add the following lines to `~/.gradle/gradle.properties` (create if nonexistent):

```
signing.keyId=AAAAAAAA
signing.password=foo
signing.secretKeyRingFile=$HOMEDIR/.gnupg/secret.gpg
```

### Generate artifacts with Gradle

$ gradle publish

All necessary artifacts are generated in `build/releases/io/tweag/jvm-batching/$VERSION`