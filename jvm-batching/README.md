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

To use it as a dependency add `:jvm-batching` and `jvm-batching:jar` to
the list of dependencies of your haskell binary or library.

## Layout of source directories

This is a multi-language package. We use
Maven's [standard directory layout][maven-sdl] to organize source code
in multiple languages side-by-side.

[maven-sdl]: https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html
