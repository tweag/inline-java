# jvm-streaming

Expose Java iterators as streams from the streaming package.

To use it as a dependency add `:jvm-streaming` and `jvm-batching:jar` to
the list of dependencies of your haskell binary or library.

## Layout of source directories

This is a multi-language package. We use
Maven's [standard directory layout][maven-sdl] to organize source code
in multiple languages side-by-side.

[maven-sdl]: https://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html
