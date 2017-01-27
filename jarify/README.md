# What is Jarify ?

Most of today's big data landscape runs on the JVM and sometimes offers python
or javascript support, but Haskell support is hard to find.

[Sparkle](https://github.com/tweag/sparkle) answered this question for Spark
and Hadoop, allowing programmers to write jobs in Haskell by transforming them
into `.jar` files.

The `jarify` package is the software used in sparkle to generate this `.jar`
file from Haskell source code, and thus letting people write Haskell code and
execute it on JVMs.

# How to use jarify

## Building

Jarify can be built with a simple:
```
stack --nix build jarify
```

## Creating a project to be jarified

Here $PACKAGE will refer to the name of the package you want to jarify

You will need the following lines in your `$PACKAGE.cabal` in order to allow it
to be jarified

* add `jarify` to your `build-depends`
* add `-pie -Wl,-z,origin -Wl,-rpath,$ORIGIN` to your `ld-options`

The `$ORIGIN` is currently a bit hacky and should be soon removed.

The `-pie`, creates your package as a
[Position Independent Executable](https://en.wikipedia.org/wiki/Position-independent_code),
this allows it to be used as a dynamic library by the java stub that will call
it (provoded by `jarify`).

## Jaraify the project

To get the jar from your package:

* build the package using the `--nix` flag
* launch `stack --nix exec -- jarify package $PACKAGE`, this should output your
  jar into `$PACKAGE.jar`

You can then execute your `jar` (if it is an executable) using:
```
stack --nix exec -- java -jar $PACKAGE.jar
```
