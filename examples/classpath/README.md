# inline-java classpath example

This example shows how to write inline code that refers to external
Java libraries. 

To build:

```
$ nix-shell --pure --run 'bazel build //examples/classpath'
```

To run:

```
$ nix-shell --pure --run 'bazel run //examples/classpath'
```
