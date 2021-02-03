# hello-java: A demo of calling the JVM from Haskell

To build:

```
$ nix-shell --pure --run 'bazel build //examples/hello:hello-java'
```

To run:

```
$ nix-shell --pure --run 'bazel run //examples/hello:hello-java'
```
