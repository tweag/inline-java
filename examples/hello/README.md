# hello-java: A demo of calling the JVM from Haskell

**This directory uses the release version of inline-java (see
the [stack.yaml](./stack.yaml)).**

**Requirements:**
* the [Stack][stack] build tool (version 1.2 or above);
* either, the [Nix][nix] package manager,
* or, OpenJDK installed from your distro.

On OS X, you'll need to install the [Legacy Java SE 6][osx-java-se]
runtime when prompted, even when using Nix.

To build:

```
$ stack --nix build
```

This will get Stack to download a JDK in a local sandbox
(using [Nix][nix]) for good build results reproducibility. **This is
the recommended way to build inline-java.** Alternatively, you'll need
it installed through your OS distribution's package manager (and
you'll need to tell Stack how to find the JVM header files and shared
libraries). Drop the `--nix` argument if you don't whish to use Nix.

To run:

```
$ stack --nix exec -- hello-java
```

[stack]: https://github.com/commercialhaskell/stack
[nix]: http://nixos.org/nix
[osx-java-se]: https://support.apple.com/kb/dl1572?locale=fr_FR
