{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.haskell.compiler.ghc802 }:

with pkgs;

let
  openjdk = openjdk8;
  jvmlibdir =
    if stdenv.isLinux
    then "${openjdk}/lib/openjdk/jre/lib/amd64/server"
    else "${openjdk}/jre/lib/server";
in
haskell.lib.buildStackProject {
  name = "inline-java";
  buildInputs =
    [ git
      gradle
      openjdk
      which
      zlib
      zip
    ];
  buildInputs = [ git openjdk ];
  ghc = haskell.compiler.ghc802;
  extraArgs = ["--extra-lib-dirs=${jvmlibdir}"];
  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  LD_LIBRARY_PATH = [jvmlibdir];
  LANG = "en_US.utf8";
}
