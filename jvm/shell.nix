{ nixpkgs ? import ../nixpkgs.nix
, ghc ? nixpkgs.ghc
}:

with nixpkgs // { ghc = ghc; };

let
  jdk = openjdk7;
  jvmlibdir =
    if stdenv.isLinux
    then "${jdk}/lib/openjdk/jre/lib/amd64/server"
    else "${jdk}/jre/lib/server";
in
haskell.lib.buildStackProject {
  name = "jvm";
  inherit ghc;
  buildInputs = [
    jdk
  ];
  extraArgs = ["--extra-lib-dirs=${jvmlibdir}"];

  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  LD_LIBRARY_PATH = [jvmlibdir];
}
