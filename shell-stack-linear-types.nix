{ pkgs ?  import ./nixpkgs.nix {}
, ghc ? pkgs.haskell.compiler.ghcHEAD
}:

with pkgs;

let
  openjdk = openjdk8;
  jvmlibdir =
    if stdenv.isLinux
    then "${openjdk}/lib/openjdk/jre/lib/amd64/server"
    else "${openjdk}/jre/lib/server";
  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  libHack = if stdenv.isDarwin then {
      DYLD_LIBRARY_PATH = [jvmlibdir];
    } else {
      LD_LIBRARY_PATH = [jvmlibdir];
    };
in
haskell.lib.buildStackProject ({
  name = "inline-java";
  buildInputs = [ git openjdk gradle zlib haskellPackages.happy haskellPackages.alex ];
  ghc = pkgs.haskell.compiler.ghcHEAD;
  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  extraArgs = ["--extra-lib-dirs=${jvmlibdir}"];
  LANG = "en_US.utf8";
} // libHack)
