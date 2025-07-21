{
  pkgs ? import ./nixpkgs.nix { },
  ghcAttr ? "ghc964",
}:

with pkgs;

let
  openjdk = openjdk11;
  jvmlibdir =
    if stdenv.isLinux
    then "${openjdk}/lib/openjdk/lib/server"
    else "${openjdk}/jre/lib/server";
  # XXX Workaround https://gitlab.haskell.org/ghc/ghc/-/issues/11042
  libHack = if stdenv.isDarwin then {
      DYLD_LIBRARY_PATH = [jvmlibdir];
    } else {
      LD_LIBRARY_PATH = [jvmlibdir];
    };
in
haskell.lib.buildStackProject ({
  name = "inline-java";
  ghc = pkgs.haskell.compiler.${ghcAttr};
  buildInputs = [
    openjdk
    git
    # Needed to get correct locale for tests with encoding
    glibcLocales
    # to avoid CA certificate failures on macOS CI
    cacert
  ];
  # XXX Workaround https://gitlab.haskell.org/ghc/ghc/-/issues/11042
  extraArgs = ["--extra-lib-dirs=${jvmlibdir}"];
  LANG = "en_US.UTF-8";
} // libHack)
