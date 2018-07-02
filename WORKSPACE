workspace(name = "io_tweag_inline_java")

http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-730d42c225f008a13e48bf5e9c13010174324b8c",
  urls = ["https://github.com/tweag/rules_haskell/archive/730d42c225f008a13e48bf5e9c13010174324b8c.tar.gz"]
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-d9df5c834f07c72be1b9e320eb742796557612f8",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/d9df5c834f07c72be1b9e320eb742796557612f8.tar.gz"]
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
  "nixpkgs_git_repository",
  "nixpkgs_package",
)

nixpkgs_git_repository(
  name = "nixpkgs",
  # Keep consistent with ./nixpkgs.nix.
  revision = "1c3b6d509d06af14b1858ffa2d27f3c902f549bd",
)

# These dependencies are built by Nix.
prebuilt_packages = [
  "Cabal",
  "base",
  "bytestring",
  "choice",
  "constraints",
  "containers",
  "deepseq",
  "directory",
  "distributed-closure",
  "exceptions",
  "filemanip",
  "filepath",
  "ghc",
  "hspec",
  "inline-c",
  "language-java",
  "mtl",
  "process",
  "singletons",
  "streaming",
  "template-haskell",
  "temporary",
  "text",
  "vector",
]

nixpkgs_package(
  name = "inline-java-toolchain",
  repository = "@nixpkgs",
  nix_file_content = """
let pkgs = import <nixpkgs> {{}};
in pkgs.buildEnv {{
  name = "inline-java-toolchain";
  paths = with pkgs; [
    (haskell.packages.ghc822.ghcWithPackages (p: with p; [{0}]))
    openjdk
  ];
}}
""".format(" ".join(prebuilt_packages))
)

nixpkgs_package(
  name = "openjdk",
  repository = "@nixpkgs",
  build_file_content = """
filegroup (
  name = "lib",
  srcs = ["lib/openjdk/jre/lib/amd64/server/libjvm.so"],
  visibility = ["//visibility:public"],
)
filegroup (
  name = "bin",
  srcs = ["bin/javac"],
  visibility = ["//visibility:public"],
)
filegroup (
  name = "jni_header",
  srcs = ["include/jni.h"],
  visibility = ["//visibility:public"],
)
filegroup (
  name = "jni_md_header",
  srcs = ["include/jni_md.h"],
  visibility = ["//visibility:public"],
)"""
)

register_toolchains("//:inline-java-toolchain")
