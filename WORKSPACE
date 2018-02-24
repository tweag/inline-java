workspace(name = "io_tweag_inline_java")

http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-23d8b0f3604373d75c7b989273b3901091e63ae8",
  urls = ["https://github.com/tweag/rules_haskell/archive/23d8b0f3604373d75c7b989273b3901091e63ae8.tar.gz"]
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
  name = "io_tweag_rules_nixpkgs",
  strip_prefix = "rules_nixpkgs-0.1.1",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.1.1.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
  "nixpkgs_git_repository",
  "nixpkgs_package",
)

nixpkgs_git_repository(
  name = "nixpkgs",
  # Keep consistent with ./nixpkgs.nix.
  revision = "4026ea9c8afd09b60896b861a04cc5748fdcdfb4",
)

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
""".format(" ".join(prebuilt_packages)))

nixpkgs_package(
  name = "openjdk",
  repository = "@nixpkgs",
  build_file_content = """
filegroup (
  name = "lib",
  srcs = ["nix/lib/openjdk/jre/lib/amd64/server/libjvm.so"],
  visibility = ["//visibility:public"],
)
filegroup (
  name = "jni_header",
  srcs = ["nix/include/jni.h"],
  visibility = ["//visibility:public"],
)
filegroup (
  name = "jni_md_header",
  srcs = ["nix/include/jni_md.h"],
  visibility = ["//visibility:public"],
)"""
)

register_toolchains("//:inline-java-toolchain")
