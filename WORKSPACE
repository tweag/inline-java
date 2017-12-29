workspace(name = "inline_java")

http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-a1f5b6a21e6cc568ef476b8c63f4c1c60ffc5a03",
    urls = ["https://github.com/tweag/rules_haskell/archive/a1f5b6a21e6cc568ef476b8c63f4c1c60ffc5a03.tar.gz"]
)

local_repository(
  name = "io_tweag_rules_haskell",
  path = "/home/shana/programming/rules_haskell",
)

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-53700e429928530f1566cfff3ec00283a123f17f",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/53700e429928530f1566cfff3ec00283a123f17f.tar.gz"],
)

# Required due to rules_haskell use of skylib.
http_archive(
  name = "bazel_skylib",
  strip_prefix = "bazel-skylib-0.2.0",
  urls = ["https://github.com/bazelbuild/bazel-skylib/archive/0.2.0.tar.gz"]
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
  "nixpkgs_git_repository",
  "nixpkgs_package",
)

nixpkgs_git_repository(
  name = "nixpkgs",
  revision = "4026ea9c8afd09b60896b861a04cc5748fdcdfb4",
)

jni_prebuilt = [
  "base",
  "bytestring",
  "choice",
  "containers",
  "constraints",
  "deepseq",
  "inline-c",
  "singletons"
]

jvm_prebuilt = [
  "base",
  "bytestring",
  "constraints",
  "choice",
  "distributed-closure",
  "exceptions",
  "singletons",
  "text",
  "vector",
]

inline_java_prebuilt = [
  "base",
  "bytestring",
  "Cabal",
  "directory",
  "filepath",
  "filemanip",
  "ghc",
  "language-java",
  "mtl",
  "process",
  "text",
  "template-haskell",
  "temporary",
  "hspec",
]

jvm_streaming_prebuilt = [
  "base",
  "distributed-closure",
  "singletons",
  "streaming",
  "hspec",
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
""".format(
    " ".join(depset(jni_prebuilt + jvm_prebuilt + inline_java_prebuilt + jvm_streaming_prebuilt).to_list())
))

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
