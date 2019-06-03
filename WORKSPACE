workspace(name = "io_tweag_inline_java")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_tweag_rules_haskell",
    sha256 = "9da2afb9f91ae876edb2614a8b177db492703bd31355ce2b86414828e4b32230",
    strip_prefix = "rules_haskell-3564881798919455b27f970f76ae6fd91dfb4fa1",
    urls = ["https://github.com/tweag/rules_haskell/archive/3564881798919455b27f970f76ae6fd91dfb4fa1.tar.gz"],
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-0.5.1",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.5.1.tar.gz"]
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

nixpkgs_package(
    name = "alex",
    attribute_path = "haskellPackages.alex",
    repository = "@nixpkgs",
)

load("@io_tweag_rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
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
        "unix",
    ],
    snapshot = "lts-13.21",
    tools = ["@alex//:bin/alex"],
)

load("@io_tweag_rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

nixpkgs_package(
    name = "glibc_locales",
    attribute_path = "glibcLocales",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "locale-archive",
    srcs = ["lib/locale/locale-archive"],
)
""",
    repository = "@nixpkgs",
)

haskell_register_ghc_nixpkgs(
    attribute_path = "ghc",
    locale_archive = "@glibc_locales//:locale-archive",
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "8.6.5",
)

nixpkgs_package(
    name = "hspec-discover",
    attribute_path = "haskellPackages.hspec-discover",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "openjdk",
    attribute_path = "openjdk11",
    repository = "@nixpkgs",
    build_file_content = """
filegroup(
    name = "bin",
    srcs = ["bin/javac"],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "libjvm",
    srcs = ["lib/openjdk/lib/server/libjvm.so"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "lib",
    srcs = [":libjvm"],
    hdrs = ["include/jni.h", "include/jni_md.h"],
    strip_include_prefix = "include",
    linkstatic = 1,
    visibility = ["//visibility:public"],
)

# XXX Temporary workaround for
# https://github.com/bazelbuild/bazel/issues/8180.
genrule(
    name = "rpath",
    srcs = ["@openjdk//:libjvm"],
    cmd = "libjvm=$(location :libjvm); echo -rpath $$(dirname $$(realpath $$libjvm)) > $@",
    outs = ["openjdk_response_file"],
    visibility = ["//visibility:public"],
)
""",
)
