workspace(name = "io_tweag_inline_java")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_haskell",
    sha256 = "0f9f404d23b30ba77b43d2d2ea392413859e009faf65e3d03d343c275fc90f28",
    strip_prefix = "rules_haskell-5834cbb0fb89da28016cf76fe31370a199c80a69",
    urls = ["https://github.com/tweag/rules_haskell/archive/5834cbb0fb89da28016cf76fe31370a199c80a69.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

nixpkgs_python_configure(repository = "@nixpkgs")

nixpkgs_package(
    name = "alex",
    attribute_path = "haskellPackages.alex",
    repository = "@nixpkgs",
)

http_archive(
    name = "th-desugar",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "th-desugar",
    version = packages["th-desugar"].version,
    srcs = glob(["**"]),
    deps = packages["th-desugar"].deps,
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "14e29e035b96d7c35bb1503426736e610465f75939bd89df1386f2a0c26ce82a",
    strip_prefix = "th-desugar-1.11",
    urls = ["http://hackage.haskell.org/package/th-desugar-1.11/th-desugar-1.11.tar.gz"],
)

http_archive(
    name = "singletons",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "singletons",
    version = packages["singletons"].version,
    srcs = glob(["**"]),
    deps = packages["singletons"].deps,
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "e12bd6e695eaf444eb6b1fd07372818aaff8703aa71265f677f3af3cb412e22b",
    strip_prefix = "singletons-2.7",
    urls = ["http://hackage.haskell.org/package/singletons-2.7/singletons-2.7.tar.gz"],
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        "Cabal",
        "async",
        "base",
        "bytestring",
        "choice",
        "constraints",
        "containers",
        "criterion",
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
        "QuickCheck",
        "quickcheck-text",
        "quickcheck-unicode",
        "split",
        "streaming",
        "template-haskell",
        "temporary",
        "text",
        "vector",
        "unix",
        # dependencies of th-desugar
        "fail",
        "ghc-prim",
        "ordered-containers",
        "semigroups",
        "stm",
        "syb",
        "th-abstraction",
        "th-lift",
        "th-orphans",
        "transformers-compat",
        # dependencies of singletons
        "ghc-boot-th",
        "pretty",
        "transformers",
    ],
    vendored_packages =
      { "singletons": "@singletons//:singletons"
      , "th-desugar": "@th-desugar//:th-desugar"
      },
    snapshot = "lts-16.5",
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

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
    attribute_path = "haskell.compiler.ghc8101",
    locale_archive = "@glibc_locales//:locale-archive",
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "8.10.1",
    compiler_flags = [
        "-Werror",
        "-Wall",
        "-Wcompat",
        "-Wincomplete-record-updates",
        "-Wredundant-constraints",
    ],
)

nixpkgs_package(
    name = "sed",
    attribute_path = "gnused",
    repository = "@nixpkgs",
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
