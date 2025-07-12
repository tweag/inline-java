workspace(name = "io_tweag_inline_java")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


http_archive(
    name = "rules_haskell",
    sha256 = "4cae22bc84f327bf3cb7605021c3663160ff6bc8a0b7b6266062366bcbd19e79",
    strip_prefix = "rules_haskell-1.0",
    urls = ["https://github.com/tweag/rules_haskell/releases/download/v1.0/rules_haskell-1.0.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
rules_haskell_dependencies()



http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "30271f7bd380e4e20e4d7132c324946c4fdbc31ebe0bbb6638a0f61a37e74397",
    strip_prefix = "rules_nixpkgs-0.13.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.13.0/rules_nixpkgs-0.13.0.tar.gz"],
)

http_archive(
    name = "rules_nixpkgs_core",
    sha256 = "30271f7bd380e4e20e4d7132c324946c4fdbc31ebe0bbb6638a0f61a37e74397",
    strip_prefix = "rules_nixpkgs-0.13.0/core",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.13.0/rules_nixpkgs-0.13.0.tar.gz"],
)


[
    http_archive(
        name = "rules_nixpkgs_" + toolchain,
        sha256 = "30271f7bd380e4e20e4d7132c324946c4fdbc31ebe0bbb6638a0f61a37e74397",
        strip_prefix = "rules_nixpkgs-0.13.0/toolchains/" + toolchain,
        urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.13.0/rules_nixpkgs-0.13.0.tar.gz"],
    )
    for toolchain in [
        "go",
    ]
]


load("@rules_nixpkgs_core//:nixpkgs.bzl", "nixpkgs_local_repository")

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

nixpkgs_python_configure(repository = "@nixpkgs")

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        "Cabal",
        "aeson",
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
        "hspec-discover",
        "inline-c",
        "language-java",
        "monad-logger",
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
        "time",
        "vector",
        "unix",
        # dependencies of th-desugar
        "fail",
        "ghc-prim",
        "linear-base",
        "ordered-containers",
        "semigroups",
        "singletons",
        "singletons-base",
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
        # gazelle_cabal dependencies
        "json", # keep
        "path", # keep
        "path-io", # keep
    ],
    extra_deps = { "zlib" : ["@zlib.dev//:zlib"] },
    components_dependencies = {
        "attoparsec": """{"lib:attoparsec": ["lib:attoparsec-internal"]}""",
    },
    components =
        {
            "attoparsec": [
                "lib",
                "lib:attoparsec-internal",
            ],
            "hspec-discover": [
                "lib",
                "exe",
            ],
        },
    local_snapshot = "//:snapshot-9.0.2.yaml",
    # stack = "@stack_ignore_global_hints//:bin/stack" if ghc_version == "9.0.1" else None,
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
    attribute_path = "haskell.compiler.ghc902",
    locale_archive = "@glibc_locales//:locale-archive",
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "9.0.2",
    ghcopts = [
        "-Werror",
        "-Wall",
        "-Wcompat",
        "-Wincomplete-record-updates",
        "-Wredundant-constraints",
    ],
)

nixpkgs_package(
    name = "hspec-discover",
    attribute_path = "haskellPackages.hspec-discover",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zlib.dev",
    repository = "@nixpkgs",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")
filegroup(
    name = "include",
    srcs = glob(["include/*.h"]),
    visibility = ["//visibility:public"],
)
cc_library(
    name = "zlib",
    srcs = ["@nixpkgs_zlib//:lib"],
    hdrs = [":include"],
    linkstatic = 1,
    strip_include_prefix = "include",
    visibility = ["//visibility:public"],
)
""",
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
    srcs = select(
      { "@bazel_tools//src/conditions:darwin": ["lib/server/libjvm.dylib"],
        "@bazel_tools//src/conditions:linux_x86_64": ["lib/openjdk/lib/server/libjvm.so"],
      }),
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

RULES_JVM_EXTERNAL_TAG = "3.3"
RULES_JVM_EXTERNAL_SHA = "d85951a92c0908c80bd8551002d66cb23c3434409c814179c0ff026b53544dab"

http_archive(
    name = "rules_jvm_external",
    strip_prefix = "rules_jvm_external-%s" % RULES_JVM_EXTERNAL_TAG,
    sha256 = RULES_JVM_EXTERNAL_SHA,
    url = "https://github.com/bazelbuild/rules_jvm_external/archive/%s.zip" % RULES_JVM_EXTERNAL_TAG,
)

load("@rules_jvm_external//:defs.bzl", "maven_install")

maven_install(
    artifacts = [
        "org.apache.commons:commons-collections4:4.1",
        "com.wizzardo:http:0.4",
        "com.wizzardo:epoll:0.3.4",
        "com.wizzardo:reactive-pg-client:0.10.2.1",
        "com.wizzardo.tools:tools-collections:0.23",
        "com.wizzardo.tools:tools-interfaces:0.23",
    ],
    repositories = [
        "https://maven.google.com",
        "https://repo1.maven.org/maven2",
    ],
)

# gazelle setup

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "130739704540caa14e77c54810b9f01d6d9ae897d53eedceb40fd6b75efc3c23",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.54.1/rules_go-v0.54.1.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.54.1/rules_go-v0.54.1.zip",
    ],
)

load("@rules_nixpkgs_go//:go.bzl", "nixpkgs_go_configure")

nixpkgs_go_configure(repository = "@nixpkgs")


http_archive(
    name = "bazel_gazelle",
    sha256 = "49b14c691ceec841f445f8642d28336e99457d1db162092fd5082351ea302f1d",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.44.0/bazel-gazelle-v0.44.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.44.0/bazel-gazelle-v0.44.0.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()


# go_repository added due to: https://github.com/bazelbuild/bazel-gazelle/issues/1217
go_repository(
    name = "org_golang_x_xerrors",
    importpath = "golang.org/x/xerrors",
    sum = "h1:go1bK/D/BFZV2I8cIQd1NKEZ+0owSTG1fDTci4IqFcE=",
    version = "v0.0.0-20200804184101-5ec99f83aff1",
)

gazelle_dependencies()


http_archive(
    name = "io_tweag_gazelle_cabal",
    strip_prefix = "gazelle_cabal-ca8f68e250bea33815fb373320f9610582c42083",
    sha256 = "bd2ee67943007723b3425bf2fcbdb6b41b269c7bc03f01e40683d4b5f3984b3b",
    url = "https://github.com/tweag/gazelle_cabal/archive/ca8f68e.zip",
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@io_tweag_gazelle_cabal//:defs.bzl", "gazelle_cabal_dependencies")
gazelle_cabal_dependencies()

