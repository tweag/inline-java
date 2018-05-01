package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_test",
  "haskell_library",
  "haskell_toolchain",
)

cc_library(
  name = "bctable",
  srcs = ["cbits/bctable.c"],
  hdrs = ["cbits/bctable.h"],
)

haskell_toolchain(
  name = "inline-java-toolchain",
  version = "8.2.2",
  tools = "@inline-java-toolchain//:bin",
)

haskell_library(
  name = "inline-java",
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'src/**/*.hsc']),
  deps = [
    "//jni",
    "//jvm",
    ":bctable",
  ],
  prebuilt_dependencies = [
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
    "temporary"
  ],
)

haskell_test(
  name = "spec",
  src_strip_prefix = "tests",
  srcs = glob(["tests/**/*.hs"]),
  deps = [
    "//jni",
    "//jvm",
    ":inline-java"
  ],
  prebuilt_dependencies = [
    "base",
    "hspec",
    "text",
  ],
  size = "small",
)
