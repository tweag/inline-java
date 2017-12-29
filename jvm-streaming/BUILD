package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_test",
  "haskell_library",
)

haskell_library(
  name = "jvm-streaming",
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs']),
  deps = [
    "//jni",
    "//:inline-java",
    "//jvm",
  ],
  prebuilt_dependencies = [
    "base",
    "distributed-closure",
    "singletons",
    "streaming",
  ],
)

haskell_test(
  name = "spec",
  src_strip_prefix = "tests",
  srcs = glob(['tests/**/*.hs']),
  deps = [
    ":jvm-streaming",
    "//jvm",
    "//:inline-java",
  ],
  prebuilt_dependencies = [
    "base",
    "hspec",
    "streaming",
  ],
  size = "small",
)
