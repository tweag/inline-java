package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
  "haskell_test",
)

java_library(
  name = "jar",
  srcs = glob(["src/main/java/**/*.java"]),
)

haskell_library(
  name = "jvm-batching",
  src_strip_prefix = "src/main/haskell",
  srcs = glob(['src/main/haskell/**/*.hs']),
  deps = [
    ":jar",
    "//jni",
    "//jvm",
    "//:inline-java",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "distributed-closure",
    "exceptions",
    "singletons",
    "text",
    "vector",
  ],
)

haskell_test(
  name = "spec",
  src_strip_prefix = "src/test/haskell",
  srcs = glob(["src/test/haskell/**/*.hs"]),
  deps = [
    "//:inline-java",
    "//jvm",
    "//jvm-batching",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "hspec",
    "streaming",
    "text",
    "vector",
  ],
  size = "small",
)
