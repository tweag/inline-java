package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_library",
)

haskell_library(
  name = "jvm",
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs']),
  deps = [
    "//jni",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "constraints",
    "choice",
    "distributed-closure",
    "exceptions",
    "singletons",
    "text",
    "vector",
  ],
)
