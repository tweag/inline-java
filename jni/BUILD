package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_import",
  "haskell_library",
)

haskell_import(
  name = "openjdk",
  shared_library = "@openjdk//:lib"
)

haskell_library(
  name = "jni",
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'src/**/*.hsc']),
  deps = [
    ":openjdk"
  ],
  # https://stackoverflow.com/questions/46160790/bazel-for-jni-jni-h-file-not-found
  external_deps = [
    "@openjdk//:jni_header",
    "@openjdk//:jni_md_header",
  ],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    "choice",
    "containers",
    "constraints",
    "deepseq",
    "inline-c",
    "singletons",
  ],
)
