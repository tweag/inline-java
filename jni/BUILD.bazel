package(default_visibility = ["//visibility:public"])

load(
  "@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_cc_import",
  "haskell_library",
)

haskell_cc_import(
  name = "openjdk",
  shared_library = "@openjdk//:lib",
  hdrs = ["@openjdk//:jni_header", "@openjdk//:jni_md_header"],
)

haskell_library(
  name = "jni",
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'src/**/*.hsc']),
  deps = [":openjdk"],
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
