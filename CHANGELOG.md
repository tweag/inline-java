# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [0.6.0] - 2016-12-13

### Added

* Can set a custom `CLASSPATH` in `Setup.hs` from Gradle build
  configurations to use when compiling inline expressions.
* GHC 8 compatibility
* Support inline expressions that compile to multiple .class files
  (e.g for anonymous classes and anonymous function literals).

### Changed

* The return type of inline expressions no longer needs
  `Reify`/`Reflect` instances.

### Fixed

* Fix antiquotation: in [java| $obj.foo() |], `obj` is now recognized
  as an antiquotation variable.
* Passing multiple options to the JVM using `withJVM`.

## [0.5.0] - 2016-12-13

### Added

* First release with support for inline Java expressions.

### Changed

* Split lower-level and mid-level bindings into separate packages: jni
  and jvm.
