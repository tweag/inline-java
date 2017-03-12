# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [0.6.3] - 2017-03-12

### Fixed

* Setting the `CLASSPATH` using Gradle is now fully supported. It was
  previously unusable due to a bug (see #42).

## [0.6.2] - 2017-02-21

* Avoid producing warnings about unused inlinejava_bytecode0 bindings.
* Support type synonyms in variable antiquotation.
* Update lower bounds of jni to 0.3 and jvm to 0.2.

## [0.6.1] - 2016-12-27

* Support passing array objects as arguments / return values.
* More robust mangling of inline class name.

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
