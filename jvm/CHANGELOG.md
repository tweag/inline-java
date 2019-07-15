# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## Next release

### Added

* Added support for ghc-8.9.

### Removed

* Removed support for ghc < 8.2.1

## [0.4.2] - 2018-02-27

### Added

* Bazel build files.
* `Reflect/Reify` instances for `IOVector (Word16|Int16|Int64|Float)`.

## [0.4.1] - 2017-12-27

### Added

* `Reify`/`Reflect` instances for `Vector/IOVector Double`.
* `withLocalRef`: a helper to delete local references.

### Changed

* `Reify`/`Reflect` were given an additional invariant.
  The result and argument of `reflect` and `reify` shouldn't share any
  direct JVM object references. This fixes memory errors in the
  instances of `[J ty]`.
  [#102](https://github.com/tweag/inline-java/pull/102)

## [0.4.0.1] - 2017-12-06

### Changed

* The `Coercible` type class now also takes a single parameter, like
  `Reify` and `Reflect`. This should make instance declarations less
  verbose. [#97](https://github.com/tweag/inline-java/pull/97)
* It is now possible to `-XGeneralizedNewtypeDeriving` all 3 type
  classes (`Coercible`, `Reify`, `Reflect`) for the common use case of
  newtype-wrapped JVM object
  references. [#97](https://github.com/tweag/inline-java/pull/97)
* The `Reify`/`Reflect` instances for `()` is now mapped to
  a *serializable* small JVM object. This is a more useful instance
  for sparkle users.

## [0.3.0] - 2017-08-31

### Added

* Marshalling lists of refs to a Java array.
* Implement push and pop family of functions for managing local
  references. This makes it possible to perform semi-automatic
  reclaiming of local references, by pushing new frames and
  deallocating whole groups of references at once when popping the
  frames.
* Benchmarks to quantify the cost of references.
* `newArray`, a polymorphic way to create new arrays on top of the raw
  newXXXArray JNI functions.
* `getStaticField`, to get the values of fields using any of the
  `getStaticXXXField` JNI functions.
* `CChar` instance for `Reify`.
* Expose `jobject` to get Java references from Haskell values with a
  Coercible instance with a reference type.

### Changed

* Simplify the `Reify` and `Reflect` machinery. These type classes now
  each take a single argument. The `Uncurry` type class, to avoid
  overlapping instances in `sparkle`, has been removed.
* `callStatic` takes a `JNI.String` instead of a singleton.
* `classOf` returns a JNI.String instead of a singleton.

### Removed

* The Uncurry type family.
* `Reify` and `Reflect` instances for `J ty`.
* Second parameter of Reflect/Reify.
* TH convenience function to produce Coercible and Reify instances for
  newtype wrappers of Java references. This is less useful now that
  we don't provide `Reify` and `Reflect` instances for `J ty`.

### Fixed

* Tests in conditional compilation for ghc-8.0.1.
* Use global refs in finalizers associated to IOVectors.

## [0.2.2] - 2017-05-03

### Added

* Added TH convenience function to produce Coercible and Reify
  instances for newtype wrappers of Java references.
* Made Choice Coercible to a Java boolean.

## [0.2.0] - 2017-02-21
