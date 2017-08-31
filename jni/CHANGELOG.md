# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## Next release

## [0.4.0] - 2017-08-31

### Added

* `arrayUpcast` for upcasting array types.
* `NFData` instances.
* Add support for inline-c-0.6 so `src/Foreign/JNI.c`
  does not cause unnecessary recompilation.
* `instance Show (Sing (a :: JType))`.
* `singToIsReferenceType`, to construct IsReferenceType instances from
  singletons.

### Changed

* `(get|set)ObjectArrayElement` have more general type signatures
  (they work on any `Coercible` value).
* JNI strings are now allocated in pinned memory on the Haskell heap
  where possible, rather than the C heap. This puts pressure on the
  Haskell GC to reclaim memory more often.

### Fixed

* `NUL` character handling. `fromChars` didn't always yield a string
  with a terminating `NUL` character. `toByteStringe` in addition, was
  not discarding the terminating `NUL` character properly.
* `Foreign.JNI` now includes `stdio.h` to help builds in Android.
* Fixed random lockups in the read-write locks used to finalize JNI.

## [0.3.1] - 2017-05-01

### Added

* `ojectFromPtr`/`unsafeObjectToPtr` to convert from/to a pointer type.
* `newJVM`/`destroyJVM`, for use in GHCi, where `withJVM` isn't
  appropriate.

### Changed

* Extend `newArray` to work over any reference type.

## [0.3.0] - 2017-02-21

## [0.2.3] - 2017-01-04

## [0.2.2] - 2016-12-28

## [0.2.0] - 2016-12-25
