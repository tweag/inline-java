# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## Next release

### Added

* Marshalling lists of refs to a Java array.
* Implement push and pop family of functions for managing local
  references. This makes it possible to perform semi-automatic
  reclaiming of local references, by pushing new frames and
  deallocating whole groups of references at once when popping the
  frames.

### Changed

* Simplify the `Reify` and `Reflect` machinery. These type classes now
  each take a single argument. The `Uncurry` type class, to avoid
  overlapping instances in `sparkle`, has been removed.
* Remove `Reify` and `Reflect` instances for `J ty`.

## [0.2.2] - 2017-05-03

## [0.2.0] - 2017-02-21
