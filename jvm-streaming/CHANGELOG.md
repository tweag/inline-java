# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## Next release

### Added
### Removed
### Changed

## [0.4.0] - 2020-11-30

### Changed

* Support building with jni-0.8

## [0.3.2] - 2020-07-16

### Changed

* Fixed pointer deallocation for iterator operations (680db81)
* Fix interdependence bug between reflected streams (d7cf7d7)
* Remove leaking class reference (3d16a0d)

## [0.3.1] - 2018-01-01

### Changed

* Enabled StaticPointers extension to produce Static instances.
* Fixed tests so they succeed when the build tree of jvm-batching is
  not available.

## [0.3] - 2018-02-27

### Added

* Benchmarks for batched marshalling of streams.

### Changed

* jvm-streaming: Implement marshalling of streams with jvm-batching.
* Store lookahead element in the Java side when reflecting streams.

## [0.2.1] - 2017-08-31

### Changed

* Update jvm-streaming to use inline-java-0.7.0.

## [0.2] - 2017-02-21
