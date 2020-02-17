This is the implementation of an Http server using
[wizzardo-http][wizzardo-http] with Haskell callbacks using
[inline-java][inline-java].

Furtheremore, an experimental interface of inline-java is used, where
[-XLinearTypes][linear-types] ensures references to Java objects are
handled correctly on the Haskell side.

A copy of this code is kept in
https://github.com/TechEmpower/FrameworkBenchmarks/blob/master/frameworks/Haskell/wizzardo-inline

The instruction to run the benchmarks can be found in
https://github.com/TechEmpower/FrameworkBenchmarks

[inline-java]: https://github.com/tweag/inline-java
[linear-types]: https://github.com/tweag/ghc-proposals/blob/linear-types/proposals/0000-linear-types.rst
