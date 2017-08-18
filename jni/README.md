# jni: Complete JNI raw bindings

[![jni on Stackage LTS](http://stackage.org/package/jni/badge/lts)](http://stackage.org/lts/package/jni)
[![jni on Stackage Nightly](http://stackage.org/package/jni/badge/nightly)](http://stackage.org/nightly/package/jni)

This package includes Haskell bindings to the
[Java Native Interface (JNI)][jni]. This package offers mostly
unopinionated low-level bindings, which closely maps to the C API. For
higher-level more opinionated bindings see the [jvm][haskell-jvm] and
[inline-java][inline-java] packages.

[jni]: https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html
[haskell-jvm]: https://github.com/tweag/inline-java/tree/master/jvm
[inline-java]: https://github.com/tweag/inline-java/

## Tracing

JNI provides tracing messages via GHC's eventlog. These messages can
be later analyzed with `jni-analyze`.

To produce the messages run

```
$ HASKELL_JNI_TRACE=localrefs your_program +RTS -l-au
```

This should generate a file `your_program.eventlog`. Next, this file can
be analyzed with

```
$ jni-analyze your_program.eventlog
```

Which produces a file `your_program.stacksamples`. This file lists all the
stack frames traced by the program. The frames with the highest counts of
local references are listed first. For instance

```
$ cat your_program.stacksamples
src/Control/Distributed/Spark.hs:67:7:tracePushLocalFrame;trace.hs:36:5:pushLocalFrame;trace.hs:37:5:f::trace.hs:42:5:pushLocalFrame 31
trace.hs:26:17:map;src/Control/Distributed/Spark/Closure.hs:120:13:reflect::src/Control/Distributed/Spark/Iterator.hs:579:1:reflectIteratorWithBatching'::src/Control/Distributed/Spark/Iterator.hs:488:21:tracePushLocalFrame 31
src/Control/Distributed/Spark.hs:67:7:tracePushLocalFrame 6
trace.hs:26:17:map 4
src/Foreign/JNI.cpphs:888:12:asDaemonThread::src/Foreign/JNI.cpphs:485:15:attachCurrentThreadAsDaemon::src/Foreign/JNI.cpphs:460:5:tracePushLocalFrame 3
src/Control/Distributed/Spark/Closure.hs:120:13:reflect::src/Control/Distributed/Spark/Iterator.hs:579:1:reflectIteratorWithBatching'::src/Control/Distributed/Spark/Iterator.hs:488:21:tracePushLocalFrame 2
src/Control/Distributed/Spark.hs:67:7:tracePushLocalFrame;trace.hs:36:5:pushLocalFrame 0
```

There is a high count of local references on the first listed frame.
The local references can be found by inspecting the code. But we can
also ask the analyzer to report the source locations where the traces
are created. For this to work, the program needs to be rerun generating
more tracing messages.

```
$ HASKELL_JNI_TRACE=localrefs.callstacks your_program +RTS -l-au
```

Using `localrefs.callstacks` produces a bigger eventlog, so it is not
the recommended choice for the first analysis pass.
Next we can tell `jni-analyze` to report the frames whose counts exceed
of 20 local references. Now the source locations where local references
are created is reported in standard error.

```
$ jni-analyze your_program.eventlog
...
Too many references (--max-local-refs 30) at the stack frame created at
src/Control/Distributed/Spark.hs:67:7:tracePushLocalFrame;trace.hs:36:5:pushLocalFrame;trace.hs:37:5:f::trace.hs:42:5:pushLocalFrame
local references:
1: trace.hs:37:5:f::trace.hs:43:23:newLocalRef::src/Foreign/JNI.cpphs:926:37:objectFromPtr
2: trace.hs:37:5:f::trace.hs:43:23:newLocalRef::src/Foreign/JNI.cpphs:926:37:objectFromPtr
3: trace.hs:37:5:f::trace.hs:43:23:newLocalRef::src/Foreign/JNI.cpphs:926:37:objectFromPtr
4: trace.hs:37:5:f::trace.hs:43:23:newLocalRef::src/Foreign/JNI.cpphs:926:37:objectFromPtr
5: trace.hs:37:5:f::trace.hs:43:23:newLocalRef::src/Foreign/JNI.cpphs:926:37:objectFromPtr
...
```

The program `jni-analyze` reports the frames with counts higher than a
default threshold. If you want to see frame with higher or
lower counts reported the threshold can be set with `--max-local-refs N`
on the command line. Other options are available too. See
`jni-analyze --help` for details.
