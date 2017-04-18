# inline-java classpath example

This example shows how to write inline code that refers to external
Java libraries. The trick is to set the `CLASSPATH` properly both at
compile time and at runtime. We get the information we need directly
from `gradle` for the former, using a `Setup.hs` hook.

## Running the example

Using stack:

```
# Installs build-time dependencies in an easily accessible location.
$ gradle installDist

$ stack exec -- classpath \
    -Djava.class.path=build/install/classpath/lib/commons-collections4-4.1.jar
```
