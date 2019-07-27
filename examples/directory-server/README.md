# Http directory server

This is a toy server showing usage of the linear interfaces of inline-java.

The server lists the contents of directories in the current working directory.

Run with
```
$ directory-server 4000
```

Inline-java is used to start an http server implemented with
`com.sun.net.httpserver.HttpServer`. A type class `MonadLift`
is used to combine linear and non-linear monadic interfaces.

The linear interfaces are the ones in inline-java. The non-linear
interfaces are MonadLogger, MonadReader and MonadFileSystem.

## Contents

The type class definitions can be found in
[Classes.hs](src/Directory/Server/Monad/Classes.hs).

The instance declarations are in [Monad.hs](src/Directory/Server/Monad.hs).

The bindings to the Java HTTP server are in [Http.hs](src/Directory/Server/Http.hs).

The server handler and startup code is in [Server.hs](src/Directory/Server.hs).

The main program is in [server.hs](src/server.hs).
