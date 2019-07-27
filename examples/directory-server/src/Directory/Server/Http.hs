{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Directory.Server.Http
  ( JHttpHandler
  , JHttpExchange
  , JHttpServer
  , startHttpServer
  , stopHttpServer
  ) where

import Control.Exception (SomeException, catch)
import qualified Control.Monad
import qualified Control.Monad.IO.Class.Linear as Linear
import qualified Control.Monad.Linear.Builder as Linear
import Data.Int
import Data.Singletons
import Data.String (fromString)
import qualified Foreign.JNI as JNI
import Foreign.JNI.Safe
import qualified Foreign.JNI.Types as NonLinear
import Foreign.Ptr
import GHC.Stable
import qualified Language.Java.Inline.Unsafe as Unsafe
import Language.Java.Inline.Safe
import Language.Java.Safe
import Prelude
import Prelude.Linear (Unrestricted(..))

imports "com.sun.net.httpserver.*"
imports "java.net.*"

type JHttpHandler = J ('Class "com.sun.net.httpserver.HttpHandler")
type JHttpExchange = J HttpExchange
type HttpExchange = 'Class "com.sun.net.httpserver.HttpExchange"
type JHttpServer = NonLinear.J ('Class "com.sun.net.httpserver.HttpServer")

-- | @startHttpServer port handler@ starts an http server on @port@.
--
-- @handler@ is invoked whenever an http request is received.
startHttpServer
  :: Linear.MonadIO m
  => Int32
  -> (JHttpExchange -> IO ())
  -> m (Unrestricted JHttpServer)
startHttpServer port handler =
    let Linear.Builder{..} = Linear.monadBuilder in do
    jHandler <- createHandler handler
    httpServer <- [java| {
      HttpServer server = HttpServer.create(new InetSocketAddress($port), 0);
      server.createContext("/", $jHandler);
      server.setExecutor(null); // Use a default executor
      server.start();
      return server;
     }|]
    newGlobalRef_ httpServer

stopHttpServer :: JHttpServer -> IO ()
stopHttpServer httpServer = [Unsafe.java| { $httpServer.stop(0); } |]

type JNIHandleFun
    = NonLinear.JNIEnv
    -> Ptr NonLinear.JObject
    -> Ptr (NonLinear.J HttpExchange)
    -> IO ()

foreign import ccall "wrapper" wrapObjectFun
  :: JNIHandleFun -> IO (FunPtr JNIHandleFun)

-- Export only to get a FunPtr.
foreign export ccall "pong_server_http_freeIterator" freeIterator
  :: NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ()
foreign import ccall "&pong_server_http_freeIterator" freeIteratorPtr
  :: FunPtr (NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ())

freeIterator :: NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ()
freeIterator _ _ ptr = do
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ptr
    handlePtr <- deRefStablePtr sptr
    freeHaskellFunPtr (handlePtr :: FunPtr JNIHandleFun)
    freeStablePtr sptr

createHandler :: Linear.MonadIO m => (JHttpExchange -> IO ()) -> m JHttpHandler
createHandler handle =
    let Linear.Builder{..} = Linear.monadBuilder in do
    Unrestricted handlePtr <-
      Linear.liftIOU $ wrapObjectFun $ \_jenv _jthis e ->
        NonLinear.objectFromPtr e Control.Monad.>>= \x ->
          handle (J x) `catch` \(_ :: SomeException) -> Control.Monad.return ()
    Unrestricted (longHandlePtr :: Int64) <- Linear.liftIOU $
      fromIntegral . ptrToIntPtr . castStablePtrToPtr <$> newStablePtr handlePtr
    jHandler <-
      [java| new HttpHandler() {
          @Override
          public void handle(HttpExchange e) { hsHandle(e); }

          private native void hsFinalize(long handlePtr);
          private native void hsHandle(HttpExchange e);

          @Override
          public void finalize() { hsFinalize($longHandlePtr); }
        } |]
    (jHandler, Unrestricted klass) <- getObjectClass jHandler
    Linear.liftIO  $ JNI.registerNatives klass
      [ JNI.JNINativeMethod
          "hsHandle"
          (methodSignature
            [SomeSing (sing :: Sing HttpExchange)]
            (sing :: Sing 'Void)
          )
          handlePtr
      , JNI.JNINativeMethod
          "hsFinalize"
          (methodSignature [SomeSing (sing :: Sing ('Prim "long"))] (sing :: Sing 'Void))
          freeIteratorPtr
      ]
    Linear.liftIO (JNI.deleteLocalRef klass)
    return jHandler
