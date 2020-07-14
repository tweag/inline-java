{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Control.Monad.Linear as Linear
import Data.Int
import Data.Singletons
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
import System.IO.Unsafe (unsafePerformIO)

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
  -> m (UnsafeUnrestrictedReference JHttpServer)
startHttpServer port handler = Linear.do
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

type JNIApplyFun
    = NonLinear.JNIEnv
    -> Ptr NonLinear.JObject
    -> Int64
    -> Ptr (NonLinear.J HttpExchange)
    -> IO ()

foreign import ccall "wrapper" wrapObjectFun
  :: JNIApplyFun -> IO (FunPtr JNIApplyFun)

-- Export only to get a FunPtr.
foreign export ccall "pong_server_http_freeIterator" freeIterator
  :: NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ()
foreign import ccall "&pong_server_http_freeIterator" freeIteratorPtr
  :: FunPtr (NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ())

freeIterator :: NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ()
freeIterator _ _ ptr = do
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ptr
    FunPtrTable {..} <- deRefStablePtr sptr
    freeStablePtr handlePtr
    freeStablePtr sptr

data FunPtrTable = FunPtrTable
  { handlePtr :: StablePtr (JHttpExchange -> IO ())
  }

createHandler :: Linear.MonadIO m => (JHttpExchange -> IO ()) -> m JHttpHandler
createHandler handle = Linear.do
    Unrestricted handlePtr <- liftPreludeIOU (newStablePtr handle)
    let longHandlePtr :: Int64
        longHandlePtr = fromIntegral $ ptrToIntPtr $ castStablePtrToPtr handlePtr
    Unrestricted (longTablePtr :: Int64) <- liftPreludeIOU $
      fromIntegral . ptrToIntPtr . castStablePtrToPtr <$>
        newStablePtr FunPtrTable {..}
    jHandler <-
      [java| new HttpHandler() {
          @Override
          public void handle(HttpExchange e) { hsApply($longHandlePtr, e); }

          private native void hsFinalize(long applyPtr);
          private native void hsApply(long handlePtr, HttpExchange e);

          @Override
          public void finalize() { hsFinalize($longTablePtr); }
        } |]
    (jHandler2, UnsafeUnrestrictedReference klass) <- getObjectClass jHandler
    liftPreludeIO (registerNativesForHttpHandler klass)
    liftPreludeIO (JNI.deleteLocalRef klass)
    Linear.return jHandler2

-- | Register functions for the native methods of the inner class
-- created by 'createHandler'.
--
-- We keep it at the top level to avoid handler-specific state from
-- leaking into the registered functions. These functions are used
-- for all the instances of the inner class.
registerNativesForHttpHandler :: NonLinear.JClass -> IO ()
registerNativesForHttpHandler klass = do
    let {-# NOINLINE applyPtr #-}
        applyPtr :: FunPtr JNIApplyFun
        applyPtr = unsafePerformIO $ wrapObjectFun $ \_jenv _jthis h e -> do
          let hPtr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral h
          handleFun <- deRefStablePtr hPtr
          NonLinear.objectFromPtr e Control.Monad.>>= \x ->
            handleFun (J x) `catch` \(_ :: SomeException) -> Control.Monad.return ()
    JNI.registerNatives klass
      [ JNI.JNINativeMethod
          "hsApply"
          (methodSignature
            [ SomeSing (sing :: Sing ('Prim "long"))
            , SomeSing (sing :: Sing HttpExchange)
            ]
            (sing :: Sing 'Void)
          )
          applyPtr
      , JNI.JNINativeMethod
          "hsFinalize"
          (methodSignature [SomeSing (sing :: Sing ('Prim "long"))] (sing :: Sing 'Void))
          freeIteratorPtr
      ]
