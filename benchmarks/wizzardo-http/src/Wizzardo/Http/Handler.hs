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
module Wizzardo.Http.Handler
  ( JHandler
  , JRequest
  , JResponse
  , createHandler
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
import Language.Java.Inline.Safe
import Language.Java.Safe
import Prelude
import Prelude.Linear (Unrestricted(..))

imports "com.wizzardo.http.*"
imports "com.wizzardo.http.request.*"
imports "com.wizzardo.http.response.*"

type JHandler = J ('Class "com.wizzardo.http.Handler")
type JResponse = NonLinear.J Response
type JRequest = NonLinear.J Request
type Response = 'Class "com.wizzardo.http.response.Response"
type Request = 'Class "com.wizzardo.http.request.Request"

type JNIHandleFun
    = NonLinear.JNIEnv
    -> Ptr NonLinear.JObject
    -> Ptr (NonLinear.J Request)
    -> Ptr (NonLinear.J Response)
    -> IO ()

foreign import ccall "wrapper" wrapObjectFun
  :: JNIHandleFun -> IO (FunPtr JNIHandleFun)

-- Export only to get a FunPtr.
foreign export ccall "wizzardo_http_handler_freeIterator" freeIterator
  :: NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ()
foreign import ccall "&wizzardo_http_handler_freeIterator" freeIteratorPtr
  :: FunPtr (NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ())

freeIterator :: NonLinear.JNIEnv -> Ptr JObject -> Int64 -> IO ()
freeIterator _ _ ptr = do
    let sptr = castPtrToStablePtr $ intPtrToPtr $ fromIntegral ptr
    handlePtr <- deRefStablePtr sptr
    freeHaskellFunPtr (handlePtr :: FunPtr JNIHandleFun)
    freeStablePtr sptr

createHandler
  :: Linear.MonadIO m
  => (Unrestricted JRequest -> Unrestricted JResponse -> IO ())
  -> m JHandler
createHandler handle =
    let Linear.Builder{..} = Linear.monadBuilder in do
    Unrestricted handlePtr <-
      Linear.liftIOU $ wrapObjectFun $ \_jenv _jthis reqPtr respPtr ->
        Control.Monad.join
          (handle <$> (Unrestricted <$> NonLinear.objectFromPtr reqPtr)
                  <*> (Unrestricted <$> NonLinear.objectFromPtr respPtr)
          )
        `catch` \(_ :: SomeException) -> Control.Monad.return ()
    Unrestricted (longHandlePtr :: Int64) <- Linear.liftIOU $
      fromIntegral . ptrToIntPtr . castStablePtrToPtr <$> newStablePtr handlePtr
    jHandler <-
      [java| new Handler() {
          @Override
          public Response handle(Request req, Response resp) {
            hsHandle(req, resp); return resp;
          }

          private native void hsFinalize(long handlePtr);
          private native void hsHandle(Request req, Response resp);

          @Override
          public void finalize() { hsFinalize($longHandlePtr); }
        } |]
    (jHandler, Unrestricted klass) <- getObjectClass jHandler
    Linear.liftIO  $ JNI.registerNatives klass
      [ JNI.JNINativeMethod
          "hsHandle"
          (methodSignature
            [ SomeSing (sing :: Sing Request)
            , SomeSing (sing :: Sing Response)
            ]
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
