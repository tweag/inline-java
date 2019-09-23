{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Control.Monad
import Control.Monad.IO.Class.Linear (MonadIO)
import qualified Control.Monad.Linear.Builder as Linear
import Data.Aeson
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import qualified Data.Text as Text
import Foreign.JNI.Safe (deleteLocalRef, newGlobalRef_, withJVM, withLocalFrame_)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Java.Inline.Safe
import Language.Java.Safe (reflect)
import System.Environment (getArgs, lookupEnv)
import qualified System.IO.Linear as Linear
import Wizzardo.Http.Handler (JHandler, createHandler)
import Prelude (IO, (=<<), Maybe(..), fromInteger, map, ($), (++))
import Prelude.Linear (Unrestricted(..))

imports "com.wizzardo.http.*"
imports "com.wizzardo.http.framework.*"
imports "com.wizzardo.http.request.*"
imports "com.wizzardo.http.response.*"

main :: IO ()
main = getArgs Control.Monad.>>= \args -> do
    let -- We use the classpath provided at build time.
        jvmArgs = case $(TH.lift =<< TH.runIO (lookupEnv "CLASSPATH")) of
          Nothing -> []
          Just cp -> [ fromString ("-Djava.class.path=" ++ cp) ]
        otherJVMArgs =
          [ "-Xmx2G"
          , "-Xms2G"
          -- , "-server"
          , "-XX:+UseNUMA"
          , "-XX:+UseParallelGC"
          , "-XX:+AggressiveOpts"
          ]
    withJVM (jvmArgs ++ otherJVMArgs) $ withLocalFrame_ $
      let Linear.Builder{..} = Linear.monadBuilder in do
      jsonHandler <- createJsonHandler
      deleteLocalRef jsonHandler
      jPlainTextHandler <- createPlainTextHandler
      jargs <- reflect (map Text.pack args)
      [java| {
        WebApplication application = new WebApplication($jargs) {
            @Override
            protected void initHttpPartsCache() {
                ByteTree tree = httpStringsCache.getTree();
                for (Request.Method method : Request.Method.values()) {
                    tree.append(method.name());
                }
                tree.append(HttpConnection.HTTP_1_1);
            }
        };

        application.onSetup(app -> {
          app.getUrlMapping()
             .append
               ( "/json"
               , (request, response) -> response
                 .setBody(JsonResponseHelper.renderJson(
                    new Object() {
                      public String message = "Hello, World!";
                    }
                 ))
                 .appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON)
               )
             .append
               ( "/plaintext"
               , $jPlainTextHandler
               );
        });
        application.start();
       } |]

createJsonHandler :: MonadIO m => m JHandler
createJsonHandler = createHandler $ \_req resp -> Linear.withLinearIO $
    let Linear.Builder{..} = Linear.monadBuilder in do
    jmsg <- reflect (toStrict $ encode $ jsonObject resp)
    [java| { $resp
            .setBody($jmsg)
            .appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
           } |]
    return (Unrestricted ())
  where
    -- Don't inline, so the serialization is not cached.
    {-# NOINLINE jsonObject #-}
    jsonObject _ = object ["message" .= Text.pack "Hello, World!"]

createPlainTextHandler :: MonadIO m => m JHandler
createPlainTextHandler =
    let Linear.Builder{..} = Linear.monadBuilder in do
    jmsg <- reflect (ByteString.Char8.pack "Hello, World!")
    Unrestricted jGlobalMsg <- newGlobalRef_ jmsg
    createHandler $ \_req resp -> Linear.withLinearIO $ do
      let _ujmsg = Unrestricted jGlobalMsg
      [java| { $resp
               .setBody("Hello, World!".getBytes())
               .appendHeader(Header.KV_CONTENT_TYPE_TEXT_PLAIN);
             } |]
      return (Unrestricted ())
