{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.IO.Class.Linear (MonadIO)
import qualified Control.Functor.Linear as Linear
import Data.Aeson
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (toStrict)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.String (fromString)
import qualified Data.Text as Text
import DbHandler (createDbHandler)
import Foreign.JNI.Safe (newGlobalRef_, withJVM, withLocalFrame_)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Java.Inline.Safe
import Language.Java.Safe (UnsafeUnrestrictedReference(..), reflect)
import System.Directory (canonicalizePath)
import System.Environment (getArgs, lookupEnv)
import qualified System.IO.Linear as Linear
import Wizzardo.Http.Handler (JHandler, createHandler)
import Prelude (IO, Maybe(..), (<>), map, mapM, ($))
import Prelude.Linear (Ur(..))

imports "com.wizzardo.http.*"
imports "com.wizzardo.http.framework.*"
imports "com.wizzardo.http.request.*"

main :: IO ()
main = do
    let -- We use the classpath provided at build time.
        cpArg = case $(do
            cps <- TH.runIO $ do
              Just cp <- lookupEnv "CLASSPATH"
              -- We canonicalize the paths because the jars
              -- are located under symbolic links that do not
              -- survive the compilation.
              mapM canonicalizePath $ splitOn ":" cp
            TH.lift (intercalate ":" cps)
          ) of
          cp -> "-Djava.class.path=" <> fromString cp
    args <- getArgs
    let -- We use the classpath provided at build time.
        otherJVMArgs =
          [ "-Xmx2G"
          , "-Xms2G"
          -- , "-server"
          , "-XX:+UseNUMA"
          , "-XX:+UseParallelGC"
          , "-XX:+AggressiveOpts"
          ]
    withJVM (cpArg : otherJVMArgs) $ withLocalFrame_ $ Linear.do
      jsonHandler <- createJsonHandler
      jPlainTextHandler <- createPlainTextHandler
      jDbHandler <- createDbHandler
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
             .append("/json", $jsonHandler)
             .append("/plaintext", $jPlainTextHandler)
             .append("/db", $jDbHandler);
        });
        application.start();
       } |]

createJsonHandler :: MonadIO m => m JHandler
createJsonHandler = createHandler $ \_req resp -> Linear.withLinearIO $ Linear.do
    jmsg <- reflect (toStrict $ encode $ jsonObject resp)
    [java| { $resp
            .setBody($jmsg)
            .appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
           } |]
    Linear.return (Ur ())
  where
    -- Don't inline, so the serialization is not cached.
    {-# NOINLINE jsonObject #-}
    jsonObject _ = object ["message" .= Text.pack "Hello, World!"]

createPlainTextHandler :: MonadIO m => m JHandler
createPlainTextHandler = Linear.do
    jmsg <- reflect (ByteString.Char8.pack "Hello, World!")
    UnsafeUnrestrictedReference jGlobalMsg <- newGlobalRef_ jmsg
    createHandler $ \_req resp -> Linear.withLinearIO $ Linear.do
      let ujmsg = UnsafeUnrestrictedReference jGlobalMsg
      [java| { $resp
               .setBody($ujmsg)
               .appendHeader(Header.KV_CONTENT_TYPE_TEXT_PLAIN);
             } |]
      Linear.return (Ur ())
