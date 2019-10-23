{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Control.Monad
import Control.Monad.IO.Class.Linear (MonadIO, liftIO)
import qualified Control.Monad.Linear.Builder as Linear
import Data.Aeson
import qualified Control.Monad.Builder as Prelude
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import Data.Int
import qualified Data.Text as Text
import qualified DirectBuffer
import Foreign.JNI.Safe (newGlobalRef_, withJVM, withLocalFrame_)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Java.Inline.Safe
import Language.Java.Safe (reflect)
import System.Clock
import System.Environment (getArgs, lookupEnv)
import qualified System.IO.Linear as Linear
import System.IO.Unsafe (unsafePerformIO)
import Wizzardo.Http.Handler (JHandler, createHandler)
import Prelude (IO, (=<<), Maybe(..), fromInteger, fromIntegral, map, print, ($), (++))
import Prelude.Linear (Unrestricted(..))

imports "com.wizzardo.http.*"
imports "com.wizzardo.http.framework.*"
imports "com.wizzardo.http.request.*"

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
      jPlainTextHandler <- createPlainTextHandler
      jsonHandler <- createJsonHandler
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
             .append("/plaintext", $jPlainTextHandler);
        });
        application.start();
       } |]

bufferPool :: DirectBuffer.DirectBufferPool
{- NOINLINE bufferPool -}
bufferPool = unsafePerformIO DirectBuffer.newDirectBufferPool

createJsonHandler :: MonadIO m => m JHandler
createJsonHandler = createHandler $ \_req resp ->
    DirectBuffer.withDirectBuffer bufferPool $ \jbuffer buffer ->
    Linear.withLinearIO $
    let Linear.Builder{..} = Linear.monadBuilder in do
    let bs = toStrict $ encode $ jsonObject resp
        len = fromIntegral (ByteString.length bs) :: Int32
        ubuffer = Unrestricted jbuffer
    liftIO $
      let Prelude.Builder{..} = Prelude.monadBuilder in do
      t0 <- getTime Monotonic
      Control.Monad.forM_ [1..100] $ \i ->
        DirectBuffer.writeBuffer buffer (toStrict $ encode $ jsonObject (i :: Int))
      t1 <- getTime Monotonic
      print (diffTimeSpec t1 t0)
    [java| {
       byte[] jmsg = new byte[$len];
       $ubuffer.rewind();
       $ubuffer.get(jmsg);
       $resp.setBody(jmsg)
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
      let ujmsg = Unrestricted jGlobalMsg
      [java| { $resp
               .setBody($ujmsg)
               .appendHeader(Header.KV_CONTENT_TYPE_TEXT_PLAIN);
             } |]
      return (Unrestricted ())
