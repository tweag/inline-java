{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Directory.Server where

import Control.Concurrent
import Control.Monad (guard, join)
import qualified Control.Monad.IO.Class.Linear as Linear
import qualified Control.Monad.Catch as Catch
import Control.Monad.Reader (ask, asks)
import Control.Monad.Lift.Linear
import qualified Control.Monad.Linear.Builder as Linear
import Control.Monad.Logger
import Data.Int
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.JNI.Safe
import qualified Foreign.JNI.Types as NonLinear -- Brings J into scope
import qualified Foreign.JNI.Unsafe as Unsafe
import Language.Java.Inline.Safe
import Language.Java.Safe
import Directory.Server.Monad.Classes
import Directory.Server.Http
import Directory.Server.Monad
import Prelude
import Prelude.Linear (Unrestricted(..))
import System.FilePath
import System.Posix.Signals (Handler(Catch), Signal, installHandler, sigINT, sigTERM)


server :: Int -> LServer ()
server port =
    let Linear.Builder{..} = Linear.monadBuilder in do
    Unrestricted mvStop <- liftPreludeIOU newEmptyMVar
    Unrestricted env <- liftU ask
    UnsafeUnrestrictedReference httpServer <-
      startHttpServer (fromIntegral port) (runHandler env)
    lift
      (logInfoN $ Text.pack $
         "Started server on port " ++ show port ++ "."
      )
    liftPreludeIO
      (withSignalHandlers
        (stopServer mvStop httpServer)
        (takeMVar mvStop)
      )
     `finally`
        (lift $ logInfoN $ Text.pack $
           "Server on port " ++ show port ++ " terminated."
        )
    where
      stopServer :: MVar () -> JHttpServer -> IO ()
      stopServer mvStop httpServer =
        runInBoundThread $ Unsafe.runInAttachedThread $ do
          stopHttpServer httpServer
          Unsafe.deleteGlobalRef httpServer
          putMVar mvStop ()

      runHandler :: Environment -> JHttpExchange -> IO ()
      runHandler env exchange = runLServer env (handleRequest exchange)

data Response = Response
  { responseCode :: Int32
  , responseMsg :: Text
  }

handleRequest :: JHttpExchange ->. LServer ()
handleRequest httpExchange =
    let Linear.Builder{..} = Linear.monadBuilder in do
    Unrestricted root <- liftU $ asks envRootDirectory
    (httpExchange, httpExchange2) <- newLocalRef httpExchange
    Unrestricted tpath <-
      [java| $httpExchange2.getRequestURI().getPath() |] >>= reify_
    Unrestricted (Response code msg) <-
      liftU $ listContents root (Text.unpack tpath)
    jmsg <- reflect msg
    [java| {
       $httpExchange.sendResponseHeaders($code, $jmsg.getBytes().length);
       $httpExchange.getResponseBody().write($jmsg.getBytes());
       $httpExchange.getResponseBody().close();
      } |]

-- | @listContents root path@ lists the contents of the directory
-- as @path@. The @path@ must be a descendent of @root@ or an error
-- response is produced.
listContents
  :: forall m.
     (MonadLogger m, MonadFileSystem m) => FilePath -> FilePath -> m Response
listContents root rawPath = do
    mpath <- makePath root rawPath
    logInfoN $ Text.pack $ "Request received: " ++ show mpath
    case mpath of
      Just path -> do
        exists <- doesDirectoryExist path
        case exists of
          True -> do
            xs <- listDirectory path
            return (Response 200 $ Text.pack $ unlines xs)
          False ->
            return (Response 404 $ Text.pack
              "404: Path does not exist or it is not a directory."
            )
      Nothing ->
        return (Response 403 $ Text.pack "403: Illegal path.")
  where
    makePath :: String -> String -> m (Maybe String)
    makePath root reqPath = do
      path <- canonicalizePath (root </> makeRelative "/" reqPath)
      return $ do
        guard (root `isPrefixOf` path)
        return path

-- | @withSignalHandlers cleanup io@ runs @io@ after installing signal
-- handlers for SIGTERM and SIGINT. It ensures that @cleanup@ is run
-- either when @io@ terminates or when SIGTERM or SIGINT are caught.
withSignalHandlers
  :: IO () -> IO () -> IO ()
withSignalHandlers cleanup io = do
    cleanupRef <- newIORef cleanup
    Catch.bracket
      ((,) <$> installHandler sigTERM (handler cleanupRef sigTERM) Nothing
           <*> installHandler sigINT (handler cleanupRef sigINT) Nothing
      )
      (\(hTERM, hINT) -> do
        _ <- installHandler sigTERM hTERM Nothing
        _ <- installHandler sigINT hINT Nothing
        runCleanup cleanupRef
      )
      (const io)
  where
    handler :: IORef (IO ()) -> Signal -> Handler
    handler cleanupRef _ = Catch (runCleanup cleanupRef)

    runCleanup cleanupRef =
      join $ atomicModifyIORef cleanupRef $ \maybeCleanup ->
        (return (), maybeCleanup)
