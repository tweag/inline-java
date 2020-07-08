{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Directory.Server.Monad where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.IO.Class.Linear as Linear
import Control.Monad.Lift.Linear
import qualified Control.Monad.Linear as Linear
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Functor.Linear as Data
import Directory.Server.Monad.Classes
import Foreign.JNI.Safe
import Prelude
import Prelude.Linear (Unrestricted(..))
import qualified Prelude.Linear as Linear
import qualified System.Directory as Directory
import qualified Unsafe.Linear as Unsafe


data Environment = Environment
    { envRootDirectory :: FilePath
    }

newtype Server a = Server (LoggingT (ReaderT Environment IO) a)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadLogger
    , Catch.MonadThrow
    , Catch.MonadCatch
    , Catch.MonadMask
    , MonadReader Environment
    )

instance MonadFileSystem Server where
  doesDirectoryExist = liftIO . Directory.doesDirectoryExist
  listDirectory = liftIO . Directory.listDirectory
  canonicalizePath = liftIO . Directory.canonicalizePath

newtype LServer a = LServer { unLServer :: Server a }

runLServer :: Environment -> LServer () -> IO ()
runLServer env (LServer (Server m)) =
  withLocalFrame_ (liftPreludeIO $ runReaderT (runStdoutLoggingT m) env)

instance Data.Functor LServer where
  fmap f = Unsafe.toLinear Linear.$ \(LServer m) ->
    LServer (fmap (\x -> f x) m)

instance Linear.Functor LServer where
  fmap = Unsafe.toLinear2 Linear.$ \(f :: a ->. b) (LServer m) ->
    LServer (fmap (\x -> f x) m)

instance Data.Applicative LServer where
  pure = LServer . pure
  (<*>) = Unsafe.toLinear2 Linear.$ \(LServer f) (LServer a) ->
    LServer (fmap (\(g :: a ->. b) -> (\x -> g x)) f <*> a)

instance Linear.Applicative LServer where
  pure = Unsafe.toLinear (LServer . pure)
  (<*>) = (Data.<*>)

instance Linear.Monad LServer where
  (>>=) = Unsafe.toLinear2 Linear.$ \(LServer m) (f :: a ->. LServer b) ->
    LServer $ m >>= \x -> case f x of
      LServer n -> n
  (>>) = Unsafe.toLinear2 Linear.$ \(LServer m0) (LServer m1) ->
    LServer (m0 >> m1)

instance MonadFinally LServer where
  finally = Unsafe.toLinear2 Linear.$ \(LServer m) (LServer cleanup) ->
    LServer (m `Catch.finally` cleanup)

instance MonadMask LServer where
  mask = Unsafe.toLinear umask
    where
      umask :: (Unmask LServer -> LServer b) -> LServer b
      umask f = LServer $ Catch.mask $ \(u :: forall a. Server a -> Server a) ->
        case f (Unmask (Unsafe.toLinear (LServer . u . unLServer))) of
          LServer n -> n

instance MonadLift LServer where
  type LiftedM LServer = Server
  lift = LServer
  liftU = LServer . fmap Unrestricted
