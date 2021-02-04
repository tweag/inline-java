{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
module Directory.Server.Monad.Classes where

import qualified Control.Functor.Linear as Linear


class MonadFileSystem m where
  doesDirectoryExist :: FilePath -> m Bool
  listDirectory :: FilePath -> m [FilePath]
  canonicalizePath :: FilePath -> m FilePath

class Linear.Monad m => MonadFinally m where
  finally :: m a %1-> m () %1-> m a

newtype Unmask m = Unmask (forall a. m a %1-> m a)

class Linear.Monad m => MonadMask m where
  mask :: (Unmask m -> m b) %1-> m b
