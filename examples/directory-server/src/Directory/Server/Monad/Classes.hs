{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Directory.Server.Monad.Classes where

import qualified Control.Monad.Linear as Linear


class MonadFileSystem m where
  doesDirectoryExist :: FilePath -> m Bool
  listDirectory :: FilePath -> m [FilePath]
  canonicalizePath :: FilePath -> m FilePath

class Linear.Monad m => MonadFinally m where
  finally :: m a ->. m () ->. m a

newtype Unmask m = Unmask (forall a. m a ->. m a)

class Linear.Monad m => MonadMask m where
  mask :: (Unmask m -> m b) ->. m b
