{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
module Foreign.JNI.Internal.RWLock
  ( RWLock
  , new
  , tryAcquireReadLock
  , releaseReadLock
  , acquireWriteLock
  ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (join, when)
import Data.Choice
import Data.IORef (IORef, atomicModifyIORef, newIORef)


-- | A read-write lock
--
-- Concurrent readers are allowed, but only one writer is supported.
newtype RWLock =
    RWLock (IORef (Int, RWWantedState))
    -- ^ A count of the held read locks and the wanted state

-- | The wanted state of the RW
data RWWantedState
    = Reading            -- ^ There are no writers
    | Writing (MVar ())
       -- ^ A writer wants to write, grant no more read locks. The MVar is used
       -- to notify the writer when the currently held read locks are released.

-- | Creates a new read-write lock.
new :: IO RWLock
new = RWLock <$> newIORef (0, Reading)

-- | Tries to acquire a read lock. If this call returns `Do #read`, no writer
-- will be granted a lock before the read lock is released.
tryAcquireReadLock :: RWLock -> IO (Choice "read")
tryAcquireReadLock (RWLock ref) = do
    atomicModifyIORef ref $ \case
      (!readers,  Reading) -> ((readers + 1, Reading),    Do #read)
      st                   -> (                       st, Don't #read)

-- | Releases a read lock.
releaseReadLock :: RWLock -> IO ()
releaseReadLock (RWLock ref) = do
    st <- atomicModifyIORef ref $
            \st@(readers, aim) -> ((readers - 1, aim), st)
    case st of
      -- Notify the writer if I'm the last reader.
      (1, Writing mv) -> putMVar mv ()
      _               -> return ()

-- | Waits until the current read locks are released and grants a write lock.
acquireWriteLock :: RWLock -> IO ()
acquireWriteLock (RWLock ref) = do
    mv <- newEmptyMVar
    join $ atomicModifyIORef ref $ \(readers, _) ->
      ((readers, Writing mv), when (readers > 0) (takeMVar mv))
