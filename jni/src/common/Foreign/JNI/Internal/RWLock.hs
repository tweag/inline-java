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

import Control.Concurrent.STM
  (TVar, atomically, check, newTVarIO, readTVar, stateTVar, writeTVar)
import Control.Monad (when)
import Data.Choice


-- | A read-write lock
--
-- Concurrent readers are allowed, but only one writer is supported.
--
-- Moreover, a writer trying to acquire a write lock has priority over
-- new readers trying to acquire a read lock.
newtype RWLock =
    RWLock (TVar (Int, RWWantedState))
    -- ^ A count of the held read locks and the wanted state

-- | The wanted state of the RW
data RWWantedState
    = Reading             -- ^ There are no writers
    | Writing (TVar Bool) -- ^ A writer wants to write, grant no more read locks.
                          -- The TVar is to be written when the last read lock
                          -- is released.

-- | Creates a new read-write lock.
new :: IO RWLock
new = RWLock <$> newTVarIO (0, Reading)

-- | Tries to acquire a read lock. If this call returns `Do #read`, no writer
-- will be granted a lock before the read lock is released. The lock can be
-- denied if a writer is writing or waiting to write.
tryAcquireReadLock :: RWLock -> IO (Choice "read")
tryAcquireReadLock (RWLock ref) = atomically $
    readTVar ref >>= \case
      (readers, Reading) -> do
        writeTVar ref (readers + 1, Reading)
        return $ Do #read
      _ -> return $ Don't #read

-- | Releases a read lock.
releaseReadLock :: RWLock -> IO ()
releaseReadLock (RWLock ref) =
    atomically $ do
      (readers, aim) <- readTVar ref
      writeTVar ref (readers - 1, aim)
      case (readers, aim) of
        (1, Writing noReadersRef) -> writeTVar noReadersRef True
        _ -> return ()

-- | Waits until the current read locks are released and grants a write lock.
-- No new reader locks are granted while the writer is waiting for the lock
-- and while it holds the write lock.
acquireWriteLock :: RWLock -> IO ()
acquireWriteLock (RWLock ref) = do
    noReadersRef <- newTVarIO False
    readers <- atomically $ stateTVar ref $ \(readers, _) ->
      (readers, (readers, Writing noReadersRef))
    when (readers > 0) $ atomically $
      readTVar noReadersRef >>= check
