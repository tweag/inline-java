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
  (TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
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
    = Reading            -- ^ There are no writers
    | Writing            -- ^ A writer wants to write, grant no more read locks.

-- | Creates a new read-write lock.
new :: IO RWLock
new = RWLock <$> newTVarIO (0, Reading)

-- | Tries to acquire a read lock. If this call returns `Do #read`, no writer
-- will be granted a lock before the read lock is released. The lock can be
-- denied if a writer is writing or waiting to write.
tryAcquireReadLock :: RWLock -> IO (Choice "read")
tryAcquireReadLock (RWLock ref) = atomically $ do
    (readers, aim) <- readTVar ref
    case aim of
      Reading -> do
        writeTVar ref (readers + 1, Reading)
        return $ Do #read
      _ -> return $ Don't #read

-- | Releases a read lock.
releaseReadLock :: RWLock -> IO ()
releaseReadLock (RWLock ref) =
    atomically $ modifyTVar' ref $ \(readers, aim) -> (readers - 1, aim)

-- | Waits until the current read locks are released and grants a write lock.
-- No new reader locks are granted while the writer is waiting for the lock
-- and while it holds the write lock.
acquireWriteLock :: RWLock -> IO ()
acquireWriteLock (RWLock ref) = do
    atomically $ modifyTVar' ref $ \(readers, _) -> (readers, Writing)
    atomically $ do
      (readers, _) <- readTVar ref
      case readers of
        0 -> return ()
        _ -> retry
