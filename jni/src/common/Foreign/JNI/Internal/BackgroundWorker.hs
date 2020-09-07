{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Foreign.JNI.Internal.BackgroundWorker
  ( BackgroundWorker
  , create
  , setQueueSize
  , stop
  , submitTask
  ) where

import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Concurrent.STM
  (TVar, atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (Exception, handle, throwIO, uninterruptibleMask_)
import Control.Monad (forever, join, void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)


-- | A background thread that can run tasks asynchronously
data BackgroundWorker = BackgroundWorker
  { nextBatch :: TVar (Int, IO ())
  , queueSize :: IORef Int
  , workerAsync :: Async ()
  }

-- | Creates a background worker that starts running immediately.
--
-- Takes a function that can initialize the thread before entering
-- the main loop.
--
create :: (IO () -> IO ()) -> IO BackgroundWorker
create runInInitializedThread = do
    queueSize <- newIORef defaultQueueSize
    nextBatch <- newTVarIO emptyBatch
    workerAsync <- async $ runInInitializedThread $ handleTermination $
      forever (runNextBatch nextBatch)
    return BackgroundWorker
      { nextBatch
      , queueSize
      , workerAsync
      }
  where
    handleTermination = handle $ \StopWorkerException -> return ()
    emptyBatch = (0, return ())
    runNextBatch nextBatch =
      join $ atomically $ readTVar nextBatch >>= \case
        (0, _) -> retry
        (_, tasks) -> tasks <$ writeTVar nextBatch emptyBatch

defaultQueueSize :: Int
defaultQueueSize = 1024 * 1024

-- | Set the maximum number of pending tasks
setQueueSize :: BackgroundWorker -> Int -> IO ()
setQueueSize = writeIORef . queueSize

data StopWorkerException = StopWorkerException
  deriving Show

instance Exception StopWorkerException

-- | Stops the background worker and waits until it terminates.
stop :: BackgroundWorker -> IO ()
stop (BackgroundWorker {nextBatch, workerAsync}) =
  uninterruptibleMask_ $ do
    submitStopAtEndOfBatch
    void $ waitCatch workerAsync
  where
    submitStopAtEndOfBatch = do
      let task = throwIO StopWorkerException
      atomically $ modifyTVar' nextBatch $ \(n, tasks) -> (n + 1, tasks >> task)

-- | Submits a task to the background worker.
--
-- Any exception thrown in the given task will stop the
-- background thread.
--
-- If the job queue is currently full, block until it isn't.
--
submitTask :: BackgroundWorker -> IO () -> IO ()
submitTask (BackgroundWorker {nextBatch, queueSize}) task = do
  maxSize <- readIORef queueSize
  atomically $ do
    (size, tasks) <- readTVar nextBatch
    if size < maxSize then
      writeTVar nextBatch (size + 1, task >> tasks)
    else
      retry
