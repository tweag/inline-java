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
  { nextBatchRef :: TVar Batch
  , queueSizeRef :: IORef Int
  , workerAsync :: Async ()
  }

-- | Creates a background worker that starts running immediately.
--
-- Takes a function that can initialize the thread before entering
-- the main loop.
--
create :: (IO () -> IO ()) -> IO BackgroundWorker
create runInInitializedThread = do
    queueSizeRef <- newIORef defaultQueueSize
    nextBatch <- newTVarIO emptyBatch
    workerAsync <- async $ runInInitializedThread $ handleTermination $
      forever (runNextBatch nextBatch)
    return BackgroundWorker
      { nextBatch
      , queueSizeRef
      , workerAsync
      }
  where
    handleTermination = handle $ \StopWorkerException -> return ()
    runNextBatch nextBatch =
      join $ atomically $ do
        batch <- readTVar nextBatch
        case getTasks batch of
          Just tasks -> tasks <$ writeTVar nextBatch emptyBatch
          Nothing -> retry

defaultQueueSize :: Int
defaultQueueSize = 1024 * 1024

-- | Set the maximum number of pending tasks
setQueueSize :: BackgroundWorker -> Int -> IO ()
setQueueSize = writeIORef . queueSizeRef

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
      atomically $ modifyTVar' nextBatch (snocTask task)

-- | Submits a task to the background worker.
--
-- Any exception thrown in the given task will stop the
-- background thread.
--
-- If the job queue is currently full, block until it isn't.
--
submitTask :: BackgroundWorker -> IO () -> IO ()
submitTask (BackgroundWorker {nextBatch, queueSizeRef}) task = do
  queueSize <- readIORef queueSizeRef
  atomically $ do
    batch <- readTVar nextBatch
    if getBatchSize batch < queueSize then
      writeTVar nextBatch (consTask task batch)
    else
      retry

-- | A batch of tasks
--
-- Contains the task count and the computation that performs
-- all the tasks.
newtype Batch = Batch (Int, IO ())

emptyBatch :: Batch
emptyBatch = Batch (0, return ())

consTask :: IO () -> Batch -> Batch
consTask task (Batch (n, tasks)) = Batch (n + 1, task >> tasks)

snocTask :: IO () -> Batch -> Batch
snocTask task (Batch (n, tasks)) = Batch (n + 1, tasks >> task)

-- | Yields Nothing on an empty batch.
getTasks :: Batch -> Maybe (IO ())
getTasks (Batch (0, _)) = Nothing
getTasks (Batch (_, tasks)) = Just tasks

getBatchSize :: Batch -> Int
getBatchSize (Batch (n, _)) = n
