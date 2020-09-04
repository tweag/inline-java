{-# LANGUAGE NamedFieldPuns #-}
module Foreign.JNI.Internal.BackgroundWorker
  ( BackgroundWorker
  , create
  , setQueueSize
  , stop
  , submitTask
  ) where

import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (Exception, handle, throwIO, uninterruptibleMask_)
import Control.Monad (forever, join, void)
import Control.Monad.Loops (whileM_)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef, writeIORef)


-- | A background thread that can run tasks asynchronously
data BackgroundWorker = BackgroundWorker
  { nextBatch :: IORef (Int, IO ())
  , queueSize :: IORef Int
  , wakeup :: MVar ()
  , workerAsync :: Async ()
  }

-- | Creates a background worker that starts running immediately.
--
-- Takes a function that can initialize the thread before entering
-- the main loop.
--
create :: (IO () -> IO ()) -> IO BackgroundWorker
create runInInitializedThread = do
    wakeup <- newEmptyMVar
    queueSize <- newIORef defaultQueueSize
    nextBatch <- newIORef emptyBatch
    workerAsync <- async $ runInInitializedThread $ handleTermination $
      forever $ do
        takeMVar wakeup
        join $ atomicModifyIORef nextBatch $ \(_, tasks) -> (emptyBatch, tasks)
    return BackgroundWorker
      { nextBatch
      , queueSize
      , wakeup
      , workerAsync
      }
  where
    handleTermination = handle $ \StopWorkerException -> return ()
    emptyBatch = (0, return ())

defaultQueueSize :: Int
defaultQueueSize = 1024

-- | Set the maximum number of pending tasks
setQueueSize :: BackgroundWorker -> Int -> IO ()
setQueueSize = writeIORef . queueSize

data StopWorkerException = StopWorkerException
  deriving Show

instance Exception StopWorkerException

-- | Stops the background worker and waits until it terminates.
stop :: BackgroundWorker -> IO ()
stop worker =
  uninterruptibleMask_ $ do
    submitStopAtEndOfBatch
    void $ waitCatch (workerAsync worker)
  where
    submitStopAtEndOfBatch = do
      let task = throwIO StopWorkerException
      atomicModifyIORef (nextBatch worker) $ \(n, tasks) -> ((n, tasks >> task), ())
      void $ tryPutMVar (wakeup worker) ()

-- | Submits a task to the background worker.
--
-- Any exception thrown in the given task will stop the
-- background thread.
--
-- If the job queue is currently full, block until it isn't
--
submitTask :: BackgroundWorker -> IO () -> IO ()
submitTask worker task = do
  maxSize <- readIORef $ queueSize worker
  flip whileM_ (return ()) $ atomicModifyIORef (nextBatch worker) $ \(n, tasks) ->
    if n >= maxSize
    then ((n, tasks), True)
    else ((n+1, task >> tasks), False)
  void $ tryPutMVar (wakeup worker) ()
