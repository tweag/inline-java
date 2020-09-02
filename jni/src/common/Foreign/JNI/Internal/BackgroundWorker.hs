{-# LANGUAGE NamedFieldPuns #-}
module Foreign.JNI.Internal.BackgroundWorker
  ( BackgroundWorker
  , create
  , stop
  , submitTask
  ) where

import Control.Concurrent.Async (Async, async, waitCatch)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (Exception, handle, throwIO, uninterruptibleMask_)
import Control.Monad (forever, join, void)
import Data.IORef (IORef, atomicModifyIORef, newIORef)


-- | A background thread that can run tasks asynchronously
data BackgroundWorker = BackgroundWorker
  { nextBatch :: IORef (IO ())
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
    nextBatch <- newIORef (return ())
    workerAsync <- async $ runInInitializedThread $ handleTermination $
      forever $ do
        takeMVar wakeup
        join $ atomicModifyIORef nextBatch $ \tasks -> (return (), tasks)
    return BackgroundWorker
      { nextBatch
      , wakeup
      , workerAsync
      }
  where
    handleTermination = handle $ \StopWorkerException -> return ()

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
      atomicModifyIORef (nextBatch worker) $ \tasks -> (tasks >> task, ())
      void $ tryPutMVar (wakeup worker) ()

-- | Submits a task to the background worker.
--
-- Any exception thrown in the given task will stop the
-- background thread.
--
submitTask :: BackgroundWorker -> IO () -> IO ()
submitTask worker task = do
  atomicModifyIORef (nextBatch worker) $ \tasks -> (task >> tasks, ())
  void $ tryPutMVar (wakeup worker) ()
