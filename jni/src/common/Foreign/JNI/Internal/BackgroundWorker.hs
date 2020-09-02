{-# LANGUAGE NamedFieldPuns #-}
module Foreign.JNI.Internal.BackgroundWorker
  ( BackgroundWorker
  , create
  , stop
  , submitTask
  ) where

import Control.Concurrent.Async (Async, asyncBound, waitCatch)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (evaluate, uninterruptibleMask_)
import Control.Monad (forever, join, void)
import Data.IORef (IORef, atomicModifyIORef, newIORef)


-- | A background thread that we can use to run JNI calls asynchronously
-- (like deleting global references from finalizers)
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
  workerAsync <- asyncBound $ runInInitializedThread $ forever $ do
    takeMVar wakeup
    join $ atomicModifyIORef nextBatch $ \tasks -> (return (), tasks)
  return BackgroundWorker
    { nextBatch
    , wakeup
    , workerAsync
    }

-- | Stops the background worker and waits until it terminates.
stop :: BackgroundWorker -> IO ()
stop worker =
  uninterruptibleMask_ $ do
    submitTask worker $ evaluate $ error "Terminating background thread"
    void $ waitCatch (workerAsync worker)

-- | Submits a task to the background worker.
--
-- Any exception thrown in the given task will stop the
-- background thread.
--
submitTask :: BackgroundWorker -> IO () -> IO ()
submitTask worker task = do
  atomicModifyIORef (nextBatch worker) $ \tasks -> (task >> tasks, ())
  void $ tryPutMVar (wakeup worker) ()
