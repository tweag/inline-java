{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.IO.Linear.Extras where

import qualified Control.Monad.IO.Class as NonLinear
import Control.Monad.Lift.Linear
import Prelude.Linear
import qualified System.IO.Linear as Linear

instance MonadLift Linear.IO where
  type LiftedM Linear.IO = IO
  lift io = Linear.fromSystemIO io
  liftU = Linear.fromSystemIOU

type MonadIO m = (MonadLift m, NonLinear.MonadIO (LiftedM m))

liftIO :: MonadIO m => IO a -> m a
liftIO io = lift (NonLinear.liftIO io)

liftIOU :: MonadIO m => IO a -> m (Unrestricted a)
liftIOU io = liftU (NonLinear.liftIO io)
