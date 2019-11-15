-- | A class to lift non-linear monads into linear monads.
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Lift.Linear where

import qualified Control.Monad as NonLinear
import qualified Control.Monad.Linear as Linear
import Prelude.Linear

-- | Lift a non-linear monad into a linear monad.
--
-- Laws:
--
-- > lift . return = return
--
-- > liftU . return = return . Unrestricted
--
-- > lift (m >>= f) = liftU m >>= \(Unrestricted a) -> lift (f a)
--
-- > liftU (m >>= f) = liftU m >>= \(Unrestricted a) -> liftU (f a)
--
class (Linear.Monad m, NonLinear.Monad (LiftedM m)) => MonadLift m where
  type LiftedM m :: * -> *
  lift :: LiftedM m a -> m a
  liftU :: LiftedM m a -> m (Unrestricted a)
