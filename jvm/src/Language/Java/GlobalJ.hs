-- | A type to stand for global references to Java values.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.GlobalJ
  ( GlobalJ
  , newGlobalJ
  , withGlobalJ
  ) where

import Control.Distributed.Closure.TH
import Control.Monad
import GHC.ForeignPtr (newConcForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.JNI
import Foreign.JNI.Types
import Language.Java

-- | Global references to java values.
--
-- When storing references in Haskell data types, this type should be preferred
-- over bare `J ty`. When 'GlobalJ' values are no longer reachable on the
-- Haskell side, 'deleteGlobalRef' is called.
newtype GlobalJ ty = GlobalJ (ForeignPtr (J ty))

-- | Creates a new global reference.
newGlobalJ :: J ty -> IO (GlobalJ ty)
newGlobalJ = newGlobalRef >=> \gj@(J p) ->
    GlobalJ <$> newConcForeignPtr p (deleteGlobalRef gj)

-- | Exposes the reference in a 'GlobalJ' value, ensuring that it is not deleted
-- before the given action completes.
withGlobalJ :: GlobalJ ty -> (J ty -> IO a) -> IO a
withGlobalJ (GlobalJ fp) io = withForeignPtr fp (io . J)

withStatic [d|
  type instance Interp (GlobalJ ty) = ty

  instance (SingI ty, IsReferenceType ty) => Reify (GlobalJ ty) ty where
    reify = newGlobalJ
 |]
