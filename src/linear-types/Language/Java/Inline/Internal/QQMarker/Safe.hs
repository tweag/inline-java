{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Java.Inline.Internal.QQMarker.Safe where

import Data.Proxy
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (Nat, Symbol)
import qualified Language.Java.Safe as Safe

-- | A function to indicate to the plugin the occurrence
-- of java quasiquotations
qqMarker
  :: forall
     k
     (args_tys :: k)     -- JType's of arguments
     tyres               -- JType of result
     (input :: Symbol)   -- input string of the quasiquoter
     (mname :: Symbol)   -- name of the method to generate
     (antiqs :: Symbol)  -- antiquoted variables as a comma-separated list
     (line :: Nat)       -- line number of the quasiquotation
     args_tuple          -- uncoerced argument types
     b                   -- uncoerced result type
     m.
     ( tyres ~ Safe.Ty b
     , Coercibles args_tuple args_tys
     , Safe.Coercible b
     , HasCallStack
     )
  => Proxy input
  -> Proxy mname
  -> Proxy antiqs
  -> Proxy line
  -> args_tuple
  %1-> Proxy args_tys
  -> (args_tuple %1-> m b)
  %1-> m b
qqMarker = withFrozenCallStack $
    error
      "A quasiquotation marker was not removed. Please, report this as a bug."

class Coercibles xs (tys :: k) | xs -> tys
instance Coercibles () ()
instance (ty ~ Safe.Ty x, Safe.Coercible x, Coercibles xs tys)
    => Coercibles (x, xs) '(ty, tys)
