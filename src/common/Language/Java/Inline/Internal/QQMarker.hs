{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Java.Inline.Internal.QQMarker where

import Data.Proxy
import Data.Singletons (Sing, SingI(..), SomeSing(..))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (Nat, Symbol)
import Language.Java

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
     ( tyres ~ Ty b
     , Coercibles args_tuple args_tys
     , Coercible b
     , HasCallStack
     )
  => Proxy input
  -> Proxy mname
  -> Proxy antiqs
  -> Proxy line
  -> args_tuple
  -> Proxy args_tys
  -> Proxy args_tuple
  -> (Proxy args_tuple -> args_tuple -> m b)
  -> m b
qqMarker = withFrozenCallStack $
    error
      "A quasiquotation marker was not removed. Please, report this as a bug."

class Coercibles xs (tys :: k) | xs -> tys where
  sings :: Proxy xs -> [SomeSing JType]
instance Coercibles () () where
  sings _ = []
instance (ty ~ Ty x, Coercible x, Coercibles xs tys)
    => Coercibles (x, xs) '(ty, tys) where
  sings _ = SomeSing (sing :: Sing ty) : sings (Proxy @xs)
