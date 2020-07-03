{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Singletons where

import GHC.TypeLits (KnownSymbol, Symbol)

type family Sing (ty :: k) :: *

class SingI (ty :: k)  where
  sing :: Sing ty

instance SingI ('[] :: [k]) where
  sing = SNil
instance (SingI x, SingI xs) => SingI (x ': xs) where
  sing = SCons sing sing

type instance Sing (n :: Symbol) = SSymbol n

data SSymbol n = KnownSymbol n => SSym

instance KnownSymbol n => SingI n where
  sing = SSym

type instance Sing (a :: [b]) = SList a

data SList xs where
  SNil :: SList '[]
  SCons :: Sing x -> SList xs -> SList (x ': xs)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

instance (forall x. Show (Sing (x :: k))) => Show (SList (a :: [k])) where
  showsPrec _ SNil = showString "SNil"
  showsPrec d (SCons ty tys) = showParen (d > 10) $
      showString "SCons " . showsPrec 11 ty . showChar ' ' . showsPrec 11 tys
