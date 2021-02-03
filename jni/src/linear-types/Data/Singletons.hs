{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Singletons where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat, KnownSymbol)

type family Sing :: k -> Data.Kind.Type

class SingI ty  where
  sing :: Sing ty

instance SingI ('[] :: [k]) where
  sing = SNil
instance (SingI x, SingI xs) => SingI (x ': xs) where
  sing = SCons (sing @x) (sing @xs)

type instance Sing = SSymbol

data SSymbol n = KnownSymbol n => SSym

instance KnownSymbol n => SingI n where
  sing = SSym

type instance Sing = SList

data SList (xs :: [k]) where
  SNil :: SList '[]
  SCons :: Sing x -> SList xs -> SList (x ': xs)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

instance ShowSing k => Show (SList(a :: [k])) where
  showsPrec _ SNil = showString "SNil"
  showsPrec d sxs@(SCons ty tys) =
    case sxs of
      (_ :: SList (x : ys)) -> showParen (d > 10) $
        showString "SCons " . showsPrec 11 ty . showChar ' ' . showsPrec 11 tys
        :: (ShowSing' x, ShowSing' ys) => ShowS

class    (forall (z :: k). ShowSing' z) => ShowSing k
instance (forall (z :: k). ShowSing' z) => ShowSing k

class    Show (Sing z) => ShowSing' (z :: k)
instance Show (Sing z) => ShowSing' z

data SNat n = KnownNat n => SNat
type instance Sing = SNat
instance KnownNat n => SingI n where
  sing = SNat
