-- | Optional code macros for convenience.

{-# LANGUAGE TemplateHaskell #-}

module Language.Java.TH (makeConversions) where

import Control.Distributed.Closure.TH (withStatic)
import qualified Language.Haskell.TH as TH
import Language.Java

-- | Define instances of the three conversion classes, 'Coercible', 'Reify' and
-- 'Reflect' for the given datatype. Example usage:
--
-- @
-- import qualified Language.Java.TH as Java
--
-- newtype JOptionPane = JOptionPane (J ('Class "javax.swing.JOptionPane"))
-- Java.makeConversions ''JOptionPane
-- @
--
-- This is equivalent to:
--
-- @
-- newtype JOptionPane = JOptionPane (J ('Class "javax.swing.JOptionPane"))
-- instance Coercible JOptionPane ('Class "javax.swing.JOptionPane")
-- instance Reify JOptionPane ('Class "javax.swing.JOptionPane")
-- instance Reflect JOptionPane ('Class "javax.swing.JOptionPane")
-- @
makeConversions :: TH.Name -> TH.Q [TH.Dec]
makeConversions x = do
    info <- TH.reify x
    let ty_x = return (TH.ConT x)
        ty_cls = case info of
          TH.TyConI (TH.DataD [] _ _ _ [cstr] _) -> case cstr of
            TH.NormalC _ [(_, TH.AppT (TH.ConT nm) ty)] | nm == ''J -> return ty
            TH.RecC _ [(_, _, TH.AppT (TH.ConT nm) ty)] | nm == ''J -> return ty
            _ -> notsupported
          _ -> notsupported
    withStatic [d|
      instance Coercible $ty_x $ty_cls
      instance Reify $ty_x $ty_cls
      instance Reflect $ty_x $ty_cls
      |]
  where
    notsupported = fail
      "makeConversions failed. Only newtype wrappers of (J ty) are supported."
