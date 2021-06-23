{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Inline.Internal.QQMarker.Names where

import Data.Maybe
import GHC.Plugins
import qualified GhcPlugins.Extras
import Language.Java.Inline.Internal.QQMarker
import qualified Language.Java.Inline.Internal.QQMarker.Safe as Safe

-- | Get the names of all markers used for java quasiquotations.
getQQMarkers :: CoreM [Name]
getQQMarkers = do
   ma <- GhcPlugins.Extras.findTHName 'qqMarker
   mb <- GhcPlugins.Extras.findTHName 'Safe.qqMarker
   return $ catMaybes [ma, mb]
