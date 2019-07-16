{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Inline.Internal.QQMarker.Names where

import Data.Maybe
import GhcPlugins
import qualified GhcPlugins.Extras
import Language.Java.Inline.Internal.QQMarker

-- | Get the names of all markers used for java quasiquotations.
getQQMarkers :: CoreM [Name]
getQQMarkers = maybeToList <$> GhcPlugins.Extras.findTHName 'qqMarker
