-- | Candidates for addition to GhcPlugins

module GhcPlugins.Extras
  ( module FamInstEnv
  , module GhcPlugins
  , module GhcPlugins.Extras
  , module TyCoRep
  ) where

import Control.Monad.Writer hiding ((<>))
import Data.Data (Data)
import Data.Maybe (mapMaybe)
import Data.IORef (readIORef)
import ErrUtils (ghcExit)
import FamInstEnv (normaliseType)
import GhcPlugins
import GHC.ThToHs (thRdrNameGuesses)
import IfaceEnv (lookupOrigNameCache)
import qualified Language.Haskell.TH as TH
import NameCache (nsNames)
import TyCoRep


-- | Produces a name in GHC Core from a Template Haskell name.
--
-- Yields Nothing if the name can't be found, which may happen if the
-- module defining the named thing hasn't been loaded.
findTHName :: TH.Name -> CoreM (Maybe Name)
findTHName th_name =
    case thRdrNameGuesses th_name of
      Orig m occ : _ -> do
        hsc_env <- getHscEnv
        nc <- liftIO $ readIORef (hsc_NC hsc_env)
        return $ lookupOrigNameCache (nsNames nc) m occ
      _ -> return Nothing

-- | Yields module annotations with values of the given type.
getModuleAnnotations :: Data a => ModGuts -> [a]
getModuleAnnotations guts =
    mapMaybe (fromSerialized deserializeWithData)
      [ v | Annotation (ModuleTarget _) v <- mg_anns guts ]

-- | Prints the given error message and terminates ghc.
failWith :: SDoc -> CoreM a
failWith m = do
    errorMsg m
    dflags <- getDynFlags
    liftIO $ ghcExit dflags 1
    return (error "ghcExit returned!?") -- unreachable
