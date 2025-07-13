-- | Candidates for addition to GhcPlugins

module GhcPlugins.Extras
  ( module GHC.Core.FamInstEnv
  , module GHC.Core.Reduction
  , module GHC.Plugins
  , module GhcPlugins.Extras
  , module GHC.Core.TyCo.Rep
  , module GHC.Types.ForeignStubs
  ) where

import Control.Monad.Writer
import Data.Data (Data)
import Data.Maybe (mapMaybe)
import GHC.Core.FamInstEnv
import GHC.Core.Reduction (Reduction(..))
import GHC.Core.TyCo.Rep
import GHC.Iface.Env (lookupNameCache)
import GHC.Plugins
import GHC.ThToHs (thRdrNameGuesses)
import GHC.Types.ForeignStubs
import GHC.Utils.Error (ghcExit)
import qualified Language.Haskell.TH as TH


-- | Produces a name in GHC Core from a Template Haskell name.
--
-- Yields Nothing if the name can't be found.
findTHName :: TH.Name -> CoreM (Maybe Name)
findTHName th_name =
    case thRdrNameGuesses th_name of
      Orig m occ : _ -> do
        hsc_env <- getHscEnv
        let nc = hsc_NC hsc_env
        liftIO $ Just <$> lookupNameCache nc m occ
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
    logger <- hsc_logger <$> getHscEnv
    liftIO $ ghcExit logger 1
    return (error "ghcExit returned!?") -- unreachable
