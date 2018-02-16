import Distribution.Simple
import Language.Java.Inline.Cabal
import qualified Language.Java.Batching.Jars

main = do
    jars <- Language.Java.Batching.Jars.getJars
    defaultMainWithHooks (addJarsToClasspath jars simpleUserHooks)
