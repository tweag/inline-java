import Distribution.Simple
import Language.Java.Inline.Cabal (gradleHooks)

main = defaultMainWithHooks (gradleHooks simpleUserHooks)
