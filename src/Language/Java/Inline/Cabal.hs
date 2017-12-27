-- | This module contains Cabal @Setup.hs@ hooks to set the @CLASSPATH@ to use
-- when compiling inline code. The @CLASSPATH@ environment variable specifies
-- where to find JVM package dependencies, such as third party packages
-- downloaded from <http://search.maven.org/ Maven Central>.
--
-- You can set the @CLASSPATH@ manually, or extract one from an external build
-- system configuration. Currently supported build systems:
--
-- * <https://gradle.org/ Gradle>

module Language.Java.Inline.Cabal
  ( gradleHooks
  , setGradleClasspath
  , gradleBuild
  ) where

import Data.Monoid
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.PackageDescription (HookedBuildInfo, PackageDescription)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcess)

-- | Adds the 'setGradleClasspath' and 'gradleBuild' hooks.
gradleHooks :: UserHooks -> UserHooks
gradleHooks hooks = hooks
    { preBuild = setGradleClasspath <> preBuild hooks
    , buildHook = gradleBuild <> buildHook hooks
    , preHaddock = setGradleClasspath <> preHaddock hooks
    }

gradleBuildFile :: FilePath
gradleBuildFile = "build.gradle"

findGradleBuild :: FilePath -> IO (Maybe FilePath)
findGradleBuild cwd = do
    let path = cwd </> gradleBuildFile
    yes <- doesFileExist path
    if yes then return (Just path) else return Nothing

-- | Set the @CLASSPATH@ from a Gradle build configuration. Uses the classpath
-- for the @main@ source set.
getGradleClasspath :: FilePath -> IO String
getGradleClasspath parentBuildfile = do
    withSystemTempFile "build.gradle" $ \buildfile h -> do
      hClose h
      writeFile buildfile $
        unlines
          [ "apply from: '" ++ parentBuildfile ++ "'"
          , "task classpath { doLast { println sourceSets.main.compileClasspath.getAsPath() } }"
          ]
      readProcess "gradle" ["-q", "-b", buildfile, "classpath"] ""
        -- trim trailing newlines
        >>= return . concat . lines

-- | Set the @CLASSPATH@ from a Gradle build configuration. Does not override
-- the @CLASSPATH@ if one exists.
setGradleClasspath :: Args -> b -> IO HookedBuildInfo
setGradleClasspath _ _ = do
    here <- getCurrentDirectory
    origclasspath <- lookupEnv "CLASSPATH"
    case origclasspath of
      Nothing -> do
        mbbuildfile <- findGradleBuild here
        case mbbuildfile of
          Nothing -> fail $ unwords [gradleBuildFile, "file not found in", here]
          Just buildfile -> do
            classpath <- getGradleClasspath buildfile
            setEnv "CLASSPATH" classpath
      Just _ -> return ()
    return (Nothing, [])

-- | Call @gradle build@ as part of the Cabal build. Useful to e.g. build
-- auxiliary Java source code and to create packages.
gradleBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
gradleBuild _ _ _ _ = do
    callProcess "gradle" ["build"]
