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
  , prependClasspathWithGradle
  , gradleBuild
  , addJarsToClasspath
  ) where

import Control.Exception (evaluate)
import Control.Monad (when)
import Data.Monoid
import Data.List (intersperse, isPrefixOf)
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.PackageDescription
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, readProcess)

-- | Adds the 'prependClasspathWithGradle' and 'gradleBuild' hooks.
--
-- Also adds the jar produced by gradle to the data-files.
gradleHooks :: UserHooks -> UserHooks
gradleHooks hooks = hooks
    { preBuild = prependClasspathWithGradle <> preBuild hooks
    , buildHook = gradleBuild <> buildHook hooks
    , preHaddock = prependClasspathWithGradle <> preHaddock hooks
    , instHook = instHook simpleUserHooks . addJarToDataFiles
    , copyHook = copyHook simpleUserHooks . addJarToDataFiles
    , regHook = regHook simpleUserHooks . addJarToDataFiles
    }

-- | Prepends the given jar paths to the CLASSPATH.
addJarsToClasspath :: [FilePath] -> UserHooks -> UserHooks
addJarsToClasspath extraJars hooks = hooks
    { preBuild = setClasspath extraJars <> preBuild hooks
    , preHaddock = setClasspath extraJars <> preHaddock hooks
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
        -- Get the last line of output. Sometimes Gradle prepends garbage to the
        -- output despite the -q flag.
        >>= return . last . lines

-- | Prepends the @CLASSPATH@ with the classpath from a Gradle build
-- configuration.
prependClasspathWithGradle :: Args -> b -> IO HookedBuildInfo
prependClasspathWithGradle _ _ = do
    here <- getCurrentDirectory
    origclasspath <- lookupEnv "CLASSPATH"
    mbbuildfile <- findGradleBuild here
    case mbbuildfile of
      Nothing -> fail $ unwords [gradleBuildFile, "file not found in", here]
      Just buildfile -> do
        classpath <- getGradleClasspath buildfile
        setEnv "CLASSPATH" $ classpath ++ maybe "" (':':) origclasspath
    return (Nothing, [])

setClasspath :: [FilePath] -> a -> b -> IO HookedBuildInfo
setClasspath extraJars _ _ = do
    mclasspath <- lookupEnv "CLASSPATH"
    setEnv "CLASSPATH" $
      concat $ intersperse ":" $
        maybe extraJars (\c -> extraJars ++ [c]) mclasspath
    return (Nothing, [])

-- | Call @gradle build@ as part of the Cabal build. Useful to e.g. build
-- auxiliary Java source code and to create packages.
gradleBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
gradleBuild pd _ _ _ = do
    setProjectName
    callProcess "gradle" ["build"]
  where
    settingsFile = "settings.gradle"

    setProjectName :: IO ()
    setProjectName = do
      missingProjectName <- isProjectNameMissing
      when missingProjectName $
          appendFile "settings.gradle" $
            "\nrootProject.name = '" ++
            unPackageName (pkgName (package pd)) ++ "'\n"

    isProjectNameMissing :: IO Bool
    isProjectNameMissing = do
      exists <- doesFileExist settingsFile
      if not exists
      then return True
      else withFile settingsFile ReadMode $ \h -> do
             b <- all (not . ("rootProject.name =" `isPrefixOf`)) . lines
               <$> hGetContents h
             -- If we don't evaluate the boolean before returning, the
             -- file handle will be closed when we try to read the file.
             evaluate b

-- "<pkgname>.jar" is a file build by gradle during building. This file needs
-- to be installed together with the library, otherwise it wouldn't be available
-- to build other code depending on it. Therefore, we add the jar to the field
-- dataFiles in the hooks instHook, copyHook and regHook. This way the jar is
-- installed, but cabal does not account it when deciding if the package needs
-- to be rebuilt.

addJarToDataFiles :: PackageDescription -> PackageDescription
addJarToDataFiles pd = pd
    { dataFiles =
        ("build/libs" </> unPackageName (pkgName (package pd)) <.> "jar")
        : dataFiles pd
    }
