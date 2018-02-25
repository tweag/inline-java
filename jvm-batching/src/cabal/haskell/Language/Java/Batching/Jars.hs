-- | Provides the paths to the jars needed to build with jvm-batching.
module Language.Java.Batching.Jars where

import Paths_jvm_batching (getDataFileName)

getJars :: IO [String]
getJars = (:[]) <$> getDataFileName "build/libs/jvm-batching.jar"
