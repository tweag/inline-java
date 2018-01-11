{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main as Criterion
import Control.DeepSeq
import Data.Int
import qualified Data.Vector as V
import Data.List.Split (chunksOf)
import qualified Data.Vector.Storable as VS
import Language.Java
import Language.Java.Batching ()

roundtrip :: forall a. (Reify a, Reflect a) => a -> IO ()
roundtrip a = pushWithSizeHint 30 $ (reflect a >>= reify :: IO a) >> pop

intValues :: [Int32]
intValues = [1..1000]

vValues :: [VS.Vector Int32]
vValues = [ VS.fromList [i, i] | i <- [1..500] ]

benchBatching :: Benchmark
benchBatching = deepseq intValues $
    bgroup "roundtrips"
    [ bgroup "ints"
      [ bgroup "no batching"
        [ bench "list" $ nfIO $ roundtrip intValues
        , bench "storable array" $ nfIO $ roundtrip $ VS.fromList intValues
        ]
      , bgroup "batching"
        [ bench "bare" $ nfIO $ roundtrip $ V.fromList intValues
        , bench "unboxed vectors" $ nfIO $ roundtrip $
            V.fromList $ map VS.fromList $
              chunksOf (length intValues `div` 7) intValues
        , bench "boxed vectors" $ nfIO $ roundtrip $
            V.fromList $ map V.fromList $
              chunksOf (length intValues `div` 7) intValues
        ]
      ]
    , bgroup "int vectors"
      [ bgroup "no batching"
        [ bench "list" $ nfIO $ roundtrip vValues
        ]
      , bgroup "batching"
        [ bench "bare" $ nfIO $ roundtrip $ V.fromList vValues
        , bench "vectors of vectors" $ nfIO $ roundtrip $
            V.fromList $ map V.fromList $
              chunksOf (length vValues `div` 7) vValues
        ]
      ]
    ]

main :: IO ()
main = withJVM ["-Djava.class.path=build/libs/jvm-batching.jar"] $
    Criterion.defaultMain [benchBatching]
