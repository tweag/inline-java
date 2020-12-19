{-# LANGUAGE ScopedTypeVariables #-}

module Bench(benchBatching) where

import Criterion.Main as Criterion
import Control.DeepSeq
import Data.Int
import qualified Data.Vector.Storable as VS
import Language.Java
import Language.Java.Streaming ()
import Streaming (Of, Stream)
import qualified Streaming.Prelude as Streaming

roundtrip :: forall a. (Reify a, Reflect a) => a -> IO ()
roundtrip a = pushWithSizeHint 30 $ (reflect a >>= reify :: IO a) >> pop

intValues :: [Int32]
intValues = [1..4000]

vValues :: [VS.Vector Int32]
vValues = [ VS.fromList [i, i] | i <- [1..2000] ]

benchBatching :: Benchmark
benchBatching = deepseq intValues $
    bgroup "roundtrips"
    [ bgroup "ints"
      [ bgroup "no batching"
        [ bench "list" $ nfIO $ roundtrip intValues
        ]
      , bgroup "batching"
        [ bench "streams" $ nfIO $ roundtrip
            (Streaming.each intValues :: Stream (Of Int32) IO ())
        ]
      ]
    , bgroup "int vectors"
      [ bgroup "no batching"
        [ bench "list" $ nfIO $ roundtrip vValues
        ]
      , bgroup "batching"
        [ bench "streams" $ nfIO $ roundtrip
            (Streaming.each vValues :: Stream (Of (VS.Vector Int32)) IO ())
        ]
      ]
    ]
