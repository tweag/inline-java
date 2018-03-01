{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main as Criterion
import Control.DeepSeq
import Data.Int
import Data.List (intersperse)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as VS
import Language.Java
import Language.Java.Streaming ()
import Language.Java.Streaming.Jars (getJars)
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

main :: IO ()
main = do
    jars <- getJars
    let classpath = concat $ intersperse ":" jars
    withJVM [Text.encodeUtf8 $ Text.pack $ "-Djava.class.path=" ++ classpath] $
      Criterion.defaultMain [benchBatching]
