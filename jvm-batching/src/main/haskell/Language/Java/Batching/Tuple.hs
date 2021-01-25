{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Java.Batching.Tuple where

import Control.Distributed.Closure.TH
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V
import Language.Java
import Language.Java.Batching
import Language.Java.Inline


data Tuple2 a b = Tuple2 a b
  deriving (Show, Eq)

withStatic [d|

  instance (Interpretation a, Interpretation b) => Interpretation (Tuple2 a b) where
    type Interp (Tuple2 a b) =
           'Class "io.tweag.jvm.batching.Tuple2" <> '[Interp a, Interp b]

  instance (Reify a, Reify b) => Reify (Tuple2 a b) where
    reify jobj =
      let _jobj = (unsafeCast jobj :: J ('Class "io.tweag.jvm.batching.Tuple2")) in
      [java| $_jobj._1 |] `withLocalRef` \ja ->
      [java| $_jobj._2 |] `withLocalRef` \jb ->
      Tuple2 <$> reify (unsafeCast (ja :: JObject))
             <*> reify (unsafeCast (jb :: JObject))

  instance (Reflect a, Reflect b) => Reflect (Tuple2 a b) where
    reflect (Tuple2 a b) =
      reflect a `withLocalRef` \ja ->
      reflect b `withLocalRef` \jb ->
      generic <$> new (upcast ja) (upcast jb)

  instance (Batchable a, Batchable b) => Batchable (Tuple2 a b) where
    type Batch (Tuple2 a b) = 'Class "io.tweag.jvm.batching.Tuple2" <> '[Batch a, Batch b]

  instance (BatchReify a, BatchReify b) => BatchReify (Tuple2 a b) where
    newBatchWriter _proxy =
      newBatchWriter (Proxy :: Proxy a) `withLocalRef` \b1 ->
      newBatchWriter (Proxy :: Proxy b) `withLocalRef` \b2 ->
      (unsafeCast
        :: J ('Class "io.tweag.jvm.batching.BatchWriters$Tuple2BatchWriter")
        -> J ('Iface "io.tweag.jvm.batching.BatchWriter"
                     <> [Interp (Tuple2 a b), Batch (Tuple2 a b)])
        )
        <$> new b1 b2
    reifyBatch jxs n p =
      let _jxs = (unsafeCast jxs :: J ('Class "io.tweag.jvm.batching.Tuple2")) in
      V.zipWith Tuple2
      <$> ([java| $_jxs._1 |] `withLocalRef`
            ((\j -> reifyBatch j n p) . (unsafeCast :: JObject -> J (Batch a))))
      <*> ([java| $_jxs._2 |] `withLocalRef`
            ((\j -> reifyBatch j n p) . (unsafeCast :: JObject -> J (Batch b))))

  instance (BatchReflect a, BatchReflect b)
        => BatchReflect (Tuple2 a b) where
    newBatchReader _proxy =
      newBatchReader (Proxy :: Proxy a) `withLocalRef` \b1 ->
      newBatchReader (Proxy :: Proxy b) `withLocalRef` \b2 ->
        (unsafeCast
          :: J ('Class "io.tweag.jvm.batching.BatchReaders$Tuple2BatchReader")
          -> J ('Iface "io.tweag.jvm.batching.BatchReader"
                       <> [Batch (Tuple2 a b), Interp (Tuple2 a b)]
               )
          )
          <$> new b1 b2
    reflectBatch pairs =
      let (as, bs) = V.unzip $ V.map (\(Tuple2 a b) -> (a, b)) pairs
       in reflectBatch as `withLocalRef` \jas ->
          reflectBatch bs `withLocalRef` \jbs ->
            (generic :: J ('Class "io.tweag.jvm.batching.Tuple2") -> J (Batch (Tuple2 a b)))
              <$> new (upcast jas) (upcast jbs)
 |]
