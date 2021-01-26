{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Java.SQL.Time where

import Control.Distributed.Closure.TH
import Control.Exception (onException)
import Foreign.JNI
import Language.Java
import Language.Java.Batching
import Language.Java.Batching.Tuple (Tuple2(..))
import Language.Java.Inline

import Data.Fixed (Fixed(..), div', divMod')
import Data.Int
import Data.Time
import qualified Data.Vector as V


-- http://docs.oracle.com/javase/7/docs/api/index.html?java/sql/Date.html

millisecInMin, millisecInHour, millisecInDay :: Int64
millisecInMin  = 60000
millisecInHour = millisecInMin * 60
millisecInDay  = millisecInHour * 24
nanosInSec, picosInMilli :: Int32
nanosInSec   = 10 ^ (9 :: Int32)
picosInMilli = nanosInSec
epoch1970 :: Day
epoch1970 = fromGregorian 1970 1 1

withStatic [d|

  instance Interpretation Day where
    type Interp Day = 'Class "java.sql.Date"

  instance Reify Day where
    reify _jobj = daySinceEpoch1970 <$> [java| $_jobj.getTime() |]

  instance Reflect Day where
    reflect day = new millisec
      where
        millisec = millisecSinceEpoch1970 day

  instance Batchable Day where
    type Batch Day = Batch Int64

  instance BatchReify Day where
    newBatchWriter _ = cast <$> new
      where
        cast
          :: J ('Class "io.tweag.jvm.types.BatchWriters$DateBatchWriter")
          -> J ('Iface "io.tweag.jvm.batching.BatchWriter"
                 <> [Interp Day, Batch Day]
               )
        cast = unsafeCast
    reifyBatch j sz p = V.map daySinceEpoch1970 <$> reifyBatch j sz p

  instance Interpretation LocalTime where
    type Interp LocalTime = 'Class "java.sql.Timestamp"

  instance Reify LocalTime where
    reify jobj =
        localTimeSinceMidnight <$> call jobj "getTime"
                               <*> call jobj "getNanos"

  instance Reflect LocalTime where
    reflect (LocalTime day tod) = do
        jtimestamp <- new millisec
        initTimestampObj nanos jtimestamp `onException` deleteLocalRef jtimestamp
        return jtimestamp
      where
        (timeMillisec, nanos) = milliNanosecSinceMidnight tod
        millisec = millisecSinceEpoch1970 day + timeMillisec

  instance Batchable LocalTime where
    type Batch LocalTime = Batch (Tuple2 Int64 Int32)

  instance BatchReify LocalTime where
    newBatchWriter _ = cast <$> new
      where
        cast
          :: J ('Class "io.tweag.jvm.types.BatchWriters$TimestampBatchWriter")
          -> J ('Iface "io.tweag.jvm.batching.BatchWriter"
                 <> [Interp LocalTime, Batch LocalTime]
               )
        cast = unsafeCast
    reifyBatch j sz p = V.map (\(Tuple2 a b) -> localTimeSinceMidnight a b)
                        <$> reifyBatch j sz p
 |]

initTimestampObj :: Int32 -> (J ('Class "java.sql.Timestamp")) -> IO ()
initTimestampObj nanos jobj = [java| {
    int nanos_obj = $jobj.getNanos();
    int totalNanos = $nanos + nanos_obj;
    if (totalNanos < $nanosInSec)
      $jobj.setNanos(totalNanos);
    else {
          long millisec = $jobj.getTime();
          $jobj.setTime(millisec + 1000);
          $jobj.setNanos(totalNanos - $nanosInSec);
    }
  } |]

millisecSinceEpoch1970 :: Day -> Int64
millisecSinceEpoch1970 day =
    fromIntegral (diffDays day epoch1970) * millisecInDay

milliNanosecSinceMidnight :: TimeOfDay -> (Int64, Int32)
milliNanosecSinceMidnight tod =
  (fromIntegral x, fromIntegral @Int32 $ div' y (10 ^ (3 :: Integer)))
  where
    (x,y) = divMod
      (diffTimeToPicoseconds $ timeOfDayToTime tod)
      (fromIntegral picosInMilli)

-- time is the constructor argument for Timestamp and represents
-- the number of milliseconds since 1/1/1970 midnight. The next are
-- java expressions:
-- nanosec  = (time % 1000) * 1000000
-- millisec = ((time / 1000) * 1000) + (nanosec / 1000000)
-- millisecToTimeOfDay accounts for this overlap between the input args
millisecToTimeOfDay :: Int64 -> Int32 -> TimeOfDay
millisecToTimeOfDay millisec nanosec =
  let (hour, minMillisec) = divMod' millisec millisecInHour
      (mins, secMillisec) = divMod' minMillisec millisecInMin
      nanosec'            = fromIntegral nanosec :: Integer
      pico = MkFixed $
               (fromIntegral secMillisec :: Integer) * fromIntegral nanosInSec +
               (nanosec' * 1000) -
               ((div nanosec' 1000000) * fromIntegral picosInMilli)
  in TimeOfDay hour mins pico

daySinceEpoch1970 :: Int64 -> Day
daySinceEpoch1970 millisec =
    let daysSince1970 = div' millisec  millisecInDay
     in addDays daysSince1970 epoch1970

localTimeSinceMidnight :: Int64 -> Int32 -> LocalTime
localTimeSinceMidnight millisec nanosec =
    let (daysSince1970, dayMillisec) = divMod' millisec millisecInDay
        day = addDays daysSince1970 epoch1970
        time = millisecToTimeOfDay dayMillisec nanosec
     in LocalTime day time
