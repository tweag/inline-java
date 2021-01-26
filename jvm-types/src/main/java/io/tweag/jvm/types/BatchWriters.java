package io.tweag.jvm.types;

import io.tweag.jvm.batching.*;

/**
 *  Various batchers
 *
 * */
public final class BatchWriters {

    public static final class DateBatchWriter
            implements BatchWriter<java.sql.Date, long[]> { 
        private io.tweag.jvm.batching.BatchWriters.LongBatchWriter b =
          new io.tweag.jvm.batching.BatchWriters.LongBatchWriter();
        public void start(int size) { b.start(size); }
        public void set(int i, java.sql.Date d) { b.set(i, d.getTime()); }
        public long[] getBatch() { return b.getBatch(); }
    }

    public static final class TimestampBatchWriter
            implements BatchWriter<java.sql.Timestamp, Tuple2<long[],int[]>> {
        private io.tweag.jvm.batching.BatchWriters.Tuple2BatchWriter<Long, Integer, long[], int[]> b =
            new io.tweag.jvm.batching.BatchWriters.Tuple2BatchWriter<Long, Integer, long[], int[]>
              ( new io.tweag.jvm.batching.BatchWriters.LongBatchWriter()
              , new io.tweag.jvm.batching.BatchWriters.IntegerBatchWriter()
              );
        public void start(int size) { b.start(size); }
        public void set(int i, java.sql.Timestamp t) {
            b.set(i, new Tuple2(t.getTime(), t.getNanos()));
        }
        public Tuple2<long[], int[]> getBatch() { return b.getBatch(); }
    }
}
