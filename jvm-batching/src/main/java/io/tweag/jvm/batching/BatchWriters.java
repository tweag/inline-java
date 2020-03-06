package io.tweag.jvm.batching;

/**
 *  Various batchers
 *
 * */
public final class BatchWriters {

    public static final class BooleanBatchWriter implements BatchWriter<Boolean, boolean[]> {
        private boolean[] arr;
        public void start(int batchSize) { arr = new boolean[batchSize]; };
        public void set(int i, Boolean o) { arr[i] = o; };
        public boolean[] getBatch() { return arr; };
    }

    public static final class CharacterBatchWriter implements BatchWriter<Character, char[]> {
        private char[] arr;
        public void start(int batchSize) { arr = new char[batchSize]; };
        public void set(int i, Character o) { arr[i] = o; };
        public char[] getBatch() { return arr; };
    }

    public static final class ByteBatchWriter implements BatchWriter<Byte, byte[]> {
        private byte[] arr;
        public void start(int batchSize) { arr = new byte[batchSize]; };
        public void set(int i, Byte o) { arr[i] = o; };
        public byte[] getBatch() { return arr; };
    }

    public static final class ShortBatchWriter implements BatchWriter<Short, short[]> {
        private short[] arr;
        public void start(int batchSize) { arr = new short[batchSize]; };
        public void set(int i, Short o) { arr[i] = o; };
        public short[] getBatch() { return arr; };
    }

    public static final class IntegerBatchWriter implements BatchWriter<Integer, int[]> {
        private int[] arr;
        public void start(int batchSize) { arr = new int[batchSize]; };
        public void set(int i, Integer o) { arr[i] = o; };
        public int[] getBatch() { return arr; };
    }

    public static final class LongBatchWriter implements BatchWriter<Long, long[]> {
        private long[] arr;
        public void start(int batchSize) { arr = new long[batchSize]; };
        public void set(int i, Long o) { arr[i] = o; };
        public long[] getBatch() { return arr; };
    }

    public static final class FloatBatchWriter implements BatchWriter<Float, float[]> {
        private float[] arr;
        public void start(int batchSize) { arr = new float[batchSize]; };
        public void set(int i, Float o) { arr[i] = o; };
        public float[] getBatch() { return arr; };
    }

    public static final class DoubleBatchWriter implements BatchWriter<Double, double[]> {
        private double[] arr;
        public void start(int batchSize) { arr = new double[batchSize]; };
        public void set(int i, Double o) { arr[i] = o; };
        public double[] getBatch() { return arr; };
    }

    public static final class ObjectBatchWriter implements BatchWriter<Object, Object[]> {
        private Object[] arr;
        public void start(int batchSize) { arr = new Object[batchSize]; };
        public void set(int i, Object o) { arr[i] = o; };
        public Object[] getBatch() { return arr; };
    }

    public static final class ByteArrayBatchWriter
            implements BatchWriter<byte[], Tuple2<byte[], int[]> > {
        private byte[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new byte[size][];
            top = 0;
        }
        public void set(int i, byte[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<byte[], int[]> getBatch() {
            byte[] batch = new byte[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                System.arraycopy(arrays[i], 0, batch, pos, arrays[i].length);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<byte[], int[]>(batch, end);
        }
    }

    public static final class BooleanArrayBatchWriter
            implements BatchWriter<boolean[], Tuple2<boolean[], int[]> > {
        private boolean[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new boolean[size][];
            top = 0;
        }
        public void set(int i, boolean[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<boolean[], int[]> getBatch() {
            boolean[] batch = new boolean[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                System.arraycopy(arrays[i], 0, batch, pos, arrays[i].length);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<boolean[], int[]>(batch, end);
        }
    }

    public static final class CharArrayBatchWriter
            implements BatchWriter<char[], Tuple2<char[], int[]> > {
        private char[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new char[size][];
            top = 0;
        }
        public void set(int i, char[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<char[], int[]> getBatch() {
            char[] batch = new char[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                System.arraycopy(arrays[i], 0, batch, pos, arrays[i].length);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<char[], int[]>(batch, end);
        }
    }

    public static final class ShortArrayBatchWriter
            implements BatchWriter<short[], Tuple2<short[], int[]> > {
        private short[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new short[size][];
            top = 0;
        }
        public void set(int i, short[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<short[], int[]> getBatch() {
            short[] batch = new short[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                System.arraycopy(arrays[i], 0, batch, pos, arrays[i].length);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<short[], int[]>(batch, end);
        }
    }

    public static final class IntArrayBatchWriter
            implements BatchWriter<int[], Tuple2<int[], int[]> > {
        private int[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new int[size][];
            top = 0;
        }
        public void set(int i, int[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<int[], int[]> getBatch() {
            int[] batch = new int[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                System.arraycopy(arrays[i], 0, batch, pos, arrays[i].length);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<int[], int[]>(batch, end);
        }
    }

    public static final class LongArrayBatchWriter
            implements BatchWriter<long[], Tuple2<long[], int[]> > {
        private long[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new long[size][];
            top = 0;
        }
        public void set(int i, long[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<long[], int[]> getBatch() {
            long[] batch = new long[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                System.arraycopy(arrays[i], 0, batch, pos, arrays[i].length);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<long[], int[]>(batch, end);
        }
    }

    public static final class FloatArrayBatchWriter
            implements BatchWriter<float[], Tuple2<float[], int[]> > {
        private float[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new float[size][];
            top = 0;
        }
        public void set(int i, float[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<float[], int[]> getBatch() {
            float[] batch = new float[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                System.arraycopy(arrays[i], 0, batch, pos, arrays[i].length);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<float[], int[]>(batch, end);
        }
    }

    public static final class DoubleArrayBatchWriter
            implements BatchWriter<double[], Tuple2<double[], int[]> > {
        private double[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new double[size][];
            top = 0;
        }
        public void set(int i, double[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<double[], int[]> getBatch() {
            double[] batch = new double[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                System.arraycopy(arrays[i], 0, batch, pos, arrays[i].length);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<double[], int[]>(batch, end);
        }
    }

    public static final class ObjectArrayBatchWriter<T, B>
            implements BatchWriter<T[], Tuple2<B, int[]> > {
        private final BatchWriter<T, B> ob;
        private T[][] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;

        public ObjectArrayBatchWriter(BatchWriter<T, B> ob) {
            this.ob = ob;
        }

        public void start(int size) {
            end = new int[size];
            arrays = (T[][]) new Object[size][];
            top = 0;
        }
        public void set(int i, T[] vec) {
            if (vec == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = vec;
            top += vec.length;
            end[i] = top;
        }
        public Tuple2<B, int[]> getBatch() {
            ob.start(top);
            int k = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                for(int j=0;j<arrays[i].length;j++) {
                    ob.set(k, arrays[i][j]);
                    k++;
                }
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<B, int[]>(ob.getBatch(), end);
        }
    }

    public static final class StringArrayBatchWriter
            implements BatchWriter<String, Tuple2<char[], int[]> > {
        private String[] arrays;
        // end[i] tells the first position after the i-th array.
        private int[] end;
        // top tells the first position not occupied by arrays in the batch.
        private int top;
        public void start(int size) {
            end = new int[size];
            arrays = new String[size];
            top = 0;
        }
        public void set(int i, String s) {
            if (s == null)
                throw new RuntimeException("null vectors are unsupported when reifying");
            arrays[i] = s;
            top += s.length();
            end[i] = top;
        }
        public Tuple2<char[], int[]> getBatch() {
            char[] batch = new char[top];
            int pos = 0;
            for(int i=0;i<end.length && arrays[i]!=null;i++) {
                arrays[i].getChars(0, arrays[i].length(), batch, pos);
                pos = end[i];
                // Release the reference to the array so it can be reclaimed
                // before the loop is over.
                arrays[i] = null;
            }
            return new Tuple2<char[], int[]>(batch, end);
        }
    }

    public static final class NullableBatchWriter<A, B>
            implements BatchWriter<A, Tuple2<boolean[], B>> {
        private boolean isnull[];
        final BatchWriter<A, B> b;
        public NullableBatchWriter(BatchWriter<A, B> b) {
            this.b = b;
        }
        public void start(int size) {
            b.start(size);
            isnull = new boolean[size];
        }
        public void set(int i, A a) {
            if (null == a)
                isnull[i] = true;
            else
                b.set(i, a);
        }
        public Tuple2<boolean[], B> getBatch() {
            return new Tuple2(isnull, b.getBatch());
        }
    }
}
