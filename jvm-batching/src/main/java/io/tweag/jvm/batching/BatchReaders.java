package io.tweag.jvm.batching;

/**
 *  Various BatchReaders
 * */
public class BatchReaders {

    public static final class BooleanBatchReader implements BatchReader<boolean[], Boolean> {
        private boolean[] arr = new boolean[0];
        public int getSize() { return arr.length; };
        public Boolean get(int i) { return arr[i]; };
        public void setBatch(boolean[] b) { arr = b; };
        public Class<Boolean> getElemClass() { return Boolean.class; };
    }

    public static final class ByteBatchReader implements BatchReader<byte[], Byte> {
        private byte[] arr = new byte[0];
        public int getSize() { return arr.length; };
        public Byte get(int i) { return arr[i]; };
        public void setBatch(byte[] b) { arr = b; };
        public Class<Byte> getElemClass() { return Byte.class; };
    }

    public static final class CharacterBatchReader implements BatchReader<char[], Character> {
        private char[] arr = new char[0];
        public int getSize() { return arr.length; };
        public Character get(int i) { return arr[i]; };
        public void setBatch(char[] b) { arr = b; };
        public Class<Character> getElemClass() { return Character.class; };
    }

    public static final class ShortBatchReader implements BatchReader<short[], Short> {
        private short[] arr = new short[0];
        public int getSize() { return arr.length; };
        public Short get(int i) { return arr[i]; };
        public void setBatch(short[] b) { arr = b; };
        public Class<Short> getElemClass() { return Short.class; };
    }

    public static final class IntegerBatchReader implements BatchReader<int[], Integer> {
        private int[] arr = new int[0];
        public int getSize() { return arr.length; };
        public Integer get(int i) { return arr[i]; };
        public void setBatch(int[] b) { arr = b; };
        public Class<Integer> getElemClass() { return Integer.class; };
    }

    public static final class LongBatchReader implements BatchReader<long[], Long> {
        private long[] arr = new long[0];
        public int getSize() { return arr.length; };
        public Long get(int i) { return arr[i]; };
        public void setBatch(long[] b) { arr = b; };
        public Class<Long> getElemClass() { return Long.class; };
    }

    public static final class FloatBatchReader implements BatchReader<float[], Float> {
        private float[] arr = new float[0];
        public int getSize() { return arr.length; };
        public Float get(int i) { return arr[i]; };
        public void setBatch(float[] b) { arr = b; };
        public Class<Float> getElemClass() { return Float.class; };
    }

    public static final class DoubleBatchReader implements BatchReader<double[], Double> {
        private double[] arr = new double[0];
        public int getSize() { return arr.length; };
        public Double get(int i) { return arr[i]; };
        public void setBatch(double[] b) { arr = b; };
        public Class<Double> getElemClass() { return Double.class; };
    }

    public static final class ObjectBatchReader implements BatchReader<Object[], Object> {
        private Object[] arr = new Object[0];
        public int getSize() { return arr.length; };
        public Object get(int i) { return arr[i]; };
        public void setBatch(Object[] b) { arr = b; };
        public Class<Object> getElemClass() { return Object.class; };
    }

    public static final class ByteArrayBatchReader
            implements BatchReader<Tuple2<byte[], int[]>, byte[]> {
        private byte[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<byte[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public byte[] get(int i) {
            final int len = end[i] - start;
            byte[] res = new byte[len];
            System.arraycopy(arr, start, res, 0, len);
            start = end[i];
            return res;
        }
        public Class<byte[]> getElemClass() { return byte[].class; };
    }

    public static final class BooleanArrayBatchReader
            implements BatchReader<Tuple2<boolean[], int[]>, boolean[]> {
        private boolean[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<boolean[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public boolean[] get(int i) {
            final int len = end[i] - start;
            boolean[] res = new boolean[len];
            System.arraycopy(arr, start, res, 0, len);
            start = end[i];
            return res;
        }
        public Class<boolean[]> getElemClass() { return boolean[].class; };
    }

    public static final class CharArrayBatchReader
            implements BatchReader<Tuple2<char[], int[]>, char[]> {
        private char[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<char[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public char[] get(int i) {
            final int len = end[i] - start;
            char[] res = new char[len];
            System.arraycopy(arr, start, res, 0, len);
            start = end[i];
            return res;
        }
        public Class<char[]> getElemClass() { return char[].class; };
    }

    public static final class ShortArrayBatchReader
            implements BatchReader<Tuple2<short[], int[]>, short[]> {
        private short[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<short[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public short[] get(int i) {
            final int len = end[i] - start;
            short[] res = new short[len];
            System.arraycopy(arr, start, res, 0, len);
            start = end[i];
            return res;
        }
        public Class<short[]> getElemClass() { return short[].class; };
    }

    public static final class IntArrayBatchReader
            implements BatchReader<Tuple2<int[], int[]>, int[]> {
        private int[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<int[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public int[] get(int i) {
            final int len = end[i] - start;
            int[] res = new int[len];
            System.arraycopy(arr, start, res, 0, len);
            start = end[i];
            return res;
        }
        public Class<int[]> getElemClass() { return int[].class; };
    }

    public static final class LongArrayBatchReader
            implements BatchReader<Tuple2<long[], int[]>, long[]> {
        private long[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<long[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public long[] get(int i) {
            final int len = end[i] - start;
            long[] res = new long[len];
            System.arraycopy(arr, start, res, 0, len);
            start = end[i];
            return res;
        }
        public Class<long[]> getElemClass() { return long[].class; };
    }

    public static final class FloatArrayBatchReader
            implements BatchReader<Tuple2<float[], int[]>, float[]> {
        private float[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<float[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public float[] get(int i) {
            final int len = end[i] - start;
            float[] res = new float[len];
            System.arraycopy(arr, start, res, 0, len);
            start = end[i];
            return res;
        }
        public Class<float[]> getElemClass() { return float[].class; };
    }

    public static final class DoubleArrayBatchReader
            implements BatchReader<Tuple2<double[], int[]>, double[]> {
        private double[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<double[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public double[] get(int i) {
            final int len = end[i] - start;
            double[] res = new double[len];
            System.arraycopy(arr, start, res, 0, len);
            start = end[i];
            return res;
        }
        public Class<double[]> getElemClass() { return double[].class; };
    }

    public static final class ObjectArrayBatchReader<A, T>
            implements BatchReader<Tuple2<A, int[]>, T[]> {
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        private final BatchReader<A, T> br;
        public ObjectArrayBatchReader(BatchReader<A, T> br) {
            this.br = br;
        }
        public void setBatch(Tuple2<A, int[]> t) {
            br.setBatch(t._1);
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public T[] get(int i) {
            final int len = end[i] - start;
            T[] res = (T[]) java.lang.reflect.Array.newInstance(br.getElemClass(), len);
            for(int j=0;j<len;j++)
                res[j] = br.get(start + j);
            start = end[i];
            return res;
        }
        public Class<T[]> getElemClass() {
            return (Class<T[]>) java.lang.reflect.Array.newInstance(br.getElemClass(), 0).getClass();
        }
    }

    public static final class StringArrayBatchReader
            implements BatchReader<Tuple2<char[], int[]>, String> {
        private char[] arr;
        // end[i] tells the first position after the i-th array.
        private int[] end = new int[0];
        private int start;
        public void setBatch(Tuple2<char[], int[]> t) {
            arr = t._1;
            end = t._2;
            start = 0;
        }
        public int getSize() { return end.length; }
        public String get(int i) {
            final int len = end[i] - start;
            String res = new String(arr, start, len);
            start = end[i];
            return res;
        }
        public Class<String> getElemClass() { return String.class; };
    }
}
