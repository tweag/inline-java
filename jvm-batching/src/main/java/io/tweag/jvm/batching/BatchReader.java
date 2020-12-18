package io.tweag.jvm.batching;

/**
 *  A BatchReader&lt;A, B&gt; provides access to the elements of type B in a
 *  batch of type A.
 */
public interface BatchReader<A, B> {
    /// Yields the batch size.
    public int getSize();
    /// Yields the object at position i of the batch.
    public B get(int i);
    /// Sets the batch read by get.
    public void setBatch(A o);
    /// Yields the class of the batched objects.
    public Class<B> getElemClass();
}
