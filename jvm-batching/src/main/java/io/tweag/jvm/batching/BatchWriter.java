package io.tweag.jvm.batching;

/**
 *  A BatchWriter<A,B> is an object that takes elements of type A one at
 *  a time and yields a batch of type B containing all of them.
 */
public interface BatchWriter<A, B> {
    /// Starts a new batch of the given size
    public void start(int batchSize);
    /// Sets the object at position i of the batch.
    public void set(int i, A o);
    /// Yields the batch.
    public B getBatch();
}
