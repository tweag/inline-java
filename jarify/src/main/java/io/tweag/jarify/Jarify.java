package io.tweag.jarify;

/* The static initializer of the `Jarify` class ensures that Haskell RTS is
 * properly initialized before any Haskell code is called (via the apply
 * method).
 */
public class Jarify extends JarifyBase {
    static {
        initializeHaskellRTS();
    }

    public static native <R> R apply(byte[] cos, Object... args);
    private static native void initializeHaskellRTS();
}
