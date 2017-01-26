package io.tweag.jarify;

public class JarifyMain {
    private static native void invokeMain(String[] args);
    public static void main(String[] args) {
        invokeMain(args);
    }
}
