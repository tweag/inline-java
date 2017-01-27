package io.tweag.jarify;

public class JarifyMain extends JarifyBase {
    private static native void invokeMain(String[] args);
    public static void main(String[] args) {
        invokeMain(args);
    }
}
