package io.tweag.jarify;

import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.zip.*;

/* The `JarifyBase` class exists to ensure that appropriate shared objects
 * (those in the 'jarify-app.zip' archive within the JAR in which this class
 * resides) are dynamically loaded *before* attempting to access any
 * functionality provided by them. The Haskell program (named 'hsapp') is
 * also loaded.
 */
public class JarifyBase {
    static {
        try {
            InputStream in =
                JarifyBase.class.getResourceAsStream("/jarify-app.zip");
            File jarifyAppZipFile =
                File.createTempFile("jarify-app-", ".zip");
            Files.copy(in, jarifyAppZipFile.toPath(),
                StandardCopyOption.REPLACE_EXISTING);
            in.close();
            try {
              loadApplication(jarifyAppZipFile, "hsapp");
            } finally {
              jarifyAppZipFile.delete();
            }
        } catch (Exception e) {
            System.err.println(e);
            throw new ExceptionInInitializerError(e);
        }
    }

    private static void loadApplication(File archive, String appName)
        throws IOException
    {
        // Extract all files from the .zip archive.
        //
        ZipFile zip = new ZipFile(archive);
        String tmpDir = System.getProperty("java.io.tmpdir");
        Path jarifyAppTmpDir =
            Files.createTempDirectory(Paths.get(tmpDir), "jarify-app-");
        ArrayList<Path> pathsList = new ArrayList();
        try {
          for (Enumeration e = zip.entries(); e.hasMoreElements(); ) {
            ZipEntry entry = (ZipEntry)e.nextElement();
            InputStream in = zip.getInputStream(entry);
            Path path = jarifyAppTmpDir.resolve(entry.getName());
            pathsList.add(path);
            Files.copy(in, path);
            in.close();
          }
          zip.close();

          // Dynamically load the app.
          //
          System.load(jarifyAppTmpDir.resolve(appName).toString());
        } finally {
          // Delete the app binary and its libraries, now that they are loaded.
          //
          for (Path p : pathsList)
            p.toFile().delete();
        }
        jarifyAppTmpDir.toFile().delete();
    }

    public static native <R> R apply(byte[] cos, Object... args);
}
