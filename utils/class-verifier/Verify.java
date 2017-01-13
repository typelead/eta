import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.net.URL;
import java.util.jar.JarFile;
import java.util.jar.JarEntry;
import java.util.Enumeration;
import java.net.URLClassLoader;
import java.net.MalformedURLException;
import java.io.IOException;

// Adapted from
// http://stackoverflow.com/questions/11016092/how-to-load-classes-at-runtime-from-a-folder-or-jar

public class Verify {
    public static void main(String[] args) throws IOException,
                                                  MalformedURLException,
                                                  ClassNotFoundException {
        String pathToJar = args[0];
        JarFile jarFile = new JarFile(pathToJar);
        Enumeration<JarEntry> e = jarFile.entries();

        URL[] urls = { new URL("jar:file:" + pathToJar+"!/") };
        URLClassLoader cl = URLClassLoader.newInstance(urls);

        while (e.hasMoreElements()) {
            JarEntry je = e.nextElement();
            if(je.isDirectory() || !je.getName().endsWith(".class")){
                continue;
            }
            // -6 because of .class
            String className = je.getName().substring(0,je.getName().length()-6);
            className = className.replace('/', '.');
            Class c = cl.loadClass(className);
        }

        System.out.println("All classes passed verification.");
    }
}
