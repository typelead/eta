import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
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
        long inputsFailed = 0;
        for (String pathToJar: args) {
            JarFile jarFile = new JarFile(pathToJar);
            Enumeration<JarEntry> e = jarFile.entries();

            URL[] urls = { new URL("jar:file:" + pathToJar+"!/") };
            URLClassLoader cl = URLClassLoader.newInstance(urls);
            Map<String, Throwable> failed = new HashMap<String, Throwable>();
            int numClasses = 0;

            while (e.hasMoreElements()) {
                JarEntry je = e.nextElement();
                if(je.isDirectory() || !je.getName().endsWith(".class")){
                    continue;
                }
                // -6 because of .class
                String className = je.getName().substring(0,je.getName().length()-6);
                className = className.replace('/', '.');
                try {
                    Class.forName(className, true, cl);
                } catch (Throwable t) {
                    failed.put(className, t);
                }

                numClasses++;
            }

            int numFailed = failed.size();
            int passed = numClasses - numFailed;

            System.out.println("[" + passed + " of " + numClasses + "] Generated classes passed verification. ( "  + pathToJar + " )");
            if (numFailed > 0) {
                System.err.println("The following " + (numFailed > 1? numFailed + " classes" : "class") +  " failed to verify:");
                for (Map.Entry<String, Throwable> entry : failed.entrySet()) {
                    System.err.println("- " + entry.getKey());
                    entry.getValue().printStackTrace();
                }
                inputsFailed++;
            }
        }

        if (inputsFailed > 0) {
            System.exit(1);
        }
    }
}
