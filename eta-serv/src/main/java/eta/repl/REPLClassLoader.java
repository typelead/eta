package eta.repl;

import java.net.URL;
import java.net.URLClassLoader;
import java.net.MalformedURLException;
import java.io.File;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Iterator;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.security.ProtectionDomain;
import java.lang.management.ThreadMXBean;
import java.lang.management.ThreadInfo;
import java.lang.management.ManagementFactory;

public class REPLClassLoader extends URLClassLoader {

    static {
        registerAsParallelCapable();
        /* NOTE: Uncomment below if this server process is mysteriously shutting down.
        Runtime.getRuntime().addShutdownHook(new Thread() {
                @Override
                public void run() {
                    System.err.println("SHUTTING DOWN!");
                    ThreadMXBean threadMxBean = ManagementFactory.getThreadMXBean();
                    for (ThreadInfo threadInfo : threadMxBean.dumpAllThreads(true, true)) {
                      System.err.print(threadInfo.toString());
                    }
                }
            });
        */
    }

    private static final REPLClassLoader replClassLoader = new REPLClassLoader();

    private REPLClassLoader() {
        super(new URL[0]);
    }

    @Override
    public Class<?> loadClass(String name) throws ClassNotFoundException {
        synchronized (getClassLoadingLock(name)) {
            Class<?> clazz = findLoadedClass(name);

            if (clazz == null) {
                /* This class loader inverts the normal delegation order by attempting to
                   resolve it first. This is to prevent loading the Eta Runtime library from
                   the system classloader which conflicts with the version eta-serv itself
                   is using. */
                try {
                    clazz = findClass(name);
                } catch (ClassNotFoundException e) {
                    clazz = getParent().loadClass(name);
                }
            }

            return clazz;
        }
    }

    public static void addURLs(String[] paths) throws MalformedURLException {
        for (String path: paths) {
            replClassLoader.addURL(new File(path).toURI().toURL());
        }
    }

    public static void loadClasses(String[] classNames, List<ByteBuffer> classes) {
        ArrayList<Class<?>> newClasses = new ArrayList<Class<?>>(classNames.length);
        Iterator<ByteBuffer> classIt = classes.iterator();

        for (int i = 0; i < classNames.length && classIt.hasNext(); i++) {
            newClasses.add(replClassLoader.defineClass(fixClassName(classNames[i]),
                                                       classIt.next(), (ProtectionDomain)null));
        }

        Iterator<Class<?>> it = newClasses.iterator();

        while (it.hasNext()) {
            replClassLoader.resolveClass(it.next());
        }
    }

    public static Object newInstance(String className) throws ClassNotFoundException, InstantiationException, IllegalAccessException {
        return replClassLoader.loadClass(fixClassName(className)).newInstance();
    }

    private static String fixClassName(String name) {
        return name.replace("/", ".");
    }

    private static Class<?> closureClass;
    private static Class<?> closuresClass;
    private static Class<?> ZCClass;
    private static Field ZCx1Field;
    private static Field ZCx2Field;
    private static Class<?> ZMZNClass;
    private static Method applyMethod;
    private static Method evalIOMethod;

    public static void lazyInit() {
        if (closureClass == null) {
            try {
                closureClass  = replClassLoader.loadClass("eta.runtime.stg.Closure");
                closuresClass = replClassLoader.loadClass("eta.runtime.stg.Closures");
                ZCClass       = replClassLoader.loadClass("ghc_prim.ghc.types.datacons.ZC");
                ZMZNClass     = replClassLoader.loadClass("ghc_prim.ghc.types.datacons.ZMZN");
                ZCx1Field     = ZCClass.getField("x1");
                ZCx2Field     = ZCClass.getField("x2");
                applyMethod   = closuresClass.getMethod("apply", closureClass, closureClass);
                evalIOMethod  = replClassLoader.loadClass("eta.runtime.Runtime").getMethod("evalIO", closureClass);
            } catch (Exception e) {
                throw new RuntimeException("Failed during Eta REPL initialization", e);
            }
        }
    }

    public static Object apply(Object e1, Object e2) {
        lazyInit();
        try {
            return applyMethod.invoke(null, e1, e2);
        } catch (Exception e) {
            throw new RuntimeException
                ("Failed during constructing Eta REPL expression", e);
        }
    }

    public static List<Object> evalStmt(Object e) {
        lazyInit();
        List<Object> list = new LinkedList<Object>();
        try {
            Object result = evalIOInternal(e);
            while (!ZMZNClass.isInstance(result)) {
                list.add(ZCx1Field.get(result));
                result = ZCx2Field.get(result);
            }
            return list;
        } catch (Exception exc) {
            throw new RuntimeException
                ("Failed during evalStmt of Eta REPL expression", exc);
        }
    }

    public static void evalIO(Object e) {
        lazyInit();
        try {
            evalIOInternal(e);
        } catch (Exception exc) {
            exc.printStackTrace();
            throw new RuntimeException
                ("Failed during evalIO of Eta REPL expression", exc);
        }
    }

    private static ByteArrayOutputStream baos;
    private static PrintStream sandboxedStream;

    public static Object evalIOInternal(Object e) {
        // TODO: Add support for stdin too!
        PrintStream oldOut = System.out;
        PrintStream oldErr = System.err;
        initSandbox();
        try {
             return evalIOMethod.invoke(null, e);
        } catch (Exception exc) {
            throw new RuntimeException
                ("Failed during evalIOInternal of Eta REPL expression", exc);
        } finally {
            System.setOut(oldOut);
            System.setErr(oldErr);
        }
    }

    public static byte[] getOutputBytes() {
        byte[] result = baos.toByteArray();
        baos.reset();
        return result;
    }

    private static void initSandbox() {
        if (sandboxedStream == null) {
            baos = new ByteArrayOutputStream();
            sandboxedStream = new PrintStream(baos);
        }
        System.setOut(sandboxedStream);
        System.setErr(sandboxedStream);
    }
}
