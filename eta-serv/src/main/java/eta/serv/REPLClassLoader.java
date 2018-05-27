package eta.serv;

import java.net.URL;
import java.net.URLClassLoader;
import java.net.MalformedURLException;
import java.io.File;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channel;
import java.nio.channels.Channels;
import java.util.List;
import java.util.ListIterator;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Iterator;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.security.ProtectionDomain;
// import java.lang.management.ThreadMXBean;
// import java.lang.management.ThreadInfo;
// import java.lang.management.ManagementFactory;

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

    private static Class<?> runtimeClass;
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
                runtimeClass  = replClassLoader.loadClass("eta.runtime.Runtime");
                closureClass  = replClassLoader.loadClass("eta.runtime.stg.Closure");
                closuresClass = replClassLoader.loadClass("eta.runtime.stg.Closures");
                ZCClass       = replClassLoader.loadClass("ghc_prim.ghc.types.datacons.ZC");
                ZMZNClass     = replClassLoader.loadClass("ghc_prim.ghc.types.datacons.ZMZN");
                ZCx1Field     = ZCClass.getField("x1");
                ZCx2Field     = ZCClass.getField("x2");
                applyMethod   = closuresClass.getMethod("apply", closureClass, closureClass);
                evalIOMethod  = runtimeClass.getMethod("evalIO", closureClass);
            } catch (Exception e) {
                throw new RuntimeException("Failed during Eta REPL initialization", e);
            }
        }
    }

    private static Method apply3Method;
    private static Method startTHMethod;
    private static Method runTHMethod;
    private static Method runModFinalizerRefsMethod;
    private static Constructor jbyteArrayConstructor;
    private static Constructor ZCConstructor;
    private static Method DZMZNMethod;

    public static void lazyInitTH() {
        lazyInit();
        if (apply3Method == null) {
            try {
                Class<?> serverClass =
                    replClassLoader.loadClass("eta_meta.language.eta.meta.Server");
                apply3Method =
                    replClassLoader.loadClass("eta.runtime.stg.Closures")
                      .getMethod("apply", closureClass, closureClass, closureClass,
                                 closureClass);
                startTHMethod = serverClass.getMethod("startTH");
                runTHMethod   = serverClass.getMethod("runTH");
                runModFinalizerRefsMethod =
                    serverClass.getMethod("runModFinalizzerRefs");
                jbyteArrayConstructor =
                    replClassLoader
                    .loadClass("ghc_prim.ghc.cstring.datacons.JByteArray")
                    .getConstructor(byte[].class);
                ZCConstructor = ZCClass.getConstructor(closureClass, closureClass);
                DZMZNMethod = replClassLoader.loadClass("ghc_prim.ghc.Types")
                                .getMethod("DZMZN");
            } catch (Exception e) {
                throw new RuntimeException("Failed during Eta REPL TH initialization", e);
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
            throw new RuntimeException
                ("Failed during evalIO of Eta REPL expression", exc);
        }
    }

    public static Object startTH() {
        lazyInitTH();
        try {
            return evalIOInternal(startTHMethod.invoke(null));
        } catch (Exception exc) {
            throw new RuntimeException("Failed during startTH", exc);
        }
    }

    public static void runTH(Object qstate, Object q, byte[] serialized) {
        lazyInitTH();
        try {
            Object serialized_ = jbyteArrayConstructor.newInstance(serialized);
            evalIOInternal(apply3Method.invoke(null, runTHMethod.invoke(null),
                                               qstate, q, serialized_));
        } catch (Exception exc) {
            throw new RuntimeException("Failed during runTH", exc);
        }
    }

    public static void runModFinalizerRefs(byte[] serialized, Object qstate,
                                           List<Object> qactions) {
        lazyInitTH();
        try {
            Object serialized_ = jbyteArrayConstructor.newInstance(serialized);
            ListIterator<Object> it = qactions.listIterator(qactions.size());
            Object qs = DZMZNMethod.invoke(null);
            while (it.hasPrevious()) {
                qs = ZCConstructor.newInstance(it.previous(), qs);
            }
            evalIOInternal(apply3Method.invoke(null,
                                               runModFinalizerRefsMethod.invoke(null),
                                               serialized_, qstate, qs));
        } catch (Exception exc) {
            throw new RuntimeException("Failed during runModFinalizerRefs", exc);
        }
    }

    private static ByteArrayOutputStream baos;
    private static PrintStream sandboxedStream;

    private static InputStream oldStdIn;
    private static PrintStream oldStdOut;
    private static PrintStream oldStdErr;

    public static Object evalIOInternal(Object e) {
        // TODO: Add support for stdin too!
        oldStdIn  = System.in;
        oldStdOut = System.out;
        oldStdErr = System.err;
        initSandbox();
        try {
             return evalIOMethod.invoke(null, e);
        } catch (Exception exc) {
            throw new RuntimeException
                ("Failed during evalIOInternal of Eta REPL expression", exc);
        } finally {
            System.setOut(oldStdOut);
            System.setErr(oldStdErr);
        }
    }

    // TODO: Currently, stdin isn't sandboxed, so this will return the default stdin.
    public static Channel getOldStdIn() {
        return Channels.newChannel(oldStdIn);
    }

    public static Channel getOldStdOut() {
        return Channels.newChannel(oldStdOut);
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
