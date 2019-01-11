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
import java.lang.reflect.InvocationTargetException;
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

    private static final REPLClassLoader parentReplClassLoader = new REPLClassLoader();
    private static ChildREPLClassLoader replClassLoader = new ChildREPLClassLoader();

    private static final REPLClassLoader classQueryClassLoader = new REPLClassLoader();

    private static class ChildREPLClassLoader extends REPLClassLoader {

        static {
            registerAsParallelCapable();
        }

        public ChildREPLClassLoader() {
            super(parentReplClassLoader);
        }

        @Override
        public Class<?> loadClass(String name) throws ClassNotFoundException {
            synchronized (getClassLoadingLock(name)) {
                Class<?> clazz = findLoadedClass(name);
                if (clazz == null) {
                    try {
                        clazz = getParent().loadClass(name);
                    } catch (ClassNotFoundException e) {
                        clazz = findClass(name);
                    }
                }
                return clazz;
            }
        }

    }


    private REPLClassLoader() {
        super(new URL[0]);
    }

    private REPLClassLoader(ClassLoader parent) {
        super(new URL[0], parent);
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
            parentReplClassLoader.addURL(toClassPathURL(path));
        }
    }

    public static void addChildURLs(String[] paths) throws MalformedURLException {
        for (String path: paths) {
            replClassLoader.addURL(toClassPathURL(path));
        }
    }

    public static void setClassInfoPath(String[] paths) throws MalformedURLException {
        for (String path: paths) {
            classQueryClassLoader.addURL(toClassPathURL(path));
        }
    }

    private static URL toClassPathURL(String path) throws MalformedURLException {
        return new File(path).toURI().toURL();
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

    public static Object newInstance(String className, String methodName)
        throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException,
               IllegalAccessException {
        return replClassLoader.loadClass(fixClassName(className)).getMethod(methodName).invoke(null);
    }

    private static String fixClassName(String name) {
        return name.replace("/", ".");
    }

    public static void resetClasses() {
        replClassLoader = new ChildREPLClassLoader();
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
    private static Method evaluateMethod;
    private static Field Czhx1Field;
    private static Class<?> CzhClass;
    private static Constructor CzhConstructor;
    private static Constructor ZCConstructor;
    private static Class<?> W64zhClass;
    private static Constructor W64zhConstructor;
    private static Object ZMZNSingleton;

    public static void lazyInit() {
        if (closureClass == null) {
            try {
                runtimeClass     = replClassLoader.loadClass("eta.runtime.Runtime");
                closureClass     = replClassLoader.loadClass("eta.runtime.stg.Closure");
                closuresClass    = replClassLoader.loadClass("eta.runtime.stg.Closures");
                ZCClass          = replClassLoader.loadClass("ghc_prim.ghc.types.datacons.ZC");
                ZMZNClass        = replClassLoader.loadClass("ghc_prim.ghc.types.datacons.ZMZN");
                ZCx1Field        = ZCClass.getField("x1");
                ZCx2Field        = ZCClass.getField("x2");
                applyMethod      = closuresClass.getMethod("apply", closureClass, closureClass);
                evalIOMethod     = runtimeClass.getMethod("evalIO", closureClass);
                evaluateMethod   = runtimeClass.getMethod("evaluate", closureClass);
                CzhClass         = replClassLoader.loadClass("ghc_prim.ghc.types.datacons.Czh");
                Czhx1Field       = CzhClass.getField("x1");
                CzhConstructor   = CzhClass.getConstructor(Integer.TYPE);
                ZCConstructor    = ZCClass.getConstructor(closureClass, closureClass);
                W64zhClass       = replClassLoader.loadClass("base.ghc.word.datacons.W64zh");
                W64zhConstructor = W64zhClass.getConstructor(long.class);
                ZMZNSingleton    = replClassLoader.loadClass("ghc_prim.ghc.Types")
                                                  .getMethod("DZMZN").invoke(null);
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

    public static void lazyInitTH() {
        lazyInit();
        if (apply3Method == null) {
            try {
                Class<?> serverClass =
                    replClassLoader.loadClass("eta_meta.language.eta.meta.Server");
                apply3Method =
                    replClassLoader.loadClass("eta.runtime.stg.Closures")
                      .getMethod("apply", closureClass, closureClass,
                                 closureClass, closureClass);
                startTHMethod = serverClass.getMethod("startTH");
                runTHMethod   = serverClass.getMethod("runTH");
                runModFinalizerRefsMethod =
                    serverClass.getMethod("runModFinalizzerRefs");
                jbyteArrayConstructor =
                    replClassLoader
                    .loadClass("ghc_prim.ghc.cstring.datacons.JByteArray")
                    .getConstructor(byte[].class);
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

    public static String evalString(Object e) {
        lazyInit();
        try {
            return convertToString(evalIOInternal(e));
        } catch (Exception exc) {
            throw new RuntimeException
                ("Failed during evalIO of Eta REPL expression", exc);
        }
    }

    public static String evalStringToString(Object e, String str) {
        lazyInit();
        try {
            return convertToString(evalIOInternal(apply(e, convertFromString(str))));
        } catch (Exception exc) {
            throw new RuntimeException
                ("Failed during evalIO of Eta REPL expression", exc);
        }
    }

    private static Object convertFromString(String str) throws
        InstantiationException, IllegalAccessException, InvocationTargetException {
        int off = 0;
        int len = str.length();
        if (len <= 0) return ZMZNSingleton;
        int codepoint = 0;
        Object prevCurrent = null;
        Object current = ZCConstructor.newInstance(null, null);
        Object head = current;
        for (off = 0;
             off < len;
             off += Character.charCount(codepoint)) {
            codepoint = str.codePointAt(off);
            ZCx1Field.set(current, CzhConstructor.newInstance(codepoint));
            Object next = ZCConstructor.newInstance(null, null);
            ZCx2Field.set(current, next);
            prevCurrent = current;
            current = next;
        }
        ZCx2Field.set(prevCurrent, ZMZNSingleton);
        return head;
    }

    private static String convertToString(Object result) throws
        IllegalAccessException, InvocationTargetException {
        StringBuilder sb = new StringBuilder();
        while (!ZMZNClass.isInstance(result = evaluateMethod.invoke(null, result))) {
            sb.appendCodePoint((int) Czhx1Field
                                .get(evaluateMethod
                                    .invoke(null, ZCx1Field.get(result))));
            result = ZCx2Field.get(result);
        }
        return sb.toString();
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
                                           List<Long> qactions) {
        lazyInitTH();
        try {
            Object serialized_ = jbyteArrayConstructor.newInstance(serialized);
            ListIterator<Long> it = qactions.listIterator(qactions.size());
            Object qs = ZMZNSingleton;
            while (it.hasPrevious()) {
                qs = ZCConstructor.newInstance(W64zhConstructor.newInstance(it.previous()), qs);
            }
            evalIOInternal(apply3Method.invoke(null,
                                               runModFinalizerRefsMethod.invoke(null),
                                               serialized_, qstate, qs));
        } catch (Exception exc) {
            throw new RuntimeException("Failed during runModFinalizerRefs", exc);
        }
    }

    public static Class<?> queryClass(String c) throws ClassNotFoundException {
        return Class.forName(c, false, classQueryClassLoader);
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
