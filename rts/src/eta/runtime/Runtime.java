package eta.runtime;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.TSO;
import eta.runtime.stg.WeakPtr;
import eta.runtime.io.MemoryManager;
import eta.runtime.exception.RuntimeInternalError;

public class Runtime {

    /** Runtime Parameters **/

    /* Parameter: maxWorkerCapabilities (int)
       The total number of Capabilities that can be spawned by the runtime itself. */
    private static int maxWorkerCapabilities
        = 2 * RuntimeOptions.getNumberOfProcessors() + 1;

    public static int getMaxWorkerCapabilities() {
        return maxWorkerCapabilities;
    }

    public static void setMaxWorkerCapabilities(int newMaxWorkerCapabilities) {
        maxWorkerCapabilities = newMaxWorkerCapabilities;
    }

    /* Parameter: maxGlobalSparks (int)
       The total number of sparks that are allowed in the Global Spark Pool at a time. */
    private static int maxGlobalSparks = 4096;

    public static int getMaxGlobalSparks() {
        return maxGlobalSparks;
    }

    private static Method findLoadedClass;
    static {
        try {
            findLoadedClass = ClassLoader.class
                                .getDeclaredMethod("findLoadedClass"
                                                  ,new Class[] { String.class });
            findLoadedClass.setAccessible(true);
        } catch(Exception e) {
            findLoadedClass = null;
        }
    }

    /* This will NOT affect the spark pool if it's already been initialized! */
    public static void setMaxGlobalSparks(int newMaxGlobalSparks) {
        java.lang.Exception failed = null;
        try {
            if(findLoadedClass != null &&
               findLoadedClass.invoke(ClassLoader.getSystemClassLoader()
                                     ,"eta.runtime.parallel.Parallel") != null) {
                /* TODO: Should we indicate to the user that it's already initialized? */
                return;
            }
            maxGlobalSparks = newMaxGlobalSparks;
        } catch (IllegalAccessException e) {
            throw new RuntimeInternalError("eta.runtime.parallel.Parallel is not accessible!");
        } catch (InvocationTargetException e) {
            throw new RuntimeInternalError("ClassLoader.findLoadedClass() does not exist!");
        }
    }

    /* Parameter: minTSOIdleTime (int)
       The minimum amount of time (in ms) the runtime should wait to spawn a new Worker
       Capabiliity to handle an idle TSO in the Global Run Queue if the
       maxWorkerCapabilities requirement is satisfied. */
    private static int minTSOIdleTime = 20;

    public static int getMinTSOIdleTime() {
        return minTSOIdleTime;
    }

    public static void setMinTSOIdleTime(int newMinTSOIdleTime) {
        minTSOIdleTime = newMinTSOIdleTime;
    }

    /* Parameter: maxTSOBlockedTime (int)
       The maximum amount of time (in ms) the runtime should wait to stop blocking
       and resume other work when blocked on a given action. */
    private static int maxTSOBlockTime = 1;

    public static int getMaxTSOBlockTime() {
        return maxTSOBlockTime;
    }

    public static long getMaxTSOBlockTimeNanos() {
        return maxTSOBlockTime * 1000000L;
    }

    public static void setMaxTSOBlockTime(int newMaxTSOBlockTime) {
        maxTSOBlockTime = newMaxTSOBlockTime;
    }

    /* Parameter: minWorkerCapabilityIdleTime (int)
       The minimum amount of time (in ms) a Capability should stay idle before
       shutting down. */
    private static int minWorkerCapabilityIdleTime = 1000;

    public static int getMinWorkerCapabilityIdleTime() {
        return minWorkerCapabilityIdleTime;
    }

    public static long getMinWorkerCapabilityIdleTimeNanos() {
        return minWorkerCapabilityIdleTime * 1000000L;
    }

    public static void setMinWorkerCapabilityIdleTime(int newMinWorkerCapabilityIdleTime) {
        minWorkerCapabilityIdleTime = newMinWorkerCapabilityIdleTime;
    }

    /* Parameter: gcOnWeakPtrFinalization (boolean)
       Should System.gc() be called when finalizing WeakPtrs since their value
       references will be nulled. Default: False to avoid unnecessary GC overhead. */
    private static boolean gcOnWeakPtrFinalization = false;

    public static boolean shouldGCOnWeakPtrFinalization() {
        return gcOnWeakPtrFinalization;
    }

    public static void setGCOnWeakPtrFinalization(boolean newGCOnWeakPtrFinalization) {
        gcOnWeakPtrFinalization = newGCOnWeakPtrFinalization;
    }

    /* Parameter: maxLocalSparks (int)
       The maximum capacity of the bounded Global Spark Queue.
       */
    private static int maxLocalSparks = 4096;

    public static int getMaxLocalSparks() {
        return maxLocalSparks;
    }

    public static void setMaxLocalSparks(int newMaxLocalSparks) {
        maxLocalSparks = newMaxLocalSparks;
    }

    /* Debug Parameters */
    private static boolean debugScheduler;
    private static boolean debugMemoryManager;
    private static boolean debugSTM;

    public static boolean setDebugMode(char c) {
        boolean valid = true;
        switch(c) {
            case 's':
                debugScheduler = true;
                break;
            case 'm':
                debugMemoryManager = true;
                break;
            case 't':
                debugSTM = true;
                break;
            default:
                valid = false;
                break;
        }
        return valid;
    }

    public static boolean debugScheduler() {
        return debugScheduler;
    }

    public static boolean debugSTM() {
        return debugSTM;
    }

    public static boolean debugMemoryManager() {
        return debugMemoryManager;
    }

    public static void main(String[] args, Closure mainClosure) {
        RuntimeOptions.parse(args);
        evalLazyIO(mainClosure);
        exit();
    }

    public static Closure evalLazyIO(Closure p) {
        return Capability.scheduleClosure(Closures.evalLazyIO(p));
    }

    public static Closure evalIO(Closure p) {
        return Capability.scheduleClosure(Closures.evalIO(p));
    }

    public static Closure evalJava(Object o, Closure p) {
        return Capability.scheduleClosure(Closures.evalJava(o, p));
    }

    public static TSO createIOThread(Closure p) {
        return new TSO(Closures.evalLazyIO(p));
    }

    public static TSO createStrictIOThread(Closure p) {
        return new TSO(Closures.evalIO(p));
    }

    public static TSO createStrictJavaThread(Object thisObj, Closure p) {
        return new TSO(Closures.evalJava(thisObj, p));
    }

    public static void shutdownAndExit(int exitCode, boolean fastExit, boolean hardExit) {
        if (!fastExit) {
            exit();
        }
        if (exitCode != 0 || hardExit) stgExit(exitCode);
    }

    public static void shutdownAndSignal(int signal, boolean fastExit) {
        if (!fastExit) {
            exit();
        }
        // TODO: Implement signals
        stgExit(1);
    }

    public static void exit() {
        evalIO(Closures.flushStdHandles);
        /* FIXME: For some reason, cleanup() causes bugs in the future invocations
                  of the Eta RTS from the same JVM. */
        // MemoryManager.cleanup();
        WeakPtr.runAllFinalizers();
        /* TODO: Check that all global state is cleaned up.
                 If there are Capabilities that are running,
                 either wait for them to finish or terminate them. */
    }

    public static void stgExit(int code) {
        System.exit(code);
    }
}
