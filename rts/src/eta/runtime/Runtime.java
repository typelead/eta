package eta.runtime;

import java.util.List;
import java.util.concurrent.locks.Lock;

import eta.runtime.stg.Task;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closures;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.ForceIO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.WeakPtr;
import eta.runtime.apply.ApV;
import eta.runtime.apply.ApO;
import static eta.runtime.RuntimeLogging.errorBelch;
import static eta.runtime.RuntimeLogging.debugBelch;
import static eta.runtime.stg.TSO.TSO_LOCKED;
import static eta.runtime.stg.TSO.TSO_BLOCKEX;
import static eta.runtime.stg.TSO.TSO_INTERRUPTIBLE;

public class Runtime {

    /** Runtime Parameters **/

    /* Parameter: maxWorkerCapabilities (int)
       The total number of Capabilities that can be spawned by the runtime itself. */
    private static int maxWorkerCapabilities = 2 * getNumberOfProcessors() + 1;

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

    Method findLoadedClass;
    static {
        try {
            findLoadedClass = ClassLoader.class
                                .getDeclaredMethod( "findLoadedClass"
                                                  , new Class[] { String.class });
            findLoadedClass.setAccessible(true);
        } catch(Exception e) {
            findLoadedClass = null;
        }
    }

    /* This will NOT affect the spark pool if it's already been initialized! */
    public static void setMaxGlobalSparks(int newMaxGlobalSparks) {
        if(findLoadedClass != null &&
           findLoadedClass.invoke( ClassLoader.getSystemClassLoader()
                                 , "eta.runtime.parallel.Parallel") != null) {
            /* TODO: Replace with custom exception */
            throw new Exception("eta.runtime.parallel.Parallel has already been initialized!");
        }
        maxGlobalSparks = newMaxGlobalSparks;
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

    public static int getMinWorkerCapabilityIdleTimeNanos() {
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

    /* Debug Parameters */
    private static boolean debugScheduler;

    public void setDebugMode(char c) {
        switch(c) {
          case 's':
              debugScheduler = true;
        }
    }


    public static void main(String[] args, Closure mainClosure) {
        RuntimeOptions.parse(args);
        evalLazyIO(cap, mainClosure);
        exit();
    }

    public static void unlock(Capability cap) {
        Task task = cap.runningTask;
        Lock l = cap.lock;
        l.lock();
        try {
            cap.release_(false);
            task.boundTaskExiting();
        } finally {
            l.unlock();
        }
    }

    public static Closure evalLazyIO(Closure p) {
        return scheduleClosure(Closures.evalLazyIO(p));
    }

    public static Closure evalIO(Closure p) {
        return scheduleClosure(Closures.evalIO(p));
    }

    public static Closure evalJava(Object o, Closure p) {
        return scheduleClosure(Closures.evalJava(o, p));
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

    public static void shutdownAndExit(ExitCode exitStatus, boolean fastExit, boolean hardExit) {
        if (!fastExit) {
            exit_();
        }
        if (exitStatus != ExitCode.EXIT_SUCCESS || hardExit) stgExit(exitStatus);
    }

    public static void shutdownAndSignal(int signal, boolean fastExit) {
        if (!fastExit) {
            exit_(false);
        }
        // TODO: Implement signals
        stgExit(ExitCode.EXIT_KILLED);
    }

    public static void exit() {
        flushStdHandles();
        Capability.runFinalizers();
    }

    public static void flushStdHandles() {
        evalIO(Closures.flushStdHandles);
    }

    public static void stgExit(int code) {
        System.exit();
    }
}
