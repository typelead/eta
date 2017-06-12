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
       The minimum amount of time (in ms) the runtime should wait to spawn a new worker
       Capabiliity to handle an idle TSO in the Global Run Queue if the
       maxWorkerCapabilities requirement is satisfied. */
    private static int minTSOIdleTime = 20;

    public static int getMinTSOIdleTime() {
        return minTSOIdleTime;
    }

    public static void setMinTSOIdleTime(int newMinTSOIdleTime) {
        minTSOIdleTime = newMinTSOIdleTime;
    }

    /* Parameter: minWorkerCapabilityIdleTime (int)
       The minimum amount of time (in ms) a Capability should stay idle before
       shutting down. */
    private static int minWorkerCapabilityIdleTime = 1000;

    public static int getMinWorkerCapabilityIdleTime() {
        return minWorkerCapabilityIdleTime;
    }

    public static void setMinWorkerCapabilityIdleTime(int newMinWorkerCapabilityIdleTime) {
        minWorkerCapabilityIdleTime = newMinWorkerCapabilityIdleTime;
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

    public static Closure scheduleClosure(Closure p) {
        return Capability.getLocal().schedule(p);
    }

    public static void scheduleThread(Capability cap, TSO tso) {
        cap.appendToRunQueue(tso);
    }

    public static void scheduleThreadOn(Capability cap, int cpu, TSO tso) {
        tso.addFlags(TSO_LOCKED);
        cpu %= Capability.enabledCapabilities;
        if (cpu == cap.no) {
            cap.appendToRunQueue(tso);
        } else {
            cap.migrateThread(tso, Capability.capabilities.get(cpu));
        }
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

    public static void exit_() {
        flushStdHandles();
        Capability.runFinalizers();
    }

    public static void flushStdHandles() {
        evalIO(Closures.flushStdHandles);
    }

    public static void stgExit(ExitCode exitCode) {
        System.exit(exitCode.code());
    }

    public enum ExitCode {
        EXIT_SUCCESS(0),
        EXIT_FAILURE(1),
        EXIT_MISMATCH(63),
        EXIT_SKIP(77),
        EXIT_KILLED(250),
        EXIT_HEAPOVERFLOW(251),
        EXIT_INTERRUPTED(252),
        EXIT_DEADLOCK(253),
        EXIT_INTERNAL_ERROR(254);

        private int code;

        ExitCode(int code) {
            this.code = code;
        }

        public int code() {
            return code;
        }

        public static ExitCode from(int code) {
            switch (code) {
                case 0:
                    return EXIT_SUCCESS;
                case 1:
                    return EXIT_FAILURE;
                case 63:
                    return EXIT_MISMATCH;
                case 77:
                    return EXIT_SKIP;
                case 250:
                    return EXIT_KILLED;
                case 251:
                    return EXIT_HEAPOVERFLOW;
                case 252:
                    return EXIT_INTERRUPTED;
                case 253:
                    return EXIT_DEADLOCK;
                case 254:
                    return EXIT_INTERNAL_ERROR;
                default:
                    throw new IllegalArgumentException("[ExitCode.from(int)] Invalid code: " + code);
            }
        }
    }

    public static void exit() {
        exit_(true);
    }
}
