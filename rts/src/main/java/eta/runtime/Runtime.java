package eta.runtime;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.util.Properties;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.TSO;
import eta.runtime.stg.WeakPtr;
import eta.runtime.io.MemoryManager;
import eta.runtime.exception.RuntimeInternalError;


public class Runtime {

    /**
     * Runtime Parameters
     **/

    public static final String RTS_PROPERTIES_PATH = "eta/rts.properties";

    /* Parameter: maxWorkerCapabilities (int)
       The total number of Capabilities that can be spawned by the runtime itself. */
    private static int maxWorkerCapabilities;

    public static final String MAX_WORKER_CAPABILITIES_PROPERTY = "eta.rts.maxWorkerCapabilities";

    public static int getMaxWorkerCapabilities() {
        return maxWorkerCapabilities;
    }

    public static void setMaxWorkerCapabilities(int newMaxWorkerCapabilities) {
        maxWorkerCapabilities = newMaxWorkerCapabilities;
    }

    /* Parameter: maxGlobalSparks (int)
       The total number of sparks that are allowed in the Global Spark Pool at a time. */
    private static int maxGlobalSparks;

    public static final String MAX_GLOBAL_SPARKS = "eta.rts.maxGlobalSparks";

    public static int getMaxGlobalSparks() {
        return maxGlobalSparks;
    }

    public static volatile boolean parallelClassLoaded = false;

    /* This will NOT affect the spark pool if it's already been initialized! */
    public static void setMaxGlobalSparks(int newMaxGlobalSparks) {
        if (parallelClassLoaded) {
            throw new IllegalStateException("Cannot set the size of the spark pool after it has been initialized!");
        } else {
            maxGlobalSparks = newMaxGlobalSparks;
        }
    }

    /* Parameter: minTSOIdleTime (int)
       The minimum amount of time (in ms) the runtime should wait to spawn a new Worker
       Capabiliity to handle an idle TSO in the Global Run Queue if the
       maxWorkerCapabilities requirement is satisfied. */
    private static int minTSOIdleTime;

    public static final String MIN_TSO_IDLE_TIME = "eta.rts.minTSOIdleTime";

    public static int getMinTSOIdleTime() {
        return minTSOIdleTime;
    }

    public static void setMinTSOIdleTime(int newMinTSOIdleTime) {
        minTSOIdleTime = newMinTSOIdleTime;
    }

    /* Parameter: maxTSOBlockedTime (int)
       The maximum amount of time (in ms) the runtime should wait to stop blocking
       and resume other work when blocked on a given action. */
    private static int maxTSOBlockTime;

    public static final String MAX_TSO_BLOCK_TIME = "eta.rts.maxTSOBlockTime";

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
    private static int minWorkerCapabilityIdleTime;

    public static final String MIN_WORKER_CAPABILITY_IDLE_TIME = "eta.rts.minWorkerCapabilityIdleTime";

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
    private static boolean gcOnWeakPtrFinalization;

    public static final String GC_ON_WEAK_PTR_FINALIZATION = "eta.rts.gcOnWeakPtrFinalization";

    public static boolean shouldGCOnWeakPtrFinalization() {
        return gcOnWeakPtrFinalization;
    }

    public static void setGCOnWeakPtrFinalization(boolean newGCOnWeakPtrFinalization) {
        gcOnWeakPtrFinalization = newGCOnWeakPtrFinalization;
    }

    /* Parameter: maxLocalSparks (int)
       The maximum capacity of the bounded Global Spark Queue.
       */
    private static int maxLocalSparks;

    public static final String MAX_LOCAL_SPARKS = "eta.rts.maxLocalSparks";

    public static int getMaxLocalSparks() {
        return maxLocalSparks;
    }

    public static void setMaxLocalSparks(int newMaxLocalSparks) {
        maxLocalSparks = newMaxLocalSparks;
    }

    /* Parameter: tailCallThreshold (int)
        Threshold for trampoline bouncing
     */
    private static int tailCallThreshold;

    public static final String TAIL_CALL_THRESHOLD = "eta.rts.tailCallThreshold";

    public static int getTailCallThreshold() {
        return tailCallThreshold;
    }

    public static void setTailCallThreshold(int tailCallThreshold) {
        Runtime.tailCallThreshold = tailCallThreshold;
    }

    /* Debug Parameters */
    private static boolean debugScheduler;
    private static boolean debugMemoryManager;

    private static boolean debugSTM;

    public static boolean setDebugMode(char c) {
        boolean valid = true;
        switch (c) {
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

    public static void initializeRuntimeParameters() {
        RuntimeOptions rto = new RuntimeOptions(RTS_PROPERTIES_PATH);
        // Initialize parameters explicitly
        maxWorkerCapabilities = rto.getInt(MAX_WORKER_CAPABILITIES_PROPERTY, getNumberOfProcessors());
        maxGlobalSparks = rto.getInt(MAX_GLOBAL_SPARKS, 4096);
        minTSOIdleTime = rto.getInt(MIN_TSO_IDLE_TIME, 20);
        maxTSOBlockTime = rto.getInt(MAX_TSO_BLOCK_TIME, 1);
        minWorkerCapabilityIdleTime = rto.getInt(MIN_WORKER_CAPABILITY_IDLE_TIME, 1000);
        gcOnWeakPtrFinalization = rto.getBoolean(GC_ON_WEAK_PTR_FINALIZATION, false);
        maxLocalSparks = rto.getInt(MAX_LOCAL_SPARKS, 4096);
        tailCallThreshold = rto.getInt(TAIL_CALL_THRESHOLD, 30);
    }

    static {
        initializeRuntimeParameters();
    }

    public static void main(String[] args, Closure mainClosure) throws Exception {
        Runtime.setProgramArguments(args);
        try {
            evalLazyIO(mainClosure);
        } finally {
            exit();
        }
    }

    public static Closure evalLazyIO(Closure p) throws Exception {
        return Capability.scheduleClosure(Closures.evalLazyIO(p));
    }

    public static Closure evalStableIO(int stablePtr) throws Exception {
        return Capability.scheduleClosure(Closures.evalStableIO(stablePtr));
    }

    public static Closure evalIO(Closure p) throws Exception {
        return Capability.scheduleClosure(Closures.evalIO(p));
    }

    public static Closure evalJava(Object o, Closure p) throws Exception {
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

    public static void shutdownAndExit(int exitCode, boolean fastExit) {
        if (!fastExit) {
            exit();
        }
        stgExit(exitCode);
    }

    public static void exit() {
        maybeFlushStdHandles();
        /* FIXME: For some reason, cleanup() causes bugs in the future invocations
                  of the Eta RTS from the same JVM. */
        // MemoryManager.cleanup();
        WeakPtr.runAllFinalizers();
        Capability.shutdownCapabilities(true);
        /* TODO: Check that all global state is cleaned up.
                 If there are Capabilities that are running,
                 either wait for them to finish or terminate them. */
    }

    public static void maybeFlushStdHandles() {
        try {
            evalIO(Closures.flushStdHandles);
        } catch (Exception e) {
        }
    }

    public static void stgExit(int code) {
        System.exit(code);
    }

    /**
     * Command Line Arguments
     **/

    private static final String RUNTIME_NAMESPACE = "eta.runtime";

    private static String[] programArguments;

    public static String[] getProgramArguments() {
        return programArguments;
    }

    public static void setProgramArguments(String[] newProgramArguments) {
        programArguments = newProgramArguments;
    }

    public static String[] getLocalProgramArguments() {
        Capability cap = Capability.getLocal();
        TSO tso = cap.context.currentTSO;
        Object result = tso.getState(Runtime.RUNTIME_NAMESPACE, "args");
        if (result == null) {
            return getProgramArguments();
        } else {
            return (String[]) result;
        }

    }

    public static void setLocalProgramArguments(String[] newArgs) {
        Capability cap = Capability.getLocal();
        TSO tso = cap.context.currentTSO;
        tso.setState(Runtime.RUNTIME_NAMESPACE, "args", newArgs);
    }

    public static int getNumberOfProcessors() {
        return java.lang.Runtime.getRuntime().availableProcessors();
    }
}
