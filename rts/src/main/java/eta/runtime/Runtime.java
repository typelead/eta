package eta.runtime;


import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import eta.runtime.stg.WeakPtr;

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

    public static long getMinTSOIdleTimeNanos() {
        return maxTSOBlockTime * 1000000L;
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

    /* Parameter: selectorSetSize (int)
        Buffer size for selector thunks
     */
    private static int selectorSetSize;

    public static final String SELECTOR_SET_SIZE = "eta.rts.selectorSetSize";

    public static int getSelectorSetSize() {
        return selectorSetSize;
    }

    public static void setSelectorSetSize(int selectorSetSize) {
        Runtime.selectorSetSize = selectorSetSize;
    }

    /* Parameter: clearThunks (boolean)
       Clear thunks of their free variables once they are evaluated. */
    private static boolean clearThunks;

    public static final String CLEAR_THUNKS = "eta.rts.clearThunks";

    public static boolean shouldClearThunks() {
        return clearThunks;
    }

    public static void setClearThunks(boolean newClearThunks) {
        clearThunks = newClearThunks;
    }

    /* Parameter: keepCAFs (boolean)
       Allow Constant Applicative Forms (CAFs) to be reverted . */
    private static boolean keepCAFs;

    public static final String KEEP_CAFS = "eta.rts.keepCAFs";

    public static boolean shouldKeepCAFs() {
        return keepCAFs;
    }

    public static void setKeepCAFs(boolean newKeepCAFs) {
        keepCAFs = newKeepCAFs;
    }

    /* Debug Parameters */
    private static boolean debugScheduler;

    public static final String DEBUG_SCHEDULER_PROPERTY = "eta.debug.scheduler";

    public static boolean debugScheduler() {
        return debugScheduler;
    }

    private static boolean debugMVar;

    public static final String DEBUG_MVAR_PROPERTY = "eta.debug.mvar";

    public static boolean debugMVar() {
        return debugMVar;
    }

    private static boolean debugSTM;

    public static final String DEBUG_STM_PROPERTY = "eta.debug.stm";

    public static boolean debugSTM() {
        return debugSTM;
    }

    private static boolean debugMemoryManager;

    public static final String DEBUG_MEMORY_MANAGER_PROPERTY = "eta.debug.memoryManager";

    public static boolean debugMemoryManager() {
        return debugMemoryManager;
    }

    private static boolean debugExceptions;

    public static final String DEBUG_EXCEPTIONS_PROPERTY = "eta.debug.exceptions";

    public static boolean debugExceptions() {
        return debugExceptions;
    }

    private static boolean debugExceptionsVerbose;

    public static final String DEBUG_EXCEPTIONSVERBOSE_PROPERTY = "eta.debug.exceptionsVerbose";

    public static boolean debugExceptionsVerbose() {
        return debugExceptionsVerbose;
    }

    private static boolean debugAsyncExceptions;

    public static final String DEBUG_ASYNCEXCEPTIONS_PROPERTY = "eta.debug.asyncExceptions";

    public static boolean debugAsyncExceptions() {
        return debugAsyncExceptions;
    }

    private static boolean debugAsyncExceptionsVerbose;

    public static final String DEBUG_ASYNCEXCEPTIONSVERBOSE_PROPERTY = "eta.debug.asyncExceptionsVerbose";

    public static boolean debugAsyncExceptionsVerbose() {
        return debugAsyncExceptionsVerbose;
    }

    private static boolean debugStablePtr;

    public static final String DEBUG_STABLEPTR_PROPERTY = "eta.debug.stablePtr";

    public static boolean debugStablePtr() {
        return debugStablePtr;
    }

    private static boolean debugTailCalls;

    public static final String DEBUG_TAILCALLS_PROPERTY = "eta.debug.tailCalls";

    public static boolean debugTailCalls() {
        return debugTailCalls;
    }

    private static boolean debugIO;

    public static final String DEBUG_IO_PROPERTY = "eta.debug.io";

    public static boolean debugIO() {
        return debugIO;
    }

    private static boolean debugIOVerbose;

    public static final String DEBUG_IOVERBOSE_PROPERTY = "eta.debug.ioVerbose";

    public static boolean debugIOVerbose() {
        return debugIOVerbose;
    }

    private static boolean debugPAPs;

    public static final String DEBUG_PAPS_PROPERTY = "eta.debug.PAPs";

    public static boolean debugPAPs() {
        return debugPAPs;
    }

    private static boolean debugStrings;

    public static final String DEBUG_STRINGS_PROPERTY = "eta.debug.strings";

    public static boolean debugStrings() {
        return debugStrings;
    }

    private static boolean debugSelectors;

    public static final String DEBUG_SELECTORS_PROPERTY = "eta.debug.selectors";

    public static boolean debugSelectors() {
        return debugSelectors;
    }

    private static boolean debugToFile;

    public static final String DEBUG_TOFILE_PROPERTY = "eta.debug.toFile";

    public static boolean debugToFile() {
        return debugToFile;
    }

    /* Print Parameters */

    private static boolean printFullArrays;

    public static final String PRINT_FULLARRAYS_PROPERTY = "eta.print.fullArrays";

    public static boolean printFullArrays() {
        return printFullArrays;
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
        selectorSetSize = rto.getInt(SELECTOR_SET_SIZE, 128);
        tailCallThreshold = rto.getInt(TAIL_CALL_THRESHOLD, 400);
        // happy requires 650.
        // alex requires 400.
        // Note that we can further increase the value if we optimize
        // our local variables better, we should be able to increase this number later.
        clearThunks = rto.getBoolean(CLEAR_THUNKS, true);
        keepCAFs = rto.getBoolean(KEEP_CAFS, false);

        debugScheduler = rto.getBoolean(DEBUG_SCHEDULER_PROPERTY, false);
        debugMVar = rto.getBoolean(DEBUG_MVAR_PROPERTY, false);
        debugSTM = rto.getBoolean(DEBUG_STM_PROPERTY, false);
        debugMemoryManager = rto.getBoolean(DEBUG_MEMORY_MANAGER_PROPERTY, false);
        debugExceptions = rto.getBoolean(DEBUG_EXCEPTIONS_PROPERTY, false);
        debugExceptionsVerbose = rto.getBoolean(DEBUG_EXCEPTIONSVERBOSE_PROPERTY, false);
        debugAsyncExceptions = rto.getBoolean(DEBUG_ASYNCEXCEPTIONS_PROPERTY, false);
        debugAsyncExceptionsVerbose = rto.getBoolean(DEBUG_ASYNCEXCEPTIONSVERBOSE_PROPERTY, false);
        debugStablePtr = rto.getBoolean(DEBUG_STABLEPTR_PROPERTY, false);
        debugTailCalls = rto.getBoolean(DEBUG_TAILCALLS_PROPERTY, false);
        debugIO = rto.getBoolean(DEBUG_IO_PROPERTY, false);
        debugIOVerbose = rto.getBoolean(DEBUG_IOVERBOSE_PROPERTY, false);
        debugPAPs = rto.getBoolean(DEBUG_PAPS_PROPERTY, false);
        debugStrings = rto.getBoolean(DEBUG_STRINGS_PROPERTY, false);
        debugSelectors = rto.getBoolean(DEBUG_SELECTORS_PROPERTY, false);
        debugToFile = rto.getBoolean(DEBUG_TOFILE_PROPERTY, false);

        printFullArrays = rto.getBoolean(PRINT_FULLARRAYS_PROPERTY, false);
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

    public static Closure evaluate(Closure p) {
        return p.evaluate(StgContext.acquire());
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
    private static final String DEFAULT_PROGRAM_NAME = "program";
    private static final String ETA_PROGRAM_NAME_PROPERTY = "eta.programName";

    private static String programName;

    static {
        /* We cache this at class load-time to make sure side effects via
           System.setProperty don't affect the default value. */
        String progName = System.getProperty(ETA_PROGRAM_NAME_PROPERTY);
        if (progName == null) {
            progName = DEFAULT_PROGRAM_NAME;
        }
        setProgramName(progName);
    }


    private static String[] programArguments;

    public static String[] getProgramArguments() {
        return programArguments;
    }

    public static void setProgramArguments(final String[] newProgramArguments) {
        programArguments = newProgramArguments;
    }

    public static String[] getLocalProgramArguments() {
        final TSO tso = Capability.getLocal().getTSO();
        final Object result = tso.getState(Runtime.RUNTIME_NAMESPACE, "args");
        if (result == null) {
            return getProgramArguments();
        } else {
            return (String[]) result;
        }

    }

    public static void setLocalProgramArguments(final String[] newArgs) {
        Capability.getLocal().getTSO().setState(Runtime.RUNTIME_NAMESPACE, "args", newArgs);
    }

    public static String getProgramName() {
        return programName;
    }

    public static void setProgramName(final String newProgramName) {
        programName = newProgramName;
    }

    public static String getLocalProgramName() {
        final TSO tso = Capability.getLocal().getTSO();
        final Object result = tso.getState(Runtime.RUNTIME_NAMESPACE, "progName");
        if (result == null) {
            return getProgramName();
        } else {
            return (String) result;
        }
    }

    public static void setLocalProgramName(final String progName) {
        Capability.getLocal().getTSO()
            .setState(Runtime.RUNTIME_NAMESPACE, "progName", progName);
    }

    public static int getNumberOfProcessors() {
        return java.lang.Runtime.getRuntime().availableProcessors();
    }
}
