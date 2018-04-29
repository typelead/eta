package eta.runtime;

import eta.runtime.Runtime;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.TSO;
import eta.runtime.exception.RuntimeInternalError;

import java.util.concurrent.atomic.AtomicBoolean;

public class RuntimeLogging {
    public static final AtomicBoolean errorLock = new AtomicBoolean();

    public static void barf(String msg, Object... args) {
        throw new RuntimeInternalError(String.format(msg, args));
    }

    public static void errorBelch(String msg, Object... args) {
        StringBuilder sw = new StringBuilder();
        sw.append("Exception in thread \""
                  + Thread.currentThread().getName()
                  + "\" eta.runtime.exception.EtaException: ");
        sw.append(String.format(msg, args));
        sw.append("\n");
        TSO tso = Capability.getLocal().context.currentTSO;
        if (tso.hasStackTrace()) {
            StackTraceElement[] stackTrace = tso.getStackTrace();
            for (StackTraceElement element : stackTrace) {
                String className = element.getClassName();
                String sourceFile = element.getFileName();
                if (sourceFile != null) {
                    sourceFile = "(" + sourceFile + ":" + element.getLineNumber() + ")";
                } else {
                    sourceFile = "";
                }
                sw.append("    at " + element.getClassName() + "."
                                    + element.getMethodName()
                                    + sourceFile
                                    + "\n");
            }
            tso.resetStack();
            try {
                while (!errorLock.compareAndSet(false, true));
                System.err.print(sw.toString());
            } finally {
                errorLock.set(false);
            }
        }
    }

    public static void debugBelch(String msg, Object... args) {
        System.out.format(msg, args);
        System.out.print("\n");
    }

    public static void debugScheduler(String msg) {
        debugGeneric("Scheduler", msg);
    }

    public static void debugMVar(String msg) {
        debugGeneric("MVar", msg);
    }

    public static void debugSTM(String msg) {
        debugGeneric("STM", msg);
    }

    public static void debugMemoryManager(String msg) {
        debugGeneric("MemoryManager", msg);
    }

    public static void debugGeneric(String type, String msg) {
        debugBelch("[Eta-RTS](%s){%s}: %s", Capability.getLocal(), type, msg);
    }
}
