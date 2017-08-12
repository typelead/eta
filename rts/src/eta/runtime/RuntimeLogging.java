package eta.runtime;

import eta.runtime.Runtime;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.TSO;
import eta.runtime.exception.RuntimeInternalError;

public class RuntimeLogging {
    public static void barf(String msg, Object... args) {
        throw new RuntimeInternalError(String.format(msg, args));
    }

    public static void errorBelch(String msg, Object... args) {
        System.err.print("***Exception***: ");
        System.err.format(msg, args);
        System.err.print("\n");
        TSO tso = Capability.getLocal().context.currentTSO;
        if (tso.hasStackTrace()) {
            StackTraceElement[] stackTrace = tso.getStackTrace();
            for (StackTraceElement element : stackTrace) {
                String className = element.getClassName();
                if (!className.startsWith("eta.runtime") && !className.startsWith("java")) {
                    System.err.println("    in " + element.getClassName());
                }
            }
            tso.setStackTrace(null);
        }
    }

    public static void debugBelch(String msg, Object... args) {
        System.out.format(msg, args);
        System.out.print("\n");
    }

    public static void debugScheduler(String msg, Object... args) {
        debugGeneric(Runtime.debugScheduler(), "Scheduler", msg, args);
    }

    public static void debugSTM(String msg, Object... args) {
        debugGeneric(Runtime.debugSTM(), "STM", msg, args);
    }

    public static void debugMemoryManager(String msg, Object... args) {
        debugGeneric(Runtime.debugMemoryManager(), "MemoryManager", msg, args);
    }

    public static void debugGeneric(boolean shouldDebug, String type, String msg, Object... args) {
        if (shouldDebug) {
            Capability cap = Capability.getLocal();
            String worker = cap.worker? "[Worker]" : "";
            debugBelch("[Eta-RTS](Capability" + worker + " %d){" + type + "}: " + msg,
                       cap.id, args);
        }
    }

    public static void printClosure(Closure closure) {
        // TODO: Override toString and make the closure more readable
        System.out.println("Closure: " + closure);
    }
}
