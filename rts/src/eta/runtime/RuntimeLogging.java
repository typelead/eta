package eta.runtime;

import eta.Runtime;
import eta.runtime.RuntimeOptions;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Task;
import eta.runtime.stg.TSO;
import static eta.runtime.Runtime.stgExit;
import static eta.runtime.Runtime.ExitCode.EXIT_INTERNAL_ERROR;
import static eta.runtime.RuntimeOptions.progName;
import static eta.runtime.RuntimeOptions.progArgs;

public class RuntimeLogging {
    public static void barf(String msg, Object... args) {
        if (RuntimeOptions.progName != null && RuntimeOptions.progArgs != null) {
            System.err.print(RuntimeOptions.progName + ": ");
        }
        System.err.print("internal error: ");
        System.err.format(msg, args);
        System.err.print("\n");
        // @VERSION_CHANGE@
        System.err.print("    (Eta version 0.0.6" /* TODO: ProjectVersion */ + ")\n");
        System.err.println("    Please report this as a Eta bug:  https://github.com/typelead/eta/issues");
        Thread.dumpStack();
        stgExit(EXIT_INTERNAL_ERROR);
    }

    public static void errorBelch(String msg, Object... args) {
        if (progName != null) {
            System.err.print(progName + ": ");
        }
        System.err.print("***Exception***: ");
        System.err.format(msg, args);
        System.err.print("\n");
        TSO tso = Task.myTask().cap.context.currentTSO;
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
        if (RuntimeOptions.DebugFlags.scheduler) {
            System.out.print("[Eta-RTS] " + Thread.currentThread() + ": ");
        }
        System.out.format(msg, args);
        System.out.print("\n");
    }

    public static void debugScheduler(String msg, Object... args) {
        debugGeneric("Scheduler", msg, args);
    }

    public static debugGeneric(String type, String msg, Object... args) {
        debugBelch("[Eta-RTS](Task %d){" + type + "}: " + msg,
                   Thread.currentThread().getId(), args);
    }

    public static void debugSTM(String msg, Object... args) {
        debugGeneric("STM", msg, args);
    }

    public static void printClosure(Closure closure) {
        // TODO: Override toString and make the closure more readable
        System.out.println("Closure: " + closure);
    }
}
