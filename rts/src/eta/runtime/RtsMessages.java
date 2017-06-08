package eta.runtime;

import eta.runtime.Rts;
import eta.runtime.RtsFlags;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Task;
import eta.runtime.stg.StgTSO;
import static eta.runtime.Rts.stgExit;
import static eta.runtime.Rts.ExitCode.EXIT_INTERNAL_ERROR;
import static eta.runtime.RtsFlags.progName;
import static eta.runtime.RtsFlags.progArgs;

public class RtsMessages {
    public static void barf(String msg, Object... args) {
        if (RtsFlags.progName != null && RtsFlags.progArgs != null) {
            System.err.print(RtsFlags.progName + ": ");
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
        StgTSO tso = Task.myTask().cap.context.currentTSO;
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
        if (RtsFlags.DebugFlags.scheduler) {
            System.out.print("[Eta-RTS] " + Thread.currentThread() + ": ");
        }
        System.out.format(msg, args);
        System.out.print("\n");
    }

    public static void printClosure(Closure closure) {
        // TODO: Override toString and make the closure more readable
        System.out.println("Closure: " + closure);
    }
}
