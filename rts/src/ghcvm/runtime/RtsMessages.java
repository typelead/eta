package ghcvm.runtime;

import ghcvm.runtime.Rts;
import ghcvm.runtime.RtsFlags;
import ghcvm.runtime.stg.StgClosure;
import static ghcvm.runtime.Rts.stgExit;
import static ghcvm.runtime.Rts.ExitCode.EXIT_INTERNAL_ERROR;
import static ghcvm.runtime.RtsFlags.progName;
import static ghcvm.runtime.RtsFlags.progArgs;

public class RtsMessages {
    public static void barf(String msg, Object... args) {
        if (RtsFlags.progName != null && RtsFlags.progArgs != null) {
            System.err.print(RtsFlags.progName + ": ");
        }
        System.err.print("internal error: ");
        System.err.format(msg, args);
        System.err.print("\n");
        System.err.print("    (GHCVM version " + 1.0 /* TODO: ProjectVersion */ + " for " + "Linux" /* TODO: HostPlatform_TYPE */ + ")\n");
        System.err.println("    Please report this as a GHCVM bug:  https://github.com/rahulmutt/ghcvm/issues");
        stgExit(EXIT_INTERNAL_ERROR);
    }

    public static void errorBelch(String msg, Object... args) {
        if (progName != null) {
            System.err.print(progName + ": ");
        }
        System.err.format(msg, args);
    }

    public static void debugBelch(String msg, Object... args) {
        System.err.format(msg, args);
    }

    public static void printClosure(StgClosure closure) {
        // TODO: Override toString and make the closure more readable
        System.out.println("Closure: " + closure);
    }
}
