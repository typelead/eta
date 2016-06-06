package ghcvm.runtime;

import ghcvm.runtime.types.*;

public class RtsStartup {
    private static int hsInitCount;
    public static void hsInit(String[] args, RtsConfig config) {
        hsInitCount++;
        if (hsInitCount > 1) return;

        //setlocale(LC_CTYPE,"");
        RtsStats.startInit();
        // TODO: Implement Stable Ptrs, Globals, File Locking, HPC, IO Manager, Storage, Tracing, processing RTS Flags (is this necessary?)
        RtsScheduler.initScheduler();
        RtsStats.endInit();
    }

    public static void shutdownHaskellAndExit(ExitCode exitStatus, boolean faseExit) {}
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
    }
}
