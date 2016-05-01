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

    public static void shutdownHaskellAndExit(int exitStatus, boolean faseExit) {}
}
