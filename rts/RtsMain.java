package ghcvm.runtime;

import ghcvm.runtime.SchedulerStatus;
import ghcvm.runtime.RtsConstants;
import ghcvm.runtime.RtsStartup;
import ghcvm.runtime.RtsConfig;
import ghcvm.runtime.Capability;
import ghcvm.runtime.Ptr;

public class RtsMain {
    public static int hsMain(String[] args, CLOSURE_PTR mainClosure, RtsConfig config) {
        int exitStatus = 0;
        SchedulerStatus status = null;
        Ptr<Capability> capPtr = null;

        RtsStartup.hsInit(args, config);

        Capability cap = Rts.lock();
        capPtr = new Ptr<Capability>(cap);
        Rts.evalLazyIO(capPtr, mainClosure, null);
        status = Rts.getSchedStatus(cap);
        Rts.unlock(cap);

        switch (status) {
            case Killed:
                RtsMessages.errorBelch("main thread exited (uncaught exception)");
                exitStatus = RtsConstants.EXIT_KILLED;
                break;
            case Interrupted:
                RtsMessages.errorBelch("interrupted");
                exitStatus = RtsConstants.EXIT_INTERRUPTED;
                break;
            case HeapExhausted:
                exitStatus = RtsConstants.EXIT_HEAPOVERFLOW;
                break;
            case Success:
                exitStatus = RtsConstants.EXIT_SUCCESS;
                break;
            default:
                RtsMessages.barf("main thread completed with invalid status");
        }
        RtsStartup.shutdownHaskellAndExit(exitStatus, false);
        // This return is never seen since shutdownHaskellAndExit() will
        // terminate the process. It's there to keep javac happy.
        return exitStatus;
    }
}
