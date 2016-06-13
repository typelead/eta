package ghcvm.runtime;

import java.util.concurrent.locks.Lock;

import ghcvm.runtime.types.*;
import ghcvm.runtime.closure.*;
import ghcvm.runtime.stackframe.*;
import static ghcvm.runtime.RtsScheduler.*;
import static ghcvm.runtime.RtsMessages.*;

public class Rts {
    public static ExitCode hsMain(String[] args, StgClosure mainClosure, RtsConfig config) {
        ExitCode exitStatus = ExitCode.EXIT_SUCCESS;

        hsInit(args, config);

        Capability cap = lock();
        HaskellResult result = Rts.evalLazyIO(cap, mainClosure);
        cap = result.cap;
        SchedulerStatus status = cap.getSchedStatus();
        unlock(cap);

        switch (status) {
            case Killed:
                RtsMessages.errorBelch("main thread exited (uncaught exception)");
                exitStatus = ExitCode.EXIT_KILLED;
                break;
            case Interrupted:
                RtsMessages.errorBelch("interrupted");
                exitStatus = ExitCode.EXIT_INTERRUPTED;
                break;
            case HeapExhausted:
                exitStatus = ExitCode.EXIT_HEAPOVERFLOW;
                break;
            case Success:
                exitStatus = ExitCode.EXIT_SUCCESS;
                break;
            default:
                RtsMessages.barf("main thread completed with invalid status");
        }
        shutdownHaskellAndExit(exitStatus, false);
        // This return is never seen since shutdownHaskellAndExit() will
        // terminate the process. It's there to keep javac happy.
        return exitStatus;
    }

    public static Capability lock() {
        Task task = Task.newBoundTask();
        if (task.runningFinalizers) {
            errorBelch("error: a Java finalizer called back into Haskell.\n" +
                       "   To create finalizers that may call back into Haskell, use\n" +
                       "   Foreign.Concurrent.newForeignPtr instead of Foreign.newForeignPtr.");
            stgExit(ExitCode.EXIT_FAILURE);
            return null;
        } else {
            return task.waitForCapability();
        }
    }
    public static void unlock(Capability cap) {
        Task task = cap.runningTask;
        Lock l = cap.lock;
        l.lock();
        try {
            cap.release_(false);
            task.boundTaskExiting();
        } finally {
            l.unlock();
        }
    }
    public static HaskellResult evalLazyIO(Capability cap, StgClosure p) {
        // TODO: Java has a hard-to-get stack size. How do we deal with that?
        StgTSO tso = createIOThread(cap, p);
        return scheduleWaitThread(tso, cap);
    }
    public static SchedulerStatus getSchedStatus(Capability cap) {return null;}
    public static StgTSO createIOThread(Capability cap, StgClosure p) {
        StgTSO t = new StgTSO(cap);
        t.pushClosure(Stg.ap_v);
        t.pushClosure(new StgEnter(p));
        return t;
    }

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

    public static void shutdownHaskellAndExit(ExitCode exitStatus, boolean fastExit) {
        if (!fastExit) {
            hsInitCount = 1;
            hsExit_(false);
        }
        stgExit(exitStatus);
    }

    public static void hsExit_(boolean waitForeign) {
        if (hsInitCount <= 0) {
            errorBelch("warning: too many hs_exits()s");
        } else {
            hsInitCount--;
            if (hsInitCount <= 0) {
                // TODO: Finish up
            }
        }
    }

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

    public void hsExit() {
        hsExit_(true);
    }
}
