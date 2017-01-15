package eta.runtime;

import java.util.List;
import java.util.concurrent.locks.Lock;

import eta.runtime.stg.Task;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.ForceIO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgWeak;
import eta.runtime.apply.ApV;
import eta.runtime.apply.ApO;
import static eta.runtime.RtsScheduler.SchedulerStatus;
import static eta.runtime.RtsScheduler.scheduleWaitThread;
import static eta.runtime.RtsMessages.errorBelch;
import static eta.runtime.RtsMessages.debugBelch;
import static eta.runtime.stg.StgTSO.TSO_LOCKED;

public class Rts {
    public static class HaskellResult {
        public final Capability cap;
        public final StgClosure result;

        public HaskellResult(final Capability cap, final StgClosure result) {
            this.cap = cap;
            this.result = result;
        }
    }

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

    public static HaskellResult evalIO(Capability cap, StgClosure p) {
        // TODO: Java has a hard-to-get stack size. How do we deal with that?
        StgTSO tso = createStrictIOThread(cap, p);
        return scheduleWaitThread(tso, cap);
    }

    public static HaskellResult evalJava(Capability cap, Object o, StgClosure p) {
        // TODO: Java has a hard-to-get stack size. How do we deal with that?
        StgTSO tso = createStrictJavaThread(cap, o, p);
        return scheduleWaitThread(tso, cap);
    }

    public static SchedulerStatus getSchedStatus(Capability cap) {return null;}

    public static StgTSO createIOThread(Capability cap, StgClosure p) {
        StgTSO t = new StgTSO(cap);
        t.pushClosure(new ApV());
        t.pushClosure(new StgEnter(p));
        return t;
    }

    public static StgTSO createStrictIOThread(Capability cap, StgClosure p) {
        StgTSO t = new StgTSO(cap);
        t.pushClosure(new ForceIO());
        t.pushClosure(new ApV());
        t.pushClosure(new StgEnter(p));
        return t;
    }

    public static StgTSO createStrictJavaThread(Capability cap, Object thisObj, StgClosure p) {
        StgTSO t = new StgTSO(cap);
        t.pushClosure(new ForceIO());
        t.pushClosure(new ApO(thisObj));
        t.pushClosure(new StgEnter(p));
        return t;
    }

    private static int hsInitCount;
    public static void hsInit(String[] args, RtsConfig config) {
        hsInitCount++;
        if (hsInitCount > 1) return;

        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("Eta RTS initialized!");
        }

        //setlocale(LC_CTYPE,"");
        RtsStats.startInit();
        // TODO: Implement Stable Ptrs, Globals, File Locking, HPC, IO Manager, Storage, Tracing, processing RTS Flags (is this necessary?)
        RtsFlags.initDefaults();
        RtsFlags.setFullProgArgs(args);
        RtsFlags.setup(args, config.rtsOptsEnabled, config.rtsOpts,
                      config.rtsHsMain);

        RtsScheduler.initScheduler();
        // RtsScheduler.initTimer();
        /* TODO: Ensure that the timer can start here */
        // RtsScheduler.startTimer();
        if (RtsFlags.ModeFlags.threaded) {
            RtsIO.ioManagerStart();
        }
        RtsStats.endInit();
    }

    public static void shutdownHaskellAndExit(ExitCode exitStatus, boolean fastExit) {
        if (!fastExit) {
            hsInitCount = 1;
            hsExit_(false);
        }
        stgExit(exitStatus);
    }

    public static void shutdownHaskellAndSignal(int signal, boolean fastExit) {
        if (!fastExit) {
            hsExit_(false);
        }
        // TODO: Implement signals
        stgExit(ExitCode.EXIT_KILLED);
    }

    public static void hsExit_(boolean waitForeign) {
        if (hsInitCount <= 0) {
            errorBelch("warning: too many hs_exits()s");
        } else {
            hsInitCount--;
            if (hsInitCount <= 0) {
                // TODO: Finish up
                flushStdHandles();
                if (RtsFlags.ModeFlags.threaded) {
                    RtsIO.ioManagerDie();
                }
                RtsScheduler.exit(waitForeign);
                for (Capability c: Capability.capabilities) {
                    runAllJavaFinalizers(c.weakPtrList);
                }
                if (RtsFlags.MiscFlags.installSignalHandlers) {
                    /* TODO: Signal Handling: freeSignalHandlers() */
                }
                //RtsScheduler.stopTimer();
                //RtsScheduler.exitTimer(waitForeign);
                if (RtsFlags.MiscFlags.installSignalHandlers) {
                    /* TODO: Signal Handling: resetDefaultHandlers() */
                }
                RtsScheduler.free();
            }
        }
    }

    private static StgClosure flushStdHandles_closure = null;

    static {
        try {
            flushStdHandles_closure = (StgClosure)
                Class.forName("base.ghc.TopHandler")
                .getField("flushStdHandles_closure")
                .get(null);
        } catch (Exception e) {
            e.printStackTrace();
            flushStdHandles_closure = null;
        }
    }

    public static void flushStdHandles() {
        Capability cap = Rts.lock();
        HaskellResult result = Rts.evalIO(cap, flushStdHandles_closure);
        cap = result.cap;
        Rts.unlock(cap);
    }


    public static void stgExit(ExitCode exitCode) {
        int code = exitCode.code();
        if (code != 0) {
            System.exit(code);
        }
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

        public static ExitCode from(int code) {
            switch (code) {
                case 0:
                    return EXIT_SUCCESS;
                case 1:
                    return EXIT_FAILURE;
                case 63:
                    return EXIT_MISMATCH;
                case 77:
                    return EXIT_SKIP;
                case 250:
                    return EXIT_KILLED;
                case 251:
                    return EXIT_HEAPOVERFLOW;
                case 252:
                    return EXIT_INTERRUPTED;
                case 253:
                    return EXIT_DEADLOCK;
                case 254:
                    return EXIT_INTERNAL_ERROR;
                default:
                    throw new IllegalArgumentException("[ExitCode.from(int)] Invalid code: " + code);
            }
        }
    }

    public static void hsExit() {
        hsExit_(true);
    }

    public static void wakeUp() {
        RtsIO.ioManagerWakeup();
    }

    public static void runAllJavaFinalizers(List<StgWeak> weakPtrs) {
        Task task = Task.myTask();

        if (task != null) {
            task.runningFinalizers = true;
        }

        for (StgWeak w: weakPtrs) {
            w.runJavaFinalizers();
        }

        if (task != null) {
            task.runningFinalizers = false;
        }
    }

    public static void scheduleThread(Capability cap, StgTSO tso) {
        cap.appendToRunQueue(tso);
    }

    public static void scheduleThreadOn(Capability cap, int cpu, StgTSO tso) {
        tso.addFlags(TSO_LOCKED);
        if (RtsFlags.ModeFlags.threaded) {
            cpu %= Capability.enabledCapabilities;
            if (cpu == cap.no) {
                cap.appendToRunQueue(tso);
            } else {
                cap.migrateThread(tso, Capability.capabilities.get(cpu));
            }
        } else {
            cap.appendToRunQueue(tso);
        }
    }


}
