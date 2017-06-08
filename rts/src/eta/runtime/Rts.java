package eta.runtime;

import java.util.List;
import java.util.concurrent.locks.Lock;

import eta.runtime.stg.Task;
import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.ForceIO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgWeak;
import eta.runtime.apply.ApV;
import eta.runtime.apply.ApO;
import static eta.runtime.RtsScheduler.SchedulerStatus;
import static eta.runtime.RtsScheduler.scheduleWaitThread;
import static eta.runtime.RtsMessages.errorBelch;
import static eta.runtime.RtsMessages.debugBelch;
import static eta.runtime.stg.TSO.TSO_LOCKED;
import static eta.runtime.stg.TSO.TSO_BLOCKEX;
import static eta.runtime.stg.TSO.TSO_INTERRUPTIBLE;

public class Rts {
    public static class StgResult {
        public final Capability cap;
        public final Closure result;

        public StgResult(final Capability cap, final Closure result) {
            this.cap = cap;
            this.result = result;
        }
    }

    public static ExitCode main(String[] args, Closure mainClosure, RtsConfig config) {
        ExitCode exitStatus = ExitCode.EXIT_SUCCESS;

        init(args, config);

        Capability cap = lock();
        StgResult result = Rts.evalLazyIO(cap, mainClosure);
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
        shutdownAndExit(exitStatus, false, false);
        // This return is never seen since shutdownAndExit() will
        // terminate the process. It's there to keep javac happy.
        return exitStatus;
    }

    public static Capability lock() {
        Task task = Task.newBoundTask();
        if (task.runningFinalizers) {
            errorBelch("error: a Java finalizer called back into Eta.\n" +
                       "   To create finalizers that may call back into Eta, use\n" +
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

    public static StgResult evalLazyIO(Capability cap, Closure p) {
        TSO tso = createIOThread(cap, p);
        return scheduleWaitThread(tso, cap);
    }

    public static StgResult evalIO(Capability cap, Closure p) {
        TSO tso = createStrictIOThread(cap, p);
        return scheduleWaitThread(tso, cap);
    }

    public static StgResult evalJava(Capability cap, Object o, Closure p) {
        TSO tso = createStrictJavaThread(cap, o, p);
        return scheduleWaitThread(tso, cap);
    }

    public static SchedulerStatus getSchedStatus(Capability cap) {return null;}

    public static Closure evalLazyIO_closure = new EvalLazyIO();
    public static class EvalLazyIO extends Closure {
        @Override
        public Closure enter(StgContext context) {
            p.evaluate(context).applyV(context);
        }
    }

    public static TSO createIOThread(Capability cap, Closure p) {
        return new TSO(cap, evalLazyIO_closure);
    }

    public static Closure evalIO_closure = new EvalIO();
    public static class EvalIO extends Closure {
        @Override
        public Closure enter(StgContext context) {
            p.evaluate(context).applyV(context).evaluate(context);
        }
    }


    public static TSO createStrictIOThread(Capability cap, Closure p) {
        return new TSO(cap, evalIO_closure);
    }

    public static Closure evalJava_closure = new EvalJava();
    public static class EvalJava extends Closure {
        @Override
        public Closure enter(StgContext context) {
            p.evaluate(context).applyO(context, thisObj).evaluate(context);
        }
    }

    public static TSO createStrictJavaThread(Capability cap, Object thisObj, Closure p) {
        return new TSO(cap, evalJava_closure);
    }

    private static int rtsInitCount;
    public static void init(String[] args, RtsConfig config) {
        rtsInitCount++;
        if (rtsInitCount > 1) return;

        //setlocale(LC_CTYPE,"");
        RtsStats.startInit();
        // TODO: Implement Stable Ptrs, Globals, File Locking, HPC, IO Manager, Storage, Tracing, processing RTS Flags (is this necessary?)
        /* Don't override processed RTS args */
        if (args != null && config != null) {
            RtsFlags.initDefaults();
            RtsFlags.setFullProgArgs(args);
            RtsFlags.setup(args, config.rtsOptsEnabled, config.rtsOpts,
                           config.rtsHsMain);
        }

        RtsScheduler.init();
        if (RtsFlags.ModeFlags.threaded) {
            RtsIO.ioManagerStart();
        }
        RtsStats.endInit();
    }

    public static void shutdownAndExit(ExitCode exitStatus, boolean fastExit, boolean hardExit) {
        if (!fastExit) {
            rtsInitCount = 1;
            exit_(false);
        }
        if (exitStatus != ExitCode.EXIT_SUCCESS || hardExit) stgExit(exitStatus);
    }

    public static void shutdownAndSignal(int signal, boolean fastExit) {
        if (!fastExit) {
            exit_(false);
        }
        // TODO: Implement signals
        stgExit(ExitCode.EXIT_KILLED);
    }

    public static void exit_(boolean waitForeign) {
        if (rtsInitCount <= 0) {
            errorBelch("WARNING: Too many Rts.exit()'s.");
        } else {
            rtsInitCount--;
            if (rtsInitCount <= 0) {
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

    private static Closure flushStdHandles_closure = null;

    static {
        try {
            flushStdHandles_closure = (Closure)
                Class.forName("base.ghc.TopHandler")
                .getMethod("flushStdHandles_closure")
                .invoke(null);
        } catch (Exception e) {
            e.printStackTrace();
            flushStdHandles_closure = null;
        }
    }

    public static void flushStdHandles() {
        Capability cap = Rts.lock();
        StgResult result = Rts.evalIO(cap, flushStdHandles_closure);
        cap = result.cap;
        Rts.unlock(cap);
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

    public static void exit() {
        exit_(true);
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

    public static TSO scheduleIOClosure(Closure closure) {
        Capability cap = Capability.getFreeRunningCapability();
        TSO tso = Rts.createIOThread(cap, closure);
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        Rts.scheduleThread(cap, tso);
        return tso;
    }

    public static void scheduleThread(Capability cap, TSO tso) {
        cap.appendToRunQueue(tso);
    }

    public static void scheduleThreadOn(Capability cap, int cpu, TSO tso) {
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
