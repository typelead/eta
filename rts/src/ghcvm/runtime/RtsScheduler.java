package ghcvm.runtime;

import java.util.Queue;
import java.util.ArrayDeque;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Timer;
import java.util.TimerTask;

import ghcvm.runtime.stg.Task;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import static ghcvm.runtime.Rts.HaskellResult;
import static ghcvm.runtime.RtsScheduler.RecentActivity.*;
import static ghcvm.runtime.RtsScheduler.SchedulerState.*;
import static ghcvm.runtime.RtsScheduler.SchedulerStatus.*;

public class RtsScheduler {

    public static Timer intervalTimer;
    public static AtomicInteger timerDisabled = new AtomicInteger(0);

    public enum SchedulerState {
        SCHED_RUNNING(0),  /* running as normal */
        SCHED_INTERRUPTING(1),  /* ^C detected, before threads are deleted */
        SCHED_SHUTTING_DOWN(2);  /* final shutdown */

        private int state;

        SchedulerState(int state) {
            this.state = state;
        }

        public int compare(SchedulerState other) {
            return Integer.compare(this.state, other.state);
        }
    }

    public enum SchedulerStatus {
        NoStatus, Success, Killed, Interrupted, HeapExhausted
    }

    public static Queue<StgTSO> blockedQueue = new ArrayDeque<StgTSO>();
    public static Queue<StgTSO> sleepingQueue = new ArrayDeque<StgTSO>();
    public enum RecentActivity {
        Yes, MaybeNo, Inactive, DoneGC
    }
    public static RecentActivity recentActivity = Yes;
    public static SchedulerState schedulerState = SCHED_RUNNING;
    public static HaskellResult scheduleWaitThread(StgTSO tso, Capability cap) {
        Task task = cap.runningTask;
        tso.bound = task.incall;
        tso.cap = cap;
        task.incall.tso = tso;
        task.incall.returnStatus = NoStatus;
        cap.appendToRunQueue(tso);
        cap = cap.schedule(task);
        return new HaskellResult(cap, task.incall.ret);
    }

    public static void initScheduler() {
        schedulerState = SCHED_RUNNING;
        recentActivity = Yes;

        synchronized (RtsScheduler.class) {
            Capability.init();
            Task.init();
            if (RtsFlags.ModeFlags.threaded) {
                Task.startWorkerTasks(1, Capability.nCapabilities);
            }
        }
    }

    public static boolean emptySleepingQueue() {
        return sleepingQueue.isEmpty();
    }

    public static boolean emptyBlockedQueue() {
        return blockedQueue.isEmpty();
    }

    public static void initTimer() {
        if (!RtsFlags.DebugFlags.sanity) {
            if (RtsFlags.MiscFlags.tickInterval != 0) {
                initTicker();
            }
            timerDisabled.set(1);
        }
    }

    public static void initTicker() {
        intervalTimer = new Timer();
    }

    public static class Ticker extends TimerTask {
        public long ticksToContextSwitch = 0;
        public long ticksToGC = 0;

        @Override
        public void run() {
            if (RtsFlags.ConcFlags.ctxtSwitchTicks > 0) {
                ticksToContextSwitch--;
                if (ticksToContextSwitch <= 0) {
                    ticksToContextSwitch = RtsFlags.ConcFlags.ctxtSwitchTicks;
                    Capability.contextSwitchAll();
                }
            }

            switch (recentActivity) {
                case Yes:
                    break;
                case MaybeNo:
                    /* TODO: Figure out how to handle GC */
                    if (false) {
                        if (ticksToGC == 0) {
                            if (RtsFlags.GcFlags.doIdleGC) {
                                recentActivity = Inactive;
                                if (RtsFlags.ModeFlags.threaded) {
                                    Rts.wakeUp();
                                }
                            } else {
                                recentActivity = DoneGC;
                                stopTimer();
                            }
                        } else {
                            ticksToGC--;
                        }
                    }
                    break;
                default:
                    break;
            }
        }
    }

    public static void startTimer() {
        if (!RtsFlags.DebugFlags.sanity) {
            if (timerDisabled.decrementAndGet() == 0) {
                if (RtsFlags.MiscFlags.tickInterval != 0) {
                    startTicker();
                }
            }
        }
    }

    public static void stopTimer() {
        if (timerDisabled.incrementAndGet() == 1) {
            if (RtsFlags.MiscFlags.tickInterval != 0) {
                stopTicker();
            }
        }
    }

    public static void exitTimer(boolean wait) {
        if (RtsFlags.MiscFlags.tickInterval != 0) {
            exitTicker(wait);
        }
    }

    public static void startTicker() {
        intervalTimer.schedule(new Ticker(), 0, RtsFlags.MiscFlags.tickInterval);
    }

    public static void stopTicker() {
        intervalTimer.cancel();
        /* TODO: Block until the task is canceled */
    }

    public static void exitTicker(boolean wait) {
        intervalTimer = null;
    }

    public static void free() {
        synchronized (RtsScheduler.class) {
            int stillRunning = Task.freeTaskManager();
            if (stillRunning == 0) {
                Capability.freeCapabilities();
            }
        }
    }

    public static void exit(boolean waitForeign) {
        Task task = Task.newBoundTask();
        if (schedulerState.compare(SCHED_SHUTTING_DOWN) < 0) {
            schedulerState = SCHED_INTERRUPTING;
            Capability cap = task.cap;
            cap = task.waitForCapability();
            //scheduleDoDC(cap, task true);
            cap.release();
        }
        schedulerState = SCHED_SHUTTING_DOWN;
        Capability.shutdownCapabilities(task, waitForeign);
        task.boundTaskExiting();
    }
}
