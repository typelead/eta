package ghcvm.runtime;

import java.util.concurrent.locks.Lock;
import java.util.Queue;
import java.util.ArrayDeque;
import ghcvm.runtime.types.*;
import ghcvm.runtime.closure.*;
import static ghcvm.runtime.RtsMessages.*;

public class RtsScheduler {

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

    public static class HaskellResult {
        public final Capability cap;
        public final StgClosure result;

        public HaskellResult(final Capability cap, final StgClosure result) {
            this.cap = cap;
            this.result = result;
        }
    }

    public static Queue<StgTSO> blockedQueue = new ArrayDeque<StgTSO>();
    public static Queue<StgTSO> sleepingQueue = new ArrayDeque<StgTSO>();
    public enum RecentActivity {
        Yes, Inactive, DoneGC
    }
    public static RecentActivity recentActivity = RecentActivity.Yes;
    public static SchedulerState schedulerState = SchedulerState.SCHED_RUNNING;
    public static HaskellResult scheduleWaitThread(StgTSO tso, Capability cap) {
        Task task = cap.runningTask;
        tso.bound = task.incall;
        tso.cap = cap;
        task.incall.tso = tso;
        task.incall.returnStatus = SchedulerStatus.NoStatus;
        cap.appendToRunQueue(tso);
        cap = cap.schedule(task);
        return new HaskellResult(cap, task.incall.ret);
    }

    public static void initScheduler() {
        schedulerState = SchedulerState.SCHED_RUNNING;
        recentActivity = RecentActivity.Yes;

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
}
