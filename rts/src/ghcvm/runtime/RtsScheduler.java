package ghcvm.runtime;

#include "Rts.h"

import java.util.Queue;
import java.util.ArrayDeque;
import ghcvm.runtime.types.*;
import ghcvm.runtime.closure.*;

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

    public static Queue<StgTSO> blockedQueue = new ArrayDeque<StgTSO>();
    public static Queue<StgTSO> sleepingQueue = new ArrayDeque<StgTSO>();
    public enum RecentActivity {
        Yes, Inactive, DoneGC
    }
    public static RecentActivity recentActivity = RecentActivity.Yes;
    public static SchedulerState schedulerState = SchedulerState.SCHED_RUNNING;
    public static void scheduleWaitThread(StgTSO tso, REF_CLOSURE_PTR ret, Ptr<Capability> pcap) {
        Capability cap = pcap.ref;
        Task task = cap.runningTask;
        tso.bound = task.incall;
        tso.cap = cap;
        task.incall.tso = tso;
        task.incall.returnValue = ret;
        task.incall.returnStatus = SchedulerStatus.NoStatus;
        cap.appendToRunQueue(tso);
        cap = cap.schedule(task);
        pcap.ref = cap;
    }

    public static void initScheduler() {
        schedulerState = SchedulerState.SCHED_RUNNING;
        recentActivity = RecentActivity.Yes;

        synchronized (RtsScheduler.class) {
            Capability.init();
            RtsTaskManager.init();
            if (RtsFlags.ModeFlags.threaded) {
                RtsTaskManager.startWorkerTasks(1, Capability.nCapabilities);
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
