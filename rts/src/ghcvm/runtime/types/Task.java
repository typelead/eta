package ghcvm.runtime.types;

#include "Rts.h"

import static ghcvm.runtime.RtsTaskManager;

public class Task {
    public Capability cap;
    public InCall incall;
    public int nSpareIncalls;
    public InCall spareIncalls;
    boolean worker;
    boolean stopped;
    boolean runningFinalizers;
    Task next;
    Task allNext;
    Task allPrev;

#if defined(THREADED_RTS)
    public int threadId;
    boolean wakeup = false;
#endif

    public static class InCall {
        public StgTSO tso;
        public StgTSO suspendedTso;
        public Capability suspendedCap;
        public SchedulerStatus returnStatus;
        public REF_CLOSURE_PTR returnValue;
        public Task task;
        public InCall prevStack;
        public InCall prev;
        public InCall next;
    }

    public Task(boolean worker) {
        this.worker = worker;

        synchronized (RtsTaskManager.class) {
            this.allNext = allTasks;
            if (allTasks != null) {
                allTasks.allPrev = this;
            }
            allTasks = this;
            taskCount++;

            if (worker) {
                workerCount++;
                currentWorkerCount++;
                if (currentWorkerCount > peakWorkerCount) {
                    peakWorkerCount = currentWorkerCount;
                }
            }
        }
    }
}
