package ghcvm.runtime.types;

#include "Rts.h"

import ghcvm.runtime.RtsTaskManager;
import static ghcvm.runtime.RtsTaskManager.*;

public class Task {
    public long id;
    public Capability cap;
    public InCall incall;
    public int nSpareIncalls;
    public InCall spareIncalls;
    public boolean worker;
    public boolean stopped;
    public boolean runningFinalizers;
    public Task next;
    public Task allNext;
    public Task allPrev;
    public boolean wakeup;

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
