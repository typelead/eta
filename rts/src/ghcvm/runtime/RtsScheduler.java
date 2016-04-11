package ghcvm.runtime;

#include "Rts.h"

import ghcvm.runtime.types.*;

public class RtsScheduler {
    public static int schedulerState = SCHED_RUNNING;
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
}
