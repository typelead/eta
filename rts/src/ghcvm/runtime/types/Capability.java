package ghcvm.runtime.types;

#include "Rts.h"

import static ghcvm.runtime.types.Task.InCall;
import ghcvm.runtime.*;

public class Capability {
    public int no;
    public StgRegTable r;
    public StgFunTable f;
    public Task runningTask;
    public boolean inHaskell;
    public boolean disabled;
    public StgTSO runQueueHead;
    public StgTSO runQueueTail;
    public InCall suspendedJavaCalls;
    public int contextSwitch;
    public int interrupt;

    public Capability() {}
    public void appendToRunQueue(StgTSO tso) {
        if (runQueueHead == null) {
            runQueueHead = tso;
            tso.blockInfo = null;
        } else {
            runQueueTail.link = tso;
            tso.link = runQueueTail;
        }
        runQueueTail = tso;
    }
    public Capability schedule(Task task) {
        Capability cap = this;

        while (true) {
            if (cap.inHaskell) {
                RtsMessages.errorBelch("schedule: re-entered unsafely.\n" +
                                       "    Perhaps a 'foreign import unsafe' should be 'safe'?");
                System.exit(EXIT_FAILURE);
            }

            switch (RtsScheduler.schedulerState) {
                case SCHED_RUNNING:
                    break;
                case SCHED_INTERRUPTING:
                    // scheduleDoGC

                case SCHED_SHUTTING_DOWN:
                    if (!isBoundTask(task) && emptyRunQueue(cap)) {
                        return cap;
                    }
                    break;
                default:
                    RtsMessages.barf("sched state: " + RtsScheduler.schedulerState);
            }
        }
    }
}
