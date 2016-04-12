package ghcvm.runtime.types;

#include "Rts.h"

import java.util.ArrayList;
import static ghcvm.runtime.types.Task.InCall;
import ghcvm.runtime.*;

public class Capability {
    public static int nCapabilities = 0;
    public static int enabledCapabilities = 0;
    public static Capability mainCapability = null;
    public static Capability lastFreeCapability = null;
    public static ArrayList<Capability> capabilities = null;
    public static int pendingSync = 0;
    public static void init() {
#if defined(THREADED_RTS)
        nCapabilities = 0
        moreCapabilities(0, RtsFlags.ParFlags.nNodes);
        nCapabilities = RtsFlags.ParFlags.nNodes;
#else
        nCapabilities = 1;
        mainCapability = new Capability(0);
        capabilities[0] = mainCapability;
#endif
        enabledCapabilities = nCapabilities;
        lastFreeCapability = capabilities.get(0);
    }

#if defined(THREADED_RTS)
    public static void moreCapabilities (int from, into to) {
        ArrayList<Capability> oldCapabilities = capabilities;
        capabilities = new ArrayList<Capability>(to);
        if (to == 1) {
            mainCapability = new Capability(0);
            capabilities[0] = mainCapability;
        } else {
            for (int i = 0; i < to; i++) {
                if (i < from) {
                    capabilities[i] = oldCapabilities[i];
                } else {
                    capabilities[i] = new Capability(i);
                }
            }
        }
    }
#endif

    public int no;
    public StgRegTable r;
    public StgFunTable f;
    public Task runningTask;
    public boolean inHaskell = false;
    public int idle = 0;
    public boolean disabled = false;
    public StgTSO runQueueHead;
    public StgTSO runQueueTail;
    public InCall suspendedJavaCalls;
    public int contextSwitch;
    public int interrupt;

    public Capability(int i) {
        this.no = i;
    }

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

    public void startWorkerTask() {
        Task task = new Task(true);
        synchronized (task) {
            task.cap = this;
            this.runningTask = task;
        }
    }
}
