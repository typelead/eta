package ghcvm.runtime.types;

#include "Rts.h"

import java.util.ArrayList;
import static ghcvm.runtime.types.Task.InCall;
import static ghcvm.runtime.RtsScheduler.*;
import static ghcvm.runtime.RtsMessages.*;
import ghcvm.runtime.*;
import ghcvm.runtime.thread.*;

public class Capability {
    public static int nCapabilities;
    public static int enabledCapabilities;
    public static Capability mainCapability;
    public static Capability lastFreeCapability;
    public static ArrayList<Capability> capabilities;
    public static int pendingSync;
    public static void init() {
#if defined(THREADED_RTS)
        nCapabilities = 0
        moreCapabilities(0, RtsFlags.ParFlags.nNodes);
        nCapabilities = RtsFlags.ParFlags.nNodes;
#else
        nCapabilities = 1;
        mainCapability = new Capability(0);
        capabilities.add(0, mainCapability);
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
            capabilities.add(0, mainCapability);
        } else {
            for (int i = 0; i < to; i++) {
                if (i < from) {
                    capabilities.add(i, oldCapabilities[i]);
                } else {
                    capabilities.add(i, new Capability(i));
                }
            }
        }
    }
#endif

    public int no;
    public StgRegTable regTable = new StgRegTable();
    public StgFunTable funTable = new StgFunTable();
    public Task runningTask;
    public boolean inHaskell = false;
    public int idle = 0;
    public boolean disabled = false;
    public StgTSO runQueueHead;
    public StgTSO runQueueTail;
    public InCall suspendedJavaCalls;
    public int contextSwitch;
    public int interrupt;
    public Task spareWorkers;
    public int nSpareWorkers;
    public Task returningTasksHead;
    public Task returningTasksTail;
    public Message inbox;
    public SparkPool sparks = new SparkPool();
    public SparkCounters sparkStats = new SparkCounters();
    public int ioManagerControlWrFd; // Not sure if this is necessary

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
        while (true) {
            if (this.inHaskell) {
                errorBelch("schedule: re-entered unsafely.\n" +
                           "    Perhaps a 'foreign import unsafe' should be 'safe'?");
                System.exit(EXIT_FAILURE);
            }

            switch (schedulerState) {
                case SCHED_RUNNING:
                    break;
                case SCHED_INTERRUPTING:
                    // scheduleDoGC

                case SCHED_SHUTTING_DOWN:
                    if (!isBoundTask(task) && emptyRunQueue(this)) {
                        return this;
                    }
                    break;
                default:
                    barf("sched state: " + schedulerState);
            }
        }
    }

    public void startWorkerTask() {
        Task task = new Task(true);
        // TODO: Verify that this synchronization is doing as desired
        synchronized (task) {
            task.cap = this;
            this.runningTask = task;
            WorkerThread thread = new WorkerThread(task);
            thread.start();
            task.id = thread.getId();
        }
    }
}
