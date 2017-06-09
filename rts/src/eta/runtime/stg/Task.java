package eta.runtime.stg;

import java.util.List;
import java.util.ArrayList;
import java.util.Queue;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import eta.runtime.Rts;
import eta.runtime.RtsFlags;
import eta.runtime.stg.*;
import eta.runtime.thread.WorkerThread;
import static eta.runtime.RtsScheduler.*;
import static eta.runtime.RtsScheduler.SchedulerStatus.*;
import static eta.runtime.Rts.ExitCode.EXIT_FAILURE;
import static eta.runtime.RtsMessages.*;
import static eta.runtime.stg.Capability.*;
import static eta.runtime.Rts.*;

public class Task {
    public static List<Task> allTasks = new ArrayList<Task>();
    public static int workerCount;
    public static int currentWorkerCount;
    public static int peakWorkerCount;
    private static boolean tasksInitialized;

    public static final ThreadLocal<Task> myTask = new ThreadLocal<Task>() {
            @Override
            protected Task initialValue() {
                return null;
            }
        };

    public static void init() {
        if (!tasksInitialized) {
            allTasks.clear();
            workerCount = 0;
            currentWorkerCount = 0;
            peakWorkerCount = 0;
            tasksInitialized = true;
        }
    }

    public static void startWorkerTasks(int from, int to) {
        Capability cap = null;
        for (int i = from; i < to; i++) {
            cap = capabilities.get(i);
            Lock l = cap.lock;
            l.lock();
            try {
                cap.startWorkerTask();
            } finally {
                l.unlock();
            }
        }
    }

    public static void setMyTask(Task task) {
        myTask.set(task);
    }

    public long id;
    public WeakReference<Thread> thread;
    public Capability cap;
    public Queue<InCall> spareIncalls = new ArrayDeque<InCall>();
    public InCall incall;
    public Deque<InCall> incallStack = new ArrayDeque<InCall>();
    public boolean worker;
    public boolean stopped;
    public boolean runningFinalizers;
    public boolean wakeup;
    public final Lock lock = new ReentrantLock();
    public final Condition condition = lock.newCondition();

    public class InCall {
        public TSO tso;
        public TSO suspendedTso;
        public Capability suspendedCap;
        public SchedulerStatus returnStatus = NoStatus;
        public Closure ret;
        public Task task() {
            return Task.this;
        }
    }

    public Task(boolean worker) {
        this.worker = worker;
    }

    public void initialize() {
        synchronized (Task.class) {
            allTasks.add(this);
            if (worker) {
                workerCount++;
                currentWorkerCount++;
                if (currentWorkerCount > peakWorkerCount) {
                    peakWorkerCount = currentWorkerCount;
                }
            }
        }
    }

    public boolean isBound() {
        return incall.tso != null;
    }

    public static Task myTask() {
        return myTask.get();
    }

    public static Task newBoundTask() {
        if (!tasksInitialized) {
            // WARNING: This may not be intended behavior!
            Rts.init(null, null);
            if (!tasksInitialized) {
                errorBelch("newBoundTask: RTS is not initialized; unable to re-initialize RTS.");
                stgExit(EXIT_FAILURE);
            }
        }
        Task task = allocTask();
        task.stopped = false;
        task.newInCall();
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("New Task (task count: %d)", allTasks.size());
        }
        return task;
    }

    public void newInCall() {
        InCall incall = spareIncalls.poll();
        if (incall == null) {
            incall = this.new InCall();
        }
        this.incall = incall;
        incallStack.push(incall);
    }

    private static Task allocTask() {
        Task task = myTask();
        if (task != null) {
            return task;
        } else {
            task = new Task(false);
            task.initialize();
            task.id = Thread.currentThread().getId();
            task.thread = new WeakReference<Thread>(Thread.currentThread());
            setMyTask(task);
            return task;
        }
    }

    public Capability waitForCapability() {
        return waitForCapability(null);
    }

    public Capability waitForCapability(Capability cap) {
        if (RtsFlags.ModeFlags.threaded) {
            if (cap == null) {
                cap = getFreeCapability();
                this.cap = cap;
            } else {
                assert this.cap == cap: "waitForCapability: this.cap != cap";
            }

            if (RtsFlags.DebugFlags.scheduler) {
                debugBelch("Waiting for Capability[%d].", cap.no);
            }
            Lock l = cap.lock;
            boolean unlocked = false;
            l.lock();
            try {
                if (cap.runningTask != null) {
                    cap.newReturningTask(this);
                    l.unlock();
                    unlocked = true;
                    cap = waitForReturnCapability();
                } else {
                    cap.runningTask = this;
                }
            } finally {
                if (!unlocked) {
                    l.unlock();
                }
            }
            cap.assertFullCapabilityInvariants(this);
            if (RtsFlags.DebugFlags.scheduler) {
                debugBelch("Resuming Capability[%d].", cap.no);
            }
        } else {
            mainCapability.runningTask = this;
            cap = mainCapability;
            this.cap = cap;
        }
        return cap;
    }

    public Capability waitForReturnCapability() {
        Capability cap = null;
        while (true) {
            Lock l = lock;
            l.lock();
            try {
                if (!wakeup) condition.await();
                cap = this.cap;
                wakeup = false;
            } catch (InterruptedException e) {
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("waitForReturnCapability: Interrupted");
                }
                continue;
            } finally {
                l.unlock();
            }
            l = cap.lock;

            boolean unlocked = false;
            l.lock();
            try {
                if (cap.runningTask == null) {
                    Task task = cap.returningTasks.getFirst();
                    if (task != this) {
                        cap.giveToTask(task);
                        l.unlock();
                        unlocked = true;
                        continue;
                    } else {
                        cap.runningTask = this;
                        cap.popReturningTask();
                        l.unlock();
                        unlocked = true;
                        break;
                    }
                }
            } finally {
                if (!unlocked) {
                    l.unlock();
                }
            }
        }
        return cap;
    }

    public void boundTaskExiting() {
        assert Thread.currentThread().getId() == id;
        assert myTask() == this;
        endInCall();
        if (incall == null) {
            stopped = true;
        }
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("{Scheduler} Task[%d] exiting.", id);
        }
    }

    public void endInCall() {
        InCall incall = incallStack.pop();
        incall.tso = null;
        spareIncalls.offer(incall);
        this.incall = incallStack.peek();
    }

    public final void workerTaskStop() {
        synchronized (Task.class) {
            allTasks.remove(this);
            currentWorkerCount--;
        }
        free();
    }

    public final void free() {
        incallStack.clear();
        spareIncalls.clear();
        cap = null;
        incall = null;
    }

    public final boolean isWorker() {
        return (worker && incallStack.size() == 1);
    }

    public static void shutdownThread() {
        // Method of interrupting thread;
        //throw new InterruptedException();
    }

    public final Capability waitForWorkerCapability() {
        Capability cap = null;
        while (true) {
            lock.lock();
            try {
                if (!wakeup) condition.await();
                cap = this.cap;
                wakeup = false;
            } catch (InterruptedException e) {
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("waitForReturnCapability: Interrupted");
                }
                continue;
            } finally {
                lock.unlock();
            }
            Lock l = cap.lock;
            boolean unlocked = false;
            l.lock();
            try {
                if (cap.runningTask != null) {
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("Capability[%d] is owned by another task.", cap.no);
                    }
                    l.unlock();
                    unlocked = true;
                    continue;
                }

                if (this.cap != cap) {
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("task has been migrated to cap %d",
                                   this.cap.no);
                    }
                    l.unlock();
                    unlocked = true;
                    continue;
                }

                if (incall.tso == null) {
                    Task task = cap.spareWorkers.peek();
                    assert task != null;
                    if (task != this) {
                        cap.giveToTask(task);
                        l.unlock();
                        unlocked = true;
                        continue;
                    } else {
                        cap.spareWorkers.poll();
                    }
                }
                cap.runningTask = this;
                break;
            } finally {
                if (!unlocked) {
                    l.unlock();
                }
            }
        }
        return cap;
    }

    public final void interrupt() {
        thread.interrupt();
    }

    public static int freeTaskManager() {
        int tasksRunning = 0;
        synchronized (Task.class) {

            for (Task task: allTasks) {
                if (task.stopped) {
                    task.free();
                } else {
                    tasksRunning++;
                }
            }
            allTasks.clear();
        }
        tasksInitialized = false;
        return tasksRunning;
    }

    public final boolean isAlive() {
        return thread.isAlive();
    }

    public final boolean assertTaskId() {
        assert id == Thread.currentThread().getId();
    }
}
