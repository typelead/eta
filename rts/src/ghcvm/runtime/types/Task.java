package ghcvm.runtime.types;

import java.util.List;
import java.util.ArrayList;
import java.util.Queue;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import ghcvm.runtime.RtsFlags;
import ghcvm.runtime.closure.*;
import static ghcvm.runtime.RtsScheduler.*;
import static ghcvm.runtime.RtsScheduler.SchedulerStatus.*;
import static ghcvm.runtime.Rts.ExitCode.EXIT_FAILURE;
import static ghcvm.runtime.RtsMessages.*;
import static ghcvm.runtime.types.Capability.*;
import static ghcvm.runtime.Rts.*;

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
            synchronized (cap) {
                cap.startWorkerTask();
            }
        }
    }

    public static void setMyTask(Task task) {
        myTask.set(task);
    }

    public long id;
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
        public StgTSO tso;
        public StgTSO suspendedTso;
        public Capability suspendedCap;
        public SchedulerStatus returnStatus = NoStatus;
        public StgClosure ret;
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
            errorBelch("newBoundTask: RTS is not initialized; call hsInit() first");
            stgExit(EXIT_FAILURE);
        }
        Task task = allocTask();
        task.stopped = false;
        task.newInCall();
        //debugTrace
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
            if (RtsFlags.ModeFlags.threaded) {
                task.id = Thread.currentThread().getId();
            }
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
            } else {
                // ASSERT(this.cap == cap);
            }

            Lock l = cap.lock;
            l.lock();
            try {
                if (cap.runningTask != null) {
                    cap.newReturningTask(this);
                    l.unlock();
                    cap = waitForReturnCapability();
                } else {
                    cap.runningTask = this;
                }
            } finally {
                l.unlock();
            }
            // DEBUG_sched
            // debugTrace
        } else {
            mainCapability.runningTask = this;
            cap = mainCapability;
        }
        this.cap = cap;
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
                // Do something here;
            } finally {
                l.unlock();
            }
            l = cap.lock;
            l.lock();
            try {
                if (cap.runningTask == null) {
                    Task task = cap.returningTasks.getFirst();
                    if (task != this) {
                        cap.giveToTask(task);
                        l.unlock();
                        continue;
                    } else {
                        cap.runningTask = this;
                        cap.popReturningTask();
                        l.unlock();
                        break;
                    }
                }
            } finally {
                l.unlock();
            }
        }
        return cap;
    }

    public void boundTaskExiting() {
        endInCall();
        if (incall == null) {
            stopped = true;
        }
        //debugTrace
    }

    public void endInCall() {
        InCall incall = incallStack.pop();
        incall.tso = null;
        spareIncalls.offer(incall);
        this.incall = incallStack.peek();
    }
}
