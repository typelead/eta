package eta.runtime.thread;

import java.util.concurrent.locks.Lock;

import eta.runtime.stg.*;

public class WorkerThread extends Thread {
    private Task task;

    public WorkerThread(Task task) {
        this.task = task;
    }

    public void setTask() {
        task.thread = this;
    }

    @Override
    public void run() {
        // try {
            Capability cap = null;
            Lock l = task.lock;
            l.lock();
            try {
                cap = task.cap;
            } finally {
                l.unlock();
            }
            // setThreadAffinity
            Task.setMyTask(task);
            task.newInCall();
            cap.scheduleWorker(task);
        // } catch (Exepti e) {
        //     return;
        // }
    }
}
