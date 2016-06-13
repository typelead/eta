package ghcvm.runtime.thread;

import java.util.concurrent.locks.Lock;

import ghcvm.runtime.types.*;

public class WorkerThread extends Thread {
    private Task task;

    public WorkerThread(Task task) {
        this.task = task;
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
