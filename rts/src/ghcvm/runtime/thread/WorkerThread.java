package ghcvm.runtime.thread;

import ghcvm.runtime.types.*;

public class WorkerThread extends Thread {
    private Task task;

    public WorkerThread(Task task) {
        this.task = task;
    }

    @Override
    public void run() {

        this.task = null;
    }
}
