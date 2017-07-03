package eta.runtime.concurrent;

import eta.runtime.stg.Capability;

public class WorkerThread extends Thread {

    public WorkerThread() {}

    @Override
    public void run() {
        Capability worker = Capability.getLocal(true);
        try {
            worker.schedule(null);
        } catch (Exception e) {
            /* TODO: Find a better way to handle exceptions? */
        }
        Capability.workerCapabilities.remove(worker);
    }
}
