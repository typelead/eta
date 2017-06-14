package eta.runtime.concurrent;

import eta.runtime.stg.Capability;

public class WorkerThread extends Thread {

    public WorkerThread() {}

    @Override
    public void run() {
        Capability worker = Capability.getLocal(true);
        worker.schedule(null);
        workerCapabilities.remove(cap);
    }
}
