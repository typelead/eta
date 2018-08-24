package eta.runtime.concurrent;

import eta.runtime.Runtime;
import eta.runtime.stg.Capability;

import static eta.runtime.RuntimeLogging.*;

public class WorkerThread extends Thread {

    public WorkerThread() {}

    @Override
    public void run() {
        final Capability worker = Capability.getLocal(true);
        try {
            worker.schedule(null);
        } catch (Exception e) {
            if (Runtime.debugScheduler()) {
                debugScheduler("Died from exception: " + e.getMessage());
            }
            e.printStackTrace();
        } finally {
            worker.removeWorker();
        }
    }
}
