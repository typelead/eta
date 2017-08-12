package eta.runtime.concurrent;

import eta.runtime.Runtime;
import eta.runtime.stg.Capability;

public class OSThread extends Thread {

    private final int stablePtr;

    public OSThread(int stablePtr) {
        this.stablePtr = stablePtr;
    }

    @Override
    public void run()  {
        try {
          Runtime.evalStableIO(stablePtr);
        } catch (Exception e) {}
    }
}
