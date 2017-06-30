package eta.runtime.io;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class SizeLock {

    private AtomicInteger permits = new AtomicInteger(1);

    /* This lock is used for making tryAcquire() atomic. */
    private AtomicBoolean lock = new AtomicBoolean();

    public boolean tryAcquire() {
        if (tryStartTransaction()) {
            try {
                return unconditionalAcquire();
            } finally {
                endTransaction();
            }
        }
        return false;
    }

    public boolean unconditionalAcquire() {
        if (permits.get() <= 0) return false;
        permits.getAndDecrement();
        return true;
    }

    public void enlarge() {
        permits.getAndIncrement();
    }

    public boolean tryStartTransaction() {
        return lock.compareAndSet(false, true);
    }

    public void endTransaction() {
        lock.set(false);
    }
}
