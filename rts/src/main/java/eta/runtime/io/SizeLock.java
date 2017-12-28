package eta.runtime.io;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class SizeLock {

    private static final int INITIAL_PERMITS = 1;
    private AtomicInteger permits = new AtomicInteger(INITIAL_PERMITS);

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

    public int peekPermits() {
        return permits.get();
    }

    public void reset() {
        permits.set(INITIAL_PERMITS);
    }
}
