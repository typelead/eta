package eta.runtime.io;

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

    public void tryStartTransaction() {
        return lock.compareAndSet(false, true);
    }

    public void endTransaction() {
        return lock.set(false);
    }
}
