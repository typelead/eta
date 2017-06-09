package eta.runtime.message;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import static eta.runtime.concurrent.Concurrent.SPIN_COUNT;

public class MessageThrowTo extends Message {
    public static AtomicLong maxMessageId = new AtomicLong(0);
    public static long nextMessageId() {
        return maxMessageId.getAndIncrement();
    }
    public final long id = nextMessageId();
    public final TSO source;
    public final TSO target;
    public final  Closure exception;
    public volatile AtomicBoolean lock = new AtomicBoolean(false);

    public MessageThrowTo(final TSO source, final TSO target, final Closure exception) {
        this.source = source;
        this.target = target;
        this.exception = exception;
    }

    public void done() {
        invalidate();
    }

    public final void lock() {
        do {
            int i = 0;
            do {
                boolean old = lock.getAndSet(true);
                if (!old) return;
            } while (++i < SPIN_COUNT);
            Thread.yield();
        } while (true);
    }

    public final void unlock() {
        lock.set(false);
    }

    @Override
    public final boolean isLocked() {
        return lock.get();
    }

    public final boolean tryLock() {
        return lock.getAndSet(true);
    }
}
