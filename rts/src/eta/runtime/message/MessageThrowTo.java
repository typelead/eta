package eta.runtime.message;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import static eta.runtime.concurrent.Concurrent.SPIN_COUNT;

public class MessageThrowTo extends Message {
    public static AtomicLong maxMessageId = new AtomicLong(0);
    public final long id = nextMessageId();
    public final StgTSO source;
    public final StgTSO target;
    public final  Closure exception;
    public volatile AtomicBoolean lock = new AtomicBoolean(false);

    public MessageThrowTo(final StgTSO source, final StgTSO target, final Closure exception) {
        this.source = source;
        this.target = target;
        this.exception = exception;
    }

    @Override
    public final void execute(Capability cap) {
        lock();
        if (!isValid()) {
            unlock();
        } else {
            boolean success = cap.throwToMsg(this);
            if (success) {
                StgTSO source = this.source;
                done();
                cap.tryWakeupThread(source);
            } else {
                unlock();
            }
        }
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

    public long nextMessageId() {
        return maxMessageId.getAndIncrement();
    }
}
