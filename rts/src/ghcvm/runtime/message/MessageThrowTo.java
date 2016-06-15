package ghcvm.runtime.message;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.types.StgTSO;

public class MessageThrowTo extends Message {
    public static AtomicLong maxMessageId = new AtomicLong(0);
    public final long id = nextMessageId();
    public final StgTSO source;
    public final StgTSO target;
    public volatile StgClosure exception;
    private static final AtomicReferenceFieldUpdater<MessageThrowTo, StgClosure> exceptionUpdater = AtomicReferenceFieldUpdater.newUpdater(MessageThrowTo.class, StgClosure.class, "exception");

    public MessageThrowTo(final StgTSO source, final StgTSO target, final StgClosure exception) {
        this.source = source;
        this.target = target;
        this.exception = exception;
    }

    @Override
    public final void execute(Capability cap) {
        //lock the message
        StgClosure old = lock();
        // TODO: Refine the locking logic
        boolean success = cap.throwToMsg(this);
        if (success) {
            StgTSO source = this.source;
            done();
            cap.tryWakeupThread(source);
        } else {
            exception = old;
        }
    }

    public void done() {
        invalidate();
        //overwrite closure
        unlock();
    }

    public StgClosure lock() {
        int i = 0;
        do {
            do {
                StgClosure exception = exceptionUpdater.getAndSet(this, null);
                if (exception != null) return exception;
            } while (++i < SPIN_COUNT);
            Thread.yield();
        } while (true);
    }

    public boolean tryLock() {
        StgClosure exception = exceptionUpdater.getAndSet(this, null);
        if (exception != null) return true;
        else return false;
    }

    public void unlock() {
        exception = null;
    }

    public long nextMessageId() {
        return maxMessageId.getAndIncrement();
    }
}
