package eta.runtime.message;

import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.exception.Exception;
import static eta.runtime.stg.TSO.WhyBlocked.*;

public class MessageThrowTo extends Message {
    public final TSO source;
    public final TSO target;
    public final  Closure exception;
    public volatile AtomicBoolean lock = new AtomicBoolean(false);

    public MessageThrowTo(final TSO source, final TSO target, final Closure exception) {
        this.source = source;
        this.target = target;
        this.exception = exception;
    }

    @Override
    public void execute(Capability cap) {
        if (!isValid()) return;
        lock();
        assert source.whyBlocked == BlockedOnMsgThrowTo;
        assert source.blockInfo  == this;
        boolean success = Exception.throwToMsg(cap, this, true);
        if (success) {
            done();
        } else  {
            unlock();
        }
    }

    public void done() {
        invalidate();
        unlock();
    }

    /** Locking Mechanisms **/

    public final void lock() {
        while (!lock.compareAndSet(false, true)) {}
    }

    public final void unlock() {
        lock.set(false);
    }

    @Override
    public final boolean isLocked() {
        return lock.get();
    }

    public final boolean tryLock() {
        return lock.compareAndSet(false, true);
    }

    public final boolean tryUnlock() {
        return lock.compareAndSet(true, false);
    }

    @Override
    public String toString() {
        return "MessageThrowTo[source=" + source + ",target=" + target
             + ",exception=" + exception + "]";
    }
}
