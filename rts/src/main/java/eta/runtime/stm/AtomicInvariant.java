package eta.runtime.stm;

import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.Closure;

public class AtomicInvariant {
    public Closure code;
    public TransactionRecord lastExecution;
    public AtomicBoolean lock = new AtomicBoolean(false);

    public AtomicInvariant(Closure code) {
        this.code = code;
    }

    public final boolean lock() {
        return lock.compareAndSet(false,true);
    }

    public final void unlock() {
        lock.set(false);
    }

    public final void disconnect() {
        for (TransactionEntry e:lastExecution) {
            TVar s = e.tvar;
            s.removeInvariant(this);
        }
        lastExecution = null;
    }
}
