package eta.runtime.stm;

import java.util.Deque;
import java.util.ArrayDeque;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.util.UnsafeUtil.cas;

public class TVar extends Value {
    public volatile Closure currentValue;
    public Deque<Closure> watchQueue = new ArrayDeque<Closure>();
    public int numUpdates;

    public TVar(Closure currentValue) {
        this.currentValue = currentValue;
    }

    public Closure currentValue() {
        /* TODO: Remove the CAS operation? */
        Closure result;
        do {
            result = currentValue;
        } while (result instanceof TransactionRecord);
        return result;
    }

    @Override
    public final Closure enter(StgContext context) {
        barf("TVAR object entered!");
        return null;
    }

    public boolean condLock(TransactionRecord trec, Closure expected) {
        // TODO: Implement the lock based on locking type of STM
        return (currentValue == expected);
    }

    public boolean isLocked(TransactionRecord trec){
        return (currentValue == trec);
    }

    public void unlock(TransactionRecord trec, Closure c, boolean forceUpdate) {
        currentValue = c;
    }

    public Closure lock(TransactionRecord trec) {
        Closure result;
        do {
            result = currentValue();
        } while (!cas(this, result, trec));
        return result;
    }
}
