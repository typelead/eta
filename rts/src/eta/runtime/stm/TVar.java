package eta.runtime.stm;

import java.util.Deque;
import java.util.ArrayDeque;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.util.UnsafeUtil.cas;

public class TVar extends StgValue {
    public volatile Closure currentValue;
    public Deque<Closure> watchQueue = new ArrayDeque<Closure>();
    public int numUpdates;

    public TVar(Closure currentValue) {
        this.currentValue = currentValue;
    }

    @Override
    public final Closure enter(StgContext context) {
        barf("TVAR object entered!");
        return null;
    }

    public boolean condLock(StgTRecHeader trec, Closure expected) {
        // TODO: Implement the lock based on locking type of STM
        return (currentValue == expected);
    }

    public boolean isLocked(StgTRecHeader trec){
        return (currentValue == trec);
    }

    public void unlock(StgTRecHeader trec, Closure c, boolean forceUpdate) {
        currentValue = c;
    }

    public Closure lock(StgTRecHeader trec) {
        // TODO: Implement different forms of locks
        Closure result;
        do {
            do {
                result = currentValue;
            } while (result instanceof StgTRecHeader);
        } while (!cas(this, result, trec));
        return result;
    }
}
