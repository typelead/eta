package eta.runtime.stm;

import java.util.Deque;
import java.util.ArrayDeque;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.util.UnsafeUtil.cas;

public class StgTVar extends StgClosure {
    public volatile StgClosure currentValue;
    public Deque<StgClosure> watchQueue = new ArrayDeque<StgClosure>();
    public int numUpdates;

    public StgTVar(StgClosure currentValue) {
        this.currentValue = currentValue;
    }

    @Override
    public StgClosure getEvaluated() { return this; }

    @Override
    public final void enter(StgContext context) {
        barf("TVAR object entered!");
    }

    public boolean condLock(StgTRecHeader trec, StgClosure expected) {
        // TODO: Implement the lock based on locking type of STM
        return (currentValue == expected);
    }

    public boolean isLocked(StgTRecHeader trec){
        return (currentValue == trec);
    }

    public void unlock(StgTRecHeader trec, StgClosure c, boolean forceUpdate) {
        currentValue = c;
    }

    public StgClosure lock(StgTRecHeader trec) {
        // TODO: Implement different forms of locks
        StgClosure result;
        do {
            do {
                result = currentValue;
            } while (result.isTrecHeader());
        } while (!cas(this, result, trec));
        return result;
    }
}
