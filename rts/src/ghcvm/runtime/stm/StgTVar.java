package ghcvm.runtime.stm;

import java.util.Deque;
import java.util.ArrayDeque;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.RtsMessages.barf;
import static ghcvm.runtime.util.UnsafeUtil.cas;

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
