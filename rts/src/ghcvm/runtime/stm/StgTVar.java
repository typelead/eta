package ghcvm.runtime.stm;

import java.util.Deque;
import java.util.ArrayDeque;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import static ghcvm.runtime.RtsMessages.barf;

public class StgTVar extends StgClosure {
    // TODO: Is this volatile necessary?
    public volatile StgClosure currentValue;
    public Deque<StgClosure> watchQueue = new ArrayDeque<StgClosure>();
    public int numUpdates;

    public StgTVar(StgClosure currentValue) {
        this.currentValue = currentValue;
    }

    @Override
    public final void enter(StgContext context) {
        barf("TVAR object entered!");
    }
}
