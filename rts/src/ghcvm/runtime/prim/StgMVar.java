package ghcvm.runtime.prim;

import java.util.Deque;
import java.util.ArrayDeque;
import java.util.concurrent.atomic.AtomicBoolean;

import ghcvm.runtime.prim.StgMVar;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import static ghcvm.runtime.RtsMessages.barf;
import static ghcvm.runtime.thunk.StgWhiteHole.SPIN_COUNT;

public class StgMVar extends StgClosure {
    public Deque<StgTSO> tsoQueue = new ArrayDeque<StgTSO>();
    public StgClosure value;
    public AtomicBoolean lock = new AtomicBoolean(false);

    public StgMVar(StgClosure value) {
        this.value = value;
    }

    @Override
    public void enter(StgContext context) {
        barf("MVAR object entered!");
    }

    public void pushFirst(StgTSO tso) {
        tsoQueue.offerFirst(tso);
    }

    public void pushLast(StgTSO tso) {
        tsoQueue.offerLast(tso);
    }

    public StgTSO popFromQueue() {
        return tsoQueue.poll();
    }

    public final void lock() {
        int i = 0;
        do {
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

    public final boolean tryLock() {
        boolean old = lock.getAndSet(true);
        if (!old) return true;
        else return false;
    }
}
