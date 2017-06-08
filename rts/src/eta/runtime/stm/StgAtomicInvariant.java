package eta.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.Closure;

public class StgAtomicInvariant {
    public Closure code;
    public StgTRecHeader lastExecution;
    public AtomicBoolean lock = new AtomicBoolean(false);

    public StgAtomicInvariant(Closure code) {
        this.code = code;
    }

    public final boolean lock() {
        return lock.compareAndSet(false,true);
    }

    public final void unlock() {
        lock.set(false);
    }

    public final void disconnect() {
        /* ASSERT (lastExecution != null) */

        ListIterator<StgTRecChunk> cit = lastExecution.chunkIterator();
        loop:
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                TVar s = e.tvar;
                s.watchQueue.remove(this);
            }
        }
        lastExecution = null;
    }
}
