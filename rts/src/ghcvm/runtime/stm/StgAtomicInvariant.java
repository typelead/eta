package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import ghcvm.runtime.closure.StgClosure;

public class StgAtomicInvariant extends StgClosure {
    public StgClosure code;
    public StgTRecHeader lastExecution;
    public AtomicBoolean lock = new AtomicBoolean(false);

    public StgAtomicInvariant(StgClosure code) {
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
        Stack<StgTRecChunk> chunkStack = lastExecution.chunkStack;
        ListIterator<StgTRecChunk> cit = chunkStack.listIterator(chunkStack.size());
        loop:
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                StgTVar s = e.tvar;
                s.watchQueue.remove(this);
            }
        }
        lastExecution = null;
    }
}
