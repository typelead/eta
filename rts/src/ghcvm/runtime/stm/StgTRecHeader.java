package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.ListIterator;

import ghcvm.runtime.RtsFlags;
import ghcvm.runtime.closure.StgClosure;

public class StgTRecHeader extends StgClosure {
    public Stack<StgTRecChunk> chunkStack = new Stack<StgTRecChunk>();
    public Queue<StgInvariantCheck> invariantsToCheck = new ArrayDeque<StgInvariantCheck>();
    public TRecState state;

    public StgTRecHeader() {
        this.chunkStack.push(new StgTRecChunk());
    }

    @Override
    public boolean isTrecHeader() { return true; }

    public boolean checkReadOnly() {
        boolean result = true;
        if (RtsFlags.STM.fineGrained) {
            ListIterator<StgTRecChunk> cit = chunkStack.listIterator(chunkStack.size());
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TRecEntry e: chunk.entries) {
                    StgTVar s = e.tvar;
                    if (e.isReadOnly()) {
                        if (s.currentValue != e.expectedValue ||
                            s.numUpdates != e.numUpdates) {
                            result = false;
                            break loop;
                        }
                    }
                }
            }
        }
        return result;
    }

    public final void connectInvariant(StgAtomicInvariant inv) {
        /* ASSERT (inv.lastExection == null) */
        ListIterator<StgTRecChunk> cit = chunkStack.listIterator(chunkStack.size());
        loop:
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                StgTVar s = e.tvar;
                Queue<StgClosure> watchQueue = new ArrayDeque<StgClosure>();
                watchQueue.offer(inv);
                // TODO: Incomplete
            }
        }
    }
}
