package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.ListIterator;

import ghcvm.runtime.RtsFlags;
import ghcvm.runtime.stg.StgClosure;
import static ghcvm.runtime.stm.TRecState.TREC_ACTIVE;
import static ghcvm.runtime.stm.STM.EntrySearchResult;

public class StgTRecHeader extends StgClosure {
    public Stack<StgTRecChunk> chunkStack = new Stack<StgTRecChunk>();
    public Queue<StgInvariantCheck> invariantsToCheck = new ArrayDeque<StgInvariantCheck>();
    public TRecState state;
    public StgTRecHeader enclosingTrec;

    public StgTRecHeader() {
        this.chunkStack.push(new StgTRecChunk());
    }

    public void setEnclosing(StgTRecHeader enclosingTrec) {
        this.enclosingTrec = enclosingTrec;
        if (enclosingTrec == null) {
            this.state = TREC_ACTIVE;
        } else {
            this.state = enclosingTrec.state;
        }
    }

    @Override
    public boolean isTrecHeader() { return true; }

    public boolean checkReadOnly() {
        boolean result = true;
        if (RtsFlags.STM.fineGrained) {
            ListIterator<StgTRecChunk> cit = chunkIterator();
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
        ListIterator<StgTRecChunk> cit = chunkIterator();
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                StgTVar s = e.tvar;
                EntrySearchResult result = STM.getEntry(enclosingTrec, s);
                TRecEntry entry = result.entry;
                if (entry != null) {
                    e.expectedValue = entry.newValue;
                    e.newValue = entry.newValue;
                }
                /* TODO: Verify order */
                s.watchQueue.offer(inv);
            }
        }
        inv.lastExecution = this;
    }

    public ListIterator<StgTRecChunk> chunkIterator() {
        return chunkStack.listIterator(chunkStack.size());
    }
}
