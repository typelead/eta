package eta.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.ListIterator;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Closure;
import static eta.runtime.stm.TRecState.TREC_ACTIVE;
import static eta.runtime.stm.STM.EntrySearchResult;

public class TransactionRecord {
    public Stack<StgTRecChunk> chunkStack = new Stack<StgTRecChunk>();
    public Queue<InvariantCheck> invariantsToCheck = new ArrayDeque<InvariantCheck>();
    public TRecState state;
    public TransactionRecord enclosingTrec;

    public TransactionRecord() {
        this(null);
    }

    public TransactionRecord(TransactionRecord enclosingTrec) {
        this.chunkStack.push(new StgTRecChunk());
        this.enclosingTrec = enclosingTrec;
    }

    public void setEnclosing(TransactionRecord enclosingTrec) {
        this.enclosingTrec = enclosingTrec;
        if (enclosingTrec == null) {
            this.state = TREC_ACTIVE;
        } else {
            this.state = enclosingTrec.state;
        }
    }

    public boolean checkReadOnly() {
        boolean result = true;
        if (RtsFlags.STM.fineGrained) {
            ListIterator<StgTRecChunk> cit = chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TRecEntry e: chunk.entries) {
                    TVar s = e.tvar;
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

    public final void connectInvariant(AtomicInvariant inv) {
        /* ASSERT (inv.lastExection == null) */
        ListIterator<StgTRecChunk> cit = chunkIterator();
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                TVar s = e.tvar;
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
