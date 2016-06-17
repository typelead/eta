package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;

import ghcvm.runtime.RtsFlags;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.types.Capability;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class STM {

    public static class EntrySearchResult {
        public final StgTRecHeader header;
        public final TRecEntry entry;
        public EntrySearchResult(final StgTRecHeader header, final TRecEntry entry) {
            this.header = header;
            this.entry = entry;
        }
    }

    public static EntrySearchResult getEntry(Stack<StgTRecHeader> trecStack, StgTVar tvar) {

        ListIterator<StgTRecHeader> it = trecStack.listIterator(trecStack.size());
        EntrySearchResult result = null;
        loop:
        while (result == null && it.hasPrevious()) {
            StgTRecHeader trec = it.previous();
            Stack<StgTRecChunk> chunkStack = trec.chunkStack;
            ListIterator<StgTRecChunk> cit = chunkStack.listIterator(chunkStack.size());
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TRecEntry entry: chunk.entries) {
                    // Traversal
                    if (entry.tvar == tvar) {
                        result = new EntrySearchResult(trec, entry);
                        break loop;
                    }
                }
            }
        }
        return result;
    }

    public static StgClosure readCurrentValue(StgTRecHeader trec, StgTVar tvar) {
        StgClosure result = tvar.currentValue;
        if (RtsFlags.STM.fineGrained) {
            while (result.isTrecHeader()) {
                result = tvar.currentValue;
            }
        }
        return result;
    }

    public StgClosure newTVar = new StgClosure() {
            @Override
            public final void enter(StgContext context) {
                StgClosure init = context.R1;
                context.R1 = new StgTVar(init);
            }
        };

    public StgClosure readTVar = new StgClosure() {
            @Override
            public final void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                StgTVar tvar = (StgTVar) context.R1;
                context.R1 = cap.stmReadTvar(tso.trec, tvar);
            }
        };

    public StgClosure readTVarIO = new StgClosure() {
            @Override
            public final void enter(StgContext context) {
                StgClosure result;
                StgTVar tvar = (StgTVar) context.R1;
                do {
                    result = tvar.currentValue;
                    // TODO: Improve this condition - maybe reuse isEvaluated?
                } while (result.getClass() != StgTRecHeader.class);
                context.R1 = result;
            }
        };
}
