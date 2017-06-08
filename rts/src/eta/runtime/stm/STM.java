package eta.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Stg;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.exception.StgException;

import static eta.runtime.stm.TRecState.TREC_ACTIVE;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadRun;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;

public class STM {
    public static long TOKEN_BATCH_SIZE = 1024;
    public static long maxCommits = 0;
    public static AtomicBoolean tokenLocked = new AtomicBoolean(false);
    public static final boolean configUseReadPhase = false;
    public static final boolean doShake = false;
    public static int shakeCounter = 0;
    public static int shakeLimit = 1;

    public static boolean shake() {
        if (doShake) {
            if (((shakeCounter++) % shakeLimit) == 0) {
                shakeCounter = 1;
                shakeLimit++;
            }
        }
        return false;
    }

    public static void lock(StgTRecHeader trec) {}
    public static void unlock(StgTRecHeader trec) {}

    public static boolean watcherIsInvariant(Closure c) {
        //TODO: Better condition
        return (c.getClass() == StgAtomicInvariant.class);
    }

    public static boolean watcherIsTSO(Closure c) {
        //TODO: Better condition
        return (c.getClass() == StgTSO.class);
    }

    public static class EntrySearchResult {
        public final StgTRecHeader header;
        public final TRecEntry entry;
        public EntrySearchResult(final StgTRecHeader header, final TRecEntry entry) {
            this.header = header;
            this.entry = entry;
        }
    }

    public static EntrySearchResult getEntry(StgTRecHeader trec, StgTVar tvar) {
        EntrySearchResult result = null;
        do {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TRecEntry entry: chunk.entries) {
                    if (entry.tvar == tvar) {
                        result = new EntrySearchResult(trec, entry);
                        break loop;
                    }
                }
            }
            trec = trec.enclosingTrec;
        } while (result == null && trec != null);
        return result;
    }

    public static Closure readCurrentValue(StgTRecHeader trec, StgTVar tvar) {
        Closure result = tvar.currentValue;
        if (RtsFlags.STM.fineGrained) {
            while (result instanceof StgTRecHeader) {
                result = tvar.currentValue;
            }
        }
        return result;
    }

    /* TODO: Inline this */
    public static void newTVar(StgContext context, Closure init) {
        context.R(1, new StgTVar(init));
    }

    public static void readTVar(StgContext context, StgTVar tvar) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        context.R(1, cap.stmReadTvar(tso.trec, tvar));
    }

    public static void readTVarIO(StgContext context, StgTVar tvar) {
        Closure result;
        do {
            result = tvar.currentValue;
        } while (!(result instanceof StgTRecHeader));
        context.R(1, result);
    }

    public static void writeTVar(StgContext context, StgTVar tvar, Closure newValue) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        cap.stmWriteTvar(tso.trec, tvar, newValue);
    }

    public static void check(StgContext context, Closure invariant) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        cap.stmAddInvariantToCheck(tso.trec, invariant);
    }

    private static Closure nestedAtomically_closure = null;

    static {
        try {
            nestedAtomically_closure = (Closure)
                Class.forName("base.control.exception.Base")
                .getMethod("nestedAtomically_closure")
                .invoke(null);
        } catch (Exception e) {
            e.printStackTrace();
            nestedAtomically_closure = null;
        }
    }

    public static void atomically(StgContext context, Closure stm) {
        StgTSO tso = context.currentTSO;
        StgTRecHeader oldTrec = tso.trec;
        if (oldTrec != null) {
            StgException.raise(context, nestedAtomically_closure);
        } else {
            Capability cap = context.myCapability;
            StgTRecHeader newTrec = cap.stmStartTransaction(oldTrec);
            tso.trec = newTrec;
            tso.sp.add(new StgAtomicallyFrame(stm));
            stm.applyV(context);
        }
    }

    public static void catchSTM(StgContext context, Closure code, Closure handler) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        StgTRecHeader curTrec = tso.trec;
        StgTRecHeader newTrec = cap.stmStartTransaction(curTrec);
        tso.trec = newTrec;
        tso.sp.add(new StgCatchSTMFrame(code, handler));
        code.applyV(context);
    }

    public static void catchRetry(StgContext context, Closure firstCode, Closure altCode) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        StgTRecHeader newTrec = cap.stmStartTransaction(tso.trec);
        tso.trec = newTrec;
        tso.sp.add(new StgCatchRetryFrame(firstCode, altCode));
        firstCode.applyV(context);
    }

    public static void retry(StgContext context) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        StgTRecHeader trec = tso.trec;
        /* findRetryFrameHelper will arrange the stack pointer so
            that sp.next() should point to the desired frame */
        boolean retry = false;
        do {
            StgSTMFrame stmFrame = (StgSTMFrame) cap.findRetryFrameHelper(tso);
            retry = stmFrame.doRetry(cap, tso, trec);
        } while (retry);
    }
}
