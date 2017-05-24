package eta.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Stg;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.RtsFun;
import eta.runtime.exception.StgException;
import eta.runtime.apply.Apply;
import static eta.runtime.stm.TRecState.TREC_ACTIVE;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;

public class STM {
    public static long TOKEN_BATCH_SIZE = 1024;
    public static long maxCommits = 0;
    public static AtomicBoolean tokenLocked = new AtomicBoolean(false);
    public static final boolean configUseReadPhase = false;
    public static final boolean doShake = false;
    public static int shakeCounter = 0;
    public static int shakeLimit = 1;

    public static RtsFun newTVar = new NewTVar();
    public static RtsFun readTVar = new ReadTVar();
    public static RtsFun readTVarIO = new ReadTVarIO();
    public static RtsFun writeTVar = new WriteTVar();
    public static RtsFun check = new Check();
    public static RtsFun atomically = new Atomically();
    public static RtsFun catchSTM = new CatchSTM();
    public static RtsFun catchRetry = new CatchRetry();
    public static RtsFun retry = new Retry();
    public static RtsFun block_stmwait  = new BlockStmWait();

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

    public static boolean watcherIsInvariant(StgClosure c) {
        //TODO: Better condition
        return (c.getClass() == StgAtomicInvariant.class);
    }

    public static boolean watcherIsTSO(StgClosure c) {
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
        System.out.println("Testing: " + trec.chunkStack.size() + " more : "+ trec.chunkStack.toArray());
        StgTRecChunk x = trec.chunkStack.peek();
        System.out.println("Content: "+ x.entries);
        System.out.println("The Tvar:" + tvar);
        System.out.println("TRec:" + trec);
        do {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            System.out.println(cit.hasPrevious());
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                System.out.println("Chunk:" + chunk);
                System.out.println("Entries:" + chunk.entries);
                for (TRecEntry entry: chunk.entries) {
                    System.out.println("Check:" + entry.tvar);
                    if (entry.tvar == tvar) {
                        result = new EntrySearchResult(trec, entry);
                        System.out.println("I am here:" + result);
                        break loop;
                    }
                }
            }
            trec = trec.enclosingTrec;
        } while (result == null && trec != null);
        System.out.println("Result:" + result);
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

    private static class NewTVar extends RtsFun {
        @Override
        public final void enter(StgContext context) {
            StgClosure init = context.R(1);
            context.R(1, new StgTVar(init));
        }
    }

    private static class ReadTVar extends RtsFun {
        @Override
        public final void enter(StgContext context) {
            Capability cap = context.myCapability;
            StgTSO tso = context.currentTSO;
            StgTVar tvar = (StgTVar) context.O(1);
            context.R(1, cap.stmReadTvar(tso.trec, tvar));
        }
    }

    private static class ReadTVarIO extends RtsFun {
        @Override
        public final void enter(StgContext context) {
            StgClosure result;
            StgTVar tvar = (StgTVar) context.O(1);
            do {
                result = tvar.currentValue;
            } while (!result.isTrecHeader());
            context.R(1, result);
        }
    }

    private static class WriteTVar extends RtsFun {
        @Override
        public final void enter(StgContext context) {
            Capability cap = context.myCapability;
            StgTSO tso = context.currentTSO;
            StgTVar tvar = (StgTVar) context.O(1);
            StgClosure newValue = context.R(1);
            cap.stmWriteTvar(tso.trec, tvar, newValue);
        }
    }

    private static class Check extends RtsFun {
        @Override
        public final void enter(StgContext context) {
            Capability cap = context.myCapability;
            StgTSO tso = context.currentTSO;
            StgClosure closure = context.R(1);
            cap.stmAddInvariantToCheck(tso.trec, closure);
        }
    }

    private static class Atomically extends RtsFun {
        @Override
        public final void enter(StgContext context) {
            StgTSO tso = context.currentTSO;
            StgTRecHeader oldTrec = tso.trec;
            if (oldTrec != null) {
                context.R(1, null); /* TODO: base_ControlziExceptionziBase_nestedAtomically_closure */
                StgException.raise.enter(context);
            } else {
                Capability cap = context.myCapability;
                StgClosure stm = context.R(1);
                StgTRecHeader newTrec = cap.stmStartTransaction(oldTrec);
                tso.trec = newTrec;
                tso.sp.add(new StgAtomicallyFrame(stm));
                Apply.ap_v_fast.enter(context);
            }
        }
    }

    private static class CatchSTM extends RtsFun {
        @Override
        public final void enter(StgContext context) {
            StgClosure code = context.R(1);
            StgClosure handler = context.R(2);
            Capability cap = context.myCapability;
            StgTSO tso = context.currentTSO;
            StgTRecHeader curTrec = tso.trec;
            StgTRecHeader newTrec = cap.stmStartTransaction(curTrec);
            tso.trec = newTrec;
            tso.sp.add(new StgCatchSTMFrame(code, handler));
            Apply.ap_v_fast.enter(context);
        }
    }

    private static class CatchRetry extends RtsFun {
        @Override
        public final void enter(StgContext context) {
            StgClosure firstCode = context.R(1);
            StgClosure altCode = context.R(2);
            Capability cap = context.myCapability;
            StgTSO tso = context.currentTSO;
            StgTRecHeader newTrec = cap.stmStartTransaction(tso.trec);
            tso.trec = newTrec;
            tso.sp.add(new StgCatchRetryFrame(firstCode, altCode));
            Apply.ap_v_fast.enter(context);
        }
    }

    private static class Retry extends RtsFun {
        @Override
        public final void enter(StgContext context) {
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

    private static class BlockStmWait extends  RtsFun {
        @Override
        public final void enter(StgContext context) {
            Capability cap = context.myCapability;
            StgTSO tso = context.currentTSO;
            StgTRecHeader trec = (StgTRecHeader) context.R(3);
            cap.stmWaitUnlock(trec);
            tso.whatNext = ThreadRunGHC;
            context.ret = ThreadBlocked;
            Stg.returnToSched.enter(context);
        }
    }
}
