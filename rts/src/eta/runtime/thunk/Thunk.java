package eta.runtime.thunk;

import java.util.ListIterator;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.RtsFun;
import eta.runtime.exception.StgRaise;
import eta.runtime.message.MessageBlackHole;

import static eta.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static eta.runtime.stg.StgTSO.WhyBlocked.BlockedOnBlackHole;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static eta.runtime.RtsMessages.debugBelch;

public class Thunk {
    public static Queue<StgIndStatic> revertibleCAFList = new ConcurrentLinkedQueue<StgIndStatic>();
    public static Queue<StgIndStatic> dynamicCAFList = new ConcurrentLinkedQueue<StgIndStatic>();
    public static Queue<StgIndStatic> debugCAFList = new ConcurrentLinkedQueue<StgIndStatic>();

    private static boolean keepCAFs;

    public static RtsFun block_blackhole = new BlockBlackhole();

    public static void setKeepCAFs() {
        keepCAFs = true;
    }

    public static boolean shouldKeepCAFs() {
        return keepCAFs;
    }

    public static void revertCAFs() {
        for (StgIndStatic c: revertibleCAFList) {
            c.indirectee = null;
            /* TODO: Verify that the code to revert the CAF
                     is correct. */
        }
        revertibleCAFList.clear();
    }

    private static class BlockBlackhole extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgTSO tso = context.currentTSO;
            StgClosure closure = context.R(1);
            ListIterator<StackFrame> sp = tso.sp;
            sp.add(new StgEnter(closure));
            tso.whatNext = ThreadRunGHC;
            context.ret = ThreadBlocked;
            Stg.returnToSched.enter(context);
        }
    }

    public static void blackHole(StgContext context, StgThunk blackhole) {
        do {
            StgClosure indirectee = blackhole.indirectee;
            StgClosure eval = indirectee.getEvaluated();
            if (eval != null) {
                context.R(1, eval);
            } else {
                /* TODO: Account for other special cases like StgAtomically */
                if (indirectee.getClass() == StgRaise.class) {
                    indirectee.enter(context);
                } else {
                    debugBelch("Eval: " + eval + " Indirectee: " + blackhole.indirectee);
                    /* TODO: Add a condition here to check the empty blocking queue case. */
                    Capability cap = context.myCapability;
                    StgTSO tso = context.currentTSO;
                    MessageBlackHole msg = new MessageBlackHole(tso, blackhole);
                    boolean blocked = cap.messageBlackHole(msg);
                    if (blocked) {
                        // TODO: When context-switching is done, make this block.
                        tso.whyBlocked = BlockedOnBlackHole;
                        tso.blockInfo = msg;
                        context.R(1, blackhole);
                        Thunk.block_blackhole.enter(context);
                    } else {
                        continue;
                    }
                }
            }
            break;
        } while (true);
    }
}
