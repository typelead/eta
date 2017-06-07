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

    public static void setKeepCAFs() {
        keepCAFs = true;
    }

    public static boolean shouldKeepCAFs() {
        return keepCAFs;
    }

    public static synchronized void revertCAFs() {
        for (StgIndStatic c: revertibleCAFList) {
            c.indirectee = null;
        }
        revertibleCAFList.clear();
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
                    Thread.yield();
                    continue;
                    // TODO: When context-switching is done, make this block.
                    /* TODO: Add a condition here to check the empty blocking queue case. */
                    // Capability cap = context.myCapability;
                    // StgTSO tso = context.currentTSO;
                    // MessageBlackHole msg = new MessageBlackHole(tso, blackhole);
                    // boolean blocked = cap.messageBlackHole(msg);
                    // if (blocked) {
                    //     tso.whyBlocked = BlockedOnBlackHole;
                    //     tso.blockInfo = msg;
                    //     context.R(1, blackhole);
                    //     Thunk.block_blackhole.enter(context);
                    // } else {
                    //     continue;
                    // }
                }
            }
            break;
        } while (true);
    }
}
