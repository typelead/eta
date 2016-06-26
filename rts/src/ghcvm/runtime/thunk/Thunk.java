package ghcvm.runtime.thunk;

import java.util.ListIterator;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import ghcvm.runtime.stg.Stg;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgEnter;
import ghcvm.runtime.message.MessageBlackHole;

import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.BlockedOnBlackHole;
import static ghcvm.runtime.stg.StgContext.ReturnCode.ThreadBlocked;

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

    public static void revertCAFs() {
        for (StgIndStatic c: revertibleCAFList) {
            c.indirectee = null;
            /* TODO: Verify that the code to revert the CAF
                     is correct. */
        }
        revertibleCAFList.clear();
    }

    public static StgClosure block_blackhole = new StgClosure() {
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
        };

    public static void blackHole(StgContext context, StgThunk blackhole,  StgClosure indirectee) {
        do {
            StgClosure eval = indirectee.getEvaluated();
            if (eval != null) {
                context.R(1, eval);
            } else {
                /* TODO: Add a condition here to check the empty blocking queue case. */
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                MessageBlackHole msg = new MessageBlackHole(tso, blackhole);
                boolean blocked = cap.messageBlackHole(msg);
                if (blocked) {
                    tso.whyBlocked = BlockedOnBlackHole;
                    tso.blockInfo = msg;
                    context.R(1, blackhole);
                    Thunk.block_blackhole.enter(context);
                } else {
                    continue;
                }
            }
        } while (false);
    }
}
