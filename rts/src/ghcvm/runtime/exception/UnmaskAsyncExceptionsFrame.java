package ghcvm.runtime.exception;

import java.util.ListIterator;

import ghcvm.runtime.stg.Stg;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.ReturnClosure;
import static ghcvm.runtime.stg.StgTSO.TSO_BLOCKEX;
import static ghcvm.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadKilled;

public class UnmaskAsyncExceptionsFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        StgClosure ret = context.R1;
        ListIterator<StackFrame> sp = tso.sp;
        tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        if (!tso.blockedExceptions.isEmpty()) {
            sp.add(new ReturnClosure(ret));
            boolean performed = cap.maybePerformBlockedException(tso);
            if (performed) {
                if (tso.whatNext == ThreadKilled) {
                    Stg.threadFinished.enter(context);
                } else {
                    /* TODO: Verify R1 is conserved on the next
                             stack reload. */
                    throw StgException.stackReloadException;
                }
            } else {
                sp.previous();
                sp.remove();
            }
        } else {
            /* Jump to top of stack with R1 = ret.
               As that is already the case, no need
               for wasted instructions. */
        }
    }
}
