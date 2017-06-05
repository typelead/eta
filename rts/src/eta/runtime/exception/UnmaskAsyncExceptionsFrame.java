package eta.runtime.exception;

import java.util.ListIterator;

import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.ReturnClosure;
import static eta.runtime.stg.StgTSO.TSO_BLOCKEX;
import static eta.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadKilled;

public class UnmaskAsyncExceptionsFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        StgClosure ret = context.R(1);
        ListIterator<StackFrame> sp = tso.sp;
        tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        if (!tso.blockedExceptions.isEmpty()) {
            sp.add(new ReturnClosure(ret));
            boolean performed = cap.maybePerformBlockedException(tso);
            if (performed) {
                if (tso.whatNext == ThreadKilled) {
                    Stg.threadFinished(context);
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
