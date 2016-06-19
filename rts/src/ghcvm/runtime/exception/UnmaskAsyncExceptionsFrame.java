package ghcvm.runtime.exception;

import ghcvm.runtime.Stg;
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
        // TODO: Verify stack operations
        tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        if (!tso.blockedExceptions.isEmpty()) {
            sp.add(new ReturnClosure(ret));
            boolean performed = cap.maybePerformBlockedException(tso);
            if (performed) {
                if (tso.whatNext == ThreadKilled) {
                    Stg.threadFinished.enter(context);
                    return;
                } else {
                    tso.whatNext = ThreadRunGHC;
                    context.R1 = ret;
                    return;
                    // Iterator<StackFrame> it = stack.descendingIterator();
                    // context.it = it;
                    // it.next().enter(context);
                }
            } else {
                /* TODO: Verify that the stack hasn't been modified by
                         maybePerformBlockedException() or this will
                         remove an unknown frame. */
                sp.remove();
            }
        } else {
            /* Jump to top of stack with R1 = ret.
               As that is already the case, no need
               for wasted instructions. */
        }
    }
}
