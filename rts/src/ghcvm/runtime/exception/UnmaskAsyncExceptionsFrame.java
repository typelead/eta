package ghcvm.runtime.exception;

import ghcvm.runtime.Stg;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.stackframe.StackFrame;
import ghcvm.runtime.stackframe.ReturnClosure;
import static ghcvm.runtime.types.StgTSO.TSO_BLOCKEX;
import static ghcvm.runtime.types.StgTSO.TSO_INTERRUPTIBLE;
import static ghcvm.runtime.types.StgTSO.WhatNext.ThreadRunGHC;
import static ghcvm.runtime.types.StgTSO.WhatNext.ThreadKilled;

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
