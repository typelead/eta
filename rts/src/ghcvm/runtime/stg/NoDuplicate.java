package ghcvm.runtime.stg;

import java.util.ListIterator;

import ghcvm.runtime.Stg;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.types.Capability;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.stackframe.StackFrame;
import ghcvm.runtime.stackframe.NoDuplicateFrame;
import ghcvm.runtime.exception.StgException;
import ghcvm.runtime.exception.StgReturnException;
import static ghcvm.runtime.types.StgTSO.WhatNext.ThreadKilled;

public class NoDuplicate extends StgClosure {

    @Override
    public void enter(StgContext context) {
        if (Capability.nCapabilities != 1) {
            StgTSO tso = context.currentTSO;
            ListIterator<StackFrame> sp = context.sp;
            sp.add(new NoDuplicateFrame());
            context.myCapability.threadPaused(tso);
            if (tso.whatNext == ThreadKilled) {
                Stg.threadFinished.enter(context);
            } else {
                // TODO: Ensure stack logic is correct;
                StackFrame frame = sp.previous();
                if (frame.getClass().equals(NoDuplicateFrame.class)) {
                    sp.remove();
                }
                // Iterator<StackFrame> it = tso.stack.descendingIterator();
                // it.next().enter(context);
            }
        }
    }
}
