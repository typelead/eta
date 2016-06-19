package ghcvm.runtime.stg;

import ghcvm.runtime.closure.*;
import ghcvm.runtime.apply.*;
import ghcvm.runtime.exception.*;
import ghcvm.runtime.thunk.*;

public class Stg {
    public static StgClosure mkWeak = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgClosure key = context.R2;
                StgClosure value = context.R3;
                StgClosure finalizer = context.R4;
                StgWeak weak = new StgWeak(key, value, finalizer);
                cap.weakPtrList.add(weak);
                context.R1 = weak;
            }
        };

    public static StgClosure returnToSched = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                cap.threadPaused(tso);
                throw StgException.stgReturnException;
            }
        };

    public static StgClosure returnToSchedButFirst = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                cap.threadPaused(tso);
                StgClosure first = context.R2;
                first.enter(context);
            }
        };

    public static StgClosure noDuplicate = new StgClosure() {
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
        };

    public static StgClosure threadFinished = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                context.ret = ThreadFinished;
                throw StgException.stgReturnException;
            }
        };
}
