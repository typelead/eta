package ghcvm.runtime.exception;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.exception.StgException;
import static ghcvm.runtime.closure.StgContext.ReturnCode.ThreadFinished;
import static ghcvm.runtime.types.StgTSO.*;

public class UnmaskAsyncExceptionsFrame extends StackFrame {

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        StgTSO tso = context.currentTSO;
        StgClosure ret = context.R1;
        // TODO: Verify stack operations
        tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        if (!tso.blockedExceptions.isEmpty()) {
            context.sp.add(new ReturnClosure(ret));
            boolean performed = context.myCapability.maybePerformBlockedException(tso);
            if (performed) {
                if (tso.whatNext = ThreadKilled) {
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
                context.sp.remove();
            }
        }
        context.sp.remove();
        context.R1 = ret;
    }
}
