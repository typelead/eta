package ghcvm.runtime.closure;

import java.util.Stack;
import java.util.ListIterator;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.thunk.StgUpdateFrame;
import ghcvm.runtime.stg.StackFrame;

public class StgAPStack extends StgInd {
    public final StgClosure fun;
    public final Stack<StackFrame> stack = new Stack<StackFrame>();

    public StgAPStack() {
        this.fun = fun;
        this.arity = arity;
    }

    @Override
    public void enter(StgContext context) {
        /* TODO: Verify that the order of frames is correct. */
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        sp.add(new StgUpdateFrame(this));
        ListIterator<StackFrame> it = stack.listIterator(stack.size());
        while (it.hasPrevious()) {
            sp.add(it.previous());
        }
        context.R1 = fun;
    }
}
