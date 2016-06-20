package ghcvm.runtime.stg;

import java.util.Stack;
import java.util.ListIterator;

import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.thunk.StgUpdateFrame;

public class StgAPStack extends StgInd {
    public final StgClosure fun;
    public final Stack<StackFrame> stack;

    public StgAPStack(final StgClosure fun, final Stack<StackFrame> stack) {
        /* TODO: Do proper delegation to super class */
        super(null);
        this.fun = fun;
        this.stack = stack;
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

    @Override
    public void thunkEnter(StgContext context) {
        /* TODO: Implement */
    }
}
