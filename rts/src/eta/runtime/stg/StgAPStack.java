package eta.runtime.stg;

import java.util.Stack;
import java.util.ListIterator;

import eta.runtime.thunk.StgThunk;
import eta.runtime.thunk.StgUpdateFrame;

public class StgAPStack extends StgThunk {
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
        super.enter(context);
        /* TODO: Verify that the order of frames is correct. */
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        sp.add(new StgUpdateFrame(this));
        ListIterator<StackFrame> it = stack.listIterator(stack.size());
        while (it.hasPrevious()) {
            sp.add(it.previous());
        }
        /* TODO: Make sure ENTER_R1 functionality is implemented correctly */
        context.R(1, fun);
    }

    @Override
    public void thunkEnter(StgContext context) {
        /* TODO: Implement */
    }
}
