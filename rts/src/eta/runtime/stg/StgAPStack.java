package eta.runtime.stg;

import java.util.Stack;
import java.util.ListIterator;

import eta.runtime.thunk.Thunk;

public class StgAPStack extends Thunk {
    public final Closure fun;
    public final Stack<StackFrame> stack;

    public StgAPStack(final Closure fun, final Stack<StackFrame> stack) {
        /* TODO: Do proper delegation to super class */
        super(null);
        this.fun = fun;
        this.stack = stack;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("Unimplemented StgAPStack")
        super.enter(context);
        /* TODO: Verify that the order of frames is correct. */
        TSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        // sp.add(new StgUpdateFrame(this));
        ListIterator<StackFrame> it = stack.listIterator(stack.size());
        while (it.hasPrevious()) {
            sp.add(it.previous());
        }
        /* TODO: Make sure ENTER_R1 functionality is implemented correctly */
        return fun;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        /* TODO: Implement */
        return null;
    }
}
