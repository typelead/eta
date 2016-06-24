package ghcvm.runtime.thunk;

import java.util.Stack;
import java.util.ListIterator;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgEnter;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgAPStack;

public abstract class UpdateFrame extends StackFrame {
    public final StgThunk updatee;

    public UpdateFrame(final StgThunk updatee) {
        this.updatee = updatee;
    }

    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee) {
        Stack<StackFrame> stack = new Stack<StackFrame>();
        ListIterator<StackFrame> sp = tso.sp;
        /* ASSUMPTION: There are always a minimum of two frames
                       after sp */
        do {
            stack.push(sp.next());
            sp.remove();
        } while (sp.hasNext());
        StackFrame top = stack.pop();
        StgClosure fun = top.getClosure();
        StgAPStack ap = new StgAPStack(fun, stack);
        /* TODO: Why create an ap just to set it to the updatee? */
        if (this.updatee == updatee) {
            ap = (StgAPStack) updatee;
        } else {
            cap.updateThunk(tso, this.updatee, ap);
        }
        sp.previous();
        sp.remove();
        sp.add(new StgEnter(ap));
        sp.previous();
        return true;
    }
}
