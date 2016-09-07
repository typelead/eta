package ghcvm.runtime.thunk;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgEnter;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgAPStack;
import ghcvm.runtime.exception.StgRaise;

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

    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<StgClosure> raiseClosure, StgClosure exception) {
        StgClosure raise = raiseClosure.get();
        if (raise == null) {
            raise = new StgRaise(exception);
            raiseClosure.set(raise);
        }
        cap.updateThunk(tso, updatee, raise);
        return true;
    }
}
