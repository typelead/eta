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
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<StgClosure> topClosure) {
        Stack<StackFrame> stack = new Stack<StackFrame>();
        ListIterator<StackFrame> sp = tso.sp;
        sp.next(); //Shift to above the update frame
        do {
            stack.push(sp.next());
            sp.remove();
        } while (sp.hasNext());
        StgClosure fun = topClosure.get();
        StgClosure ap = new StgAPStack(fun, stack);
        if (this.updatee == updatee) {
            ap = updatee;
        } else {
            cap.updateThunk(tso, this.updatee, ap);
        }
        tso.spPop();
        topClosure.set(ap);
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
