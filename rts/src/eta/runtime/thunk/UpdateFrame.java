package eta.runtime.thunk;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgAPStack;
import eta.runtime.exception.StgRaise;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Default;

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

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<StgClosure> raiseClosure, StgClosure exception) {
        StgClosure raise = raiseClosure.get();
        if (raise == null) {
            raise = new StgRaise(exception);
            raiseClosure.set(raise);
        }
        cap.updateThunk(tso, updatee, raise);
        return true;
    }

    @Override
    public MarkFrameResult mark(Capability cap, StgTSO tso) {
        ListIterator<StackFrame> sp = tso.sp;
        sp.set(new StgMarkedUpdateFrame(updatee));
        StgThunk bh = updatee;
        do {
            StgClosure oldIndirectee = bh.indirectee;
            if (bh.getEvaluated() == null && bh.indirectee != tso) {
                cap.suspendComputation(tso, this);
                // TODO: Verify that suspendComputation deletes all frames above this one
                sp.next();
                sp.set(new StgEnter(bh));
                sp.previous();
                return Default;
            } else {
                if (RtsFlags.ModeFlags.threaded) {
                    if (!bh.tryLock(oldIndirectee)) {
                        continue;
                    }
                }
                bh.updateWithIndirection(tso);
                return Default;
            }
        } while (true);
    }
}
