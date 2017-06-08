package eta.runtime.thunk;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgAPStack;
import eta.runtime.exception.StgRaise;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Default;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Marked;

public abstract class UpdateFrame extends StackFrame {
    public final StgThunk updatee;
    public volatile boolean marked;

    public UpdateFrame(final StgThunk updatee) {
        this.updatee = updatee;
    }

    @Override
    public void stackEnter(StgContext context) {
        barf("Unimplemented");
    }

    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, Closure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<Closure> topClosure) {
        barf("Unimplemented");
    }

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<Closure> raiseClosure, Closure exception) {
        barf("Unimplemented");
        return false;
    }

    @Override
    public MarkFrameResult mark(Capability cap, StgTSO tso) {
        barf("Unimplemented");
        return null;
    }
}
