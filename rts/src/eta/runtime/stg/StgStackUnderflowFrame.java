package eta.runtime.stg;

import java.util.Stack;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.thunk.StgThunk;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Stop;

public abstract class StgStackUnderflowFrame extends StackFrame {
    public final Stack<StackFrame> nextChunk;

    public StgStackUnderflowFrame(Stack<StackFrame> nextChunk) {
        this.nextChunk = nextChunk;
    }

    @Override
    public void stackEnter(StgContext context) {
        barf("stackEnter: StgStackUnderflowFrame unimplemented");
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        // Some return
        cap.threadStackUnderflow(tso);
        // load_thread_state
    }

    @Override

    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<StgClosure> topClosure) {
        /* TODO: Implement when dealing with StackOverflowError */
        barf("doRaiseAsync: StgStackUnderflowFrame unimplemented");
        return true;
    }

    @Override
    public boolean doFindRetry(Capability cap, StgTSO tso) {
        barf("doFindRetry: StgStackUnderflowFrame unimplemented");
        tso.sp.next();
        cap.threadStackUnderflow(tso);
        /* TODO: Figure out how to arrange the stack here.
                 Implement when dealing with StackOverflows. */
        return true;
    }

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<StgClosure> raiseClosure, StgClosure exception) {
        barf("doRaiseExceptionHelper: StgStackUnderflowFrame unimplemented");
        return true;
    }

    @Override
    public MarkFrameResult mark(Capability cap, StgTSO tso) { return Stop; }
}
