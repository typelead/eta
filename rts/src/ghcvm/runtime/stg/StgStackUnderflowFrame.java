package ghcvm.runtime.stg;

import java.util.Stack;

import ghcvm.runtime.thunk.StgInd;

public abstract class StgStackUnderflowFrame extends StackFrame {
    public final Stack<StackFrame> nextChunk;

    public StgStackUnderflowFrame(Stack<StackFrame> nextChunk) {
        this.nextChunk = nextChunk;
    }

    @Override
    public void stackEnter(StgContext context) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        // Some return
        cap.threadStackUnderflow(tso);
        // load_thread_state
    }

    @Override

    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgInd updatee) {
        /* TODO: Implement when dealing with StackOverflowError */
        return true;
    }
}
