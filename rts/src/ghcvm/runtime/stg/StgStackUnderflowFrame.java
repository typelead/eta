package ghcvm.runtime.stg;

public abstract class StgStackUnderflowFrame extends StackFrame {
    public final Stack<StackFrame> nextChunk;

    public UpdateFrame(final StgInd updatee) {
        this.updatee = updatee;
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
    public RaiseAsyncResult doRaiseAsync() {
        return Next;
    }
}
