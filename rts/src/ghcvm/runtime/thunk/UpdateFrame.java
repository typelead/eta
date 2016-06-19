package ghcvm.runtime.thunk;

import ghcvm.runtime.stackframe.StackFrame;

public abstract class UpdateFrame extends StackFrame {
    public final StgInd updatee;

    public UpdateFrame(final StgInd updatee) {
        this.updatee = updatee;
    }

    @Override
    public RaiseAsyncResult doRaiseAsync() {
        Stack<StackFrame> stack = new Stack<StackFrame>();
        StgClosure fun = sp.previous();
        int spIndex = sp.nextIndex();
        int frameIndex = frameIt.nextIndex();
        while (frameIndex < spIndex) {
            stack.push(sp.previous());
            spIndex--;
        }
        StgAPStack ap = new StgAPStack(fun, stack);
        if (this.updatee == updatee) {
            ap = (StgAPStack) updatee;
        } else {
            cap.updateThunk(tso, this.updatee, ap);
        }
        sp.previous();
        sp.push(new StgEnter(ap));
        return Next;
    }
}
