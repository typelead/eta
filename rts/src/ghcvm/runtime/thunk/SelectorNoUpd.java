package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.apply.Apply;

public class SelectorNoUpd extends StgThunk {
    protected final int index;
    protected final StgClosure p;

    public SelectorNoUpd(int i, StgClosure p) {
        super();
        this.index = i;
        this.p = p;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        int index = context.stackTopIndex();
        StackFrame frame = context.stackTop();
        p.evaluate(context);
        context.checkForStackFrames(index, frame);
        /* TODO: Do something based on the return value; */
    }
}
