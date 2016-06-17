package ghcvm.runtime.stm;

import java.util.Queue;
import java.util.ArrayDeque;

import ghcvm.runtime.stackframe.StackFrame;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class StgAtomicallyFrame extends StackFrame {
    public final StgClosure code;
    public final Queue<StgClosure> nextInvariantToCheck = new ArrayDeque<StgClosure>();
    public final StgClosure result;

    public StgAtomicallyFrame(final StgClosure code, final StgClosure result) {
        this.code = code;
        this.result = result;
    }

    @Override
    public void stackEnter(StgContext context) {
        // TODO: Implement
    }

}
