package ghcvm.runtime.stackframe;

import java.util.ListIterator;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public abstract class StackFrame extends StgClosure {

    @Override
    public final void enter(StgContext context) {
        ListIterator<StackFrame> sp = context.sp;
        if (sp.hasNext()) {
            sp.next().enter(context);
        }
        stackEnter();
        sp.remove();
    }

    public abstract void stackEnter(StgContext context);
    public MarkFrameResult mark() { return new MarkFrameResult};

    public enum class MarkFrameResult {
        Marked, Stop, Default, Update, UpdateEvaluted
    }
}
