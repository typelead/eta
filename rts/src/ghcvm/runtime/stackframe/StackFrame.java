package ghcvm.runtime.stackframe;

import java.util.Iterator;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class StackFrame extends StgClosure {

    @Override
    public void enter(StgContext context) {
        Iterator<StackFrame> it = context.it;
        if (it != null && it.hasNext()) {
            it.next().enter(context);
        }
    }
}
