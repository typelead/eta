package ghcvm.runtime.closure;

import java.util.Iterator;

public class StackFrame extends StgClosure {

    @Override
    public void enter(StgContext context) {
        Iterator<StackFrame> it = context.it;
        if (it != null && it.hasNext()) {
            it.next().enter(context);
        }
    }
}
