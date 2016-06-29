package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.thunk.StgWhiteHole;
import static ghcvm.runtime.concurrent.Concurrent.SPIN_COUNT;

public class StgWhiteHole extends StgClosure {
    /* TODO: Extend from something else instead? */
    public static final StgWhiteHole closure = new StgWhiteHole();

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        StgIndStatic node = (StgIndStatic) context.R(2);
        int i = 0;
        /* TODO: Verify the condition */
        while (node.indirectee == closure) {
            i = i + 1;
            if (i == SPIN_COUNT) {
                i = 0;
                Thread.yield();
            }
        }
        node.enter(context);
    }
}
