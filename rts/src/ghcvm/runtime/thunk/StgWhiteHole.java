package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.thunk.StgWhiteHole;
import static ghcvm.runtime.concurrent.Concurrent.SPIN_COUNT;

public class StgWhiteHole extends StgClosure {
    public static final StgWhiteHole closure = new StgWhiteHole();

    @Override
    public void enter(StgContext context) {
        StgIndStatic node = (StgIndStatic) context.R(1);
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
