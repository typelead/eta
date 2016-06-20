package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import static ghcvm.runtime.concurrent.Concurrent.SPIN_COUNT;

public class StgWhiteHole extends StgClosure {
    public static final StgWhiteHole closure = new StgWhiteHole();

    @Override
    public final boolean isEvaluated() { return false; }

    @Override
    public void enter(StgContext context) {
        StgIndStatic node = (StgIndStatic) context.R1;
        int i = 0;
        while (node.isLocked()) {
            i = i + 1;
            if (i == SPIN_COUNT) {
                i = 0;
                Thread.yield();
            }
        }
        node.enter(context);
    }
}
