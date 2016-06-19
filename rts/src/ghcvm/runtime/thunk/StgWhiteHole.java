package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;

public class StgWhiteHole extends StgClosure {

    public static final StgWhiteHole stgWhiteHole = new StgWhiteHole();

    public static final int SPIN_COUNT = 1000;

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
