package ghcvm.runtime.thunk;

import ghcvm.runtime.thunk.StgWhiteHole;
import ghcvm.runtime.stg.StgClosure;

public abstract class StgIndStatic extends StgInd {

    public StgIndStatic(StgClosure indirectee) {
        super(indirectee);
    }

    public boolean isLocked() {
        return indirectee.getClass().equals(StgWhiteHole.class);
    }
}
