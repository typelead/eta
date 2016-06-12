package ghcvm.runtime.closure;

import ghcvm.runtime.thunk.StgWhiteHole;

public class StgIndStatic extends StgInd {

    public StgIndStatic(StgClosure indirectee) {
        super(indirectee);
    }


    public boolean isLocked() {
        return indirectee.getClass().equals(StgWhiteHole.class);
    }
}
