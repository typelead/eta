package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public class StgCAFBlackHole extends StgThunk {

    public StgCAFBlackHole(StgClosure indirectee) {
        super(indirectee);
    }

    @Override
    public void enter(StgContext context) {
        /* TODO: Implement */
    }

    @Override
    public void thunkEnter(StgContext context) {
        /* TODO: Implement */
    }
}
