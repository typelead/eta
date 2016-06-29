package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public class StgCAFBlackHole extends StgThunk {

    public StgCAFBlackHole(StgClosure indirectee) {
        super(indirectee);
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        Thunk.blackHole(context, this, indirectee);
    }
}
