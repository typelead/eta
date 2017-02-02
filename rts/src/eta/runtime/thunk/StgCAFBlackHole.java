package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;

public class StgCAFBlackHole extends StgThunk {

    public StgCAFBlackHole(StgClosure indirectee) {
        super(indirectee);
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        Thunk.blackHole(context, this);
    }
}
