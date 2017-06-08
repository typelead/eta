package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class StgCAFBlackHole extends Thunk {

    public StgCAFBlackHole(Closure indirectee) {
        super(indirectee);
    }

    @Override
    public Closure enter(StgContext context) {
        Thunk.blackHole(context, this);
    }
}
