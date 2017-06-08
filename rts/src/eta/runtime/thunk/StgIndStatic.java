package eta.runtime.thunk;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class StgIndStatic extends StgThunk {

    public StgIndStatic() {
        super();
    }

    public StgIndStatic(Closure indirectee) {
        super(indirectee);
    }

    @Override
    public Closure enter(StgContext context) {
        if (indirectee == null) {
            Capability cap = context.myCapability;
            StgThunk bh = cap.newCAF(this);
            if (bh == null) {
                return enter(context);
            } else {
                UpdateInfo ui = context.pushUpdate(this);
                try {
                    Closure result = thunkEnter(context);
                } catch (EtaAsyncException ea) {
                    if (ea.stopHere == ui) {
                        return enter(context);
                    } else {
                        throw ea;
                    }
                } finally {
                    StgThunk popped = context.popUpdate();
                    assert popped == this;
                }
                return updateCode(context, result);
            }
        } else {
            return blackHole(context);
        }
    }
}
