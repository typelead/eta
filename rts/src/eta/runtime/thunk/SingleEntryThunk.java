package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public abstract class SingleEntryThunk extends Thunk {

    @Override
    public Closure evaluate(StgContext context) {
        if (Thread.interrupted()) {
            context.myCapability.idleLoop(false);
        }
        /* TODO: Have some mechanism to *ensure* that it doesn't run multiple times. */
        Closure result = thunkEnter(context);
        clear();
        return result;
    }
}
