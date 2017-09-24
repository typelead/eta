package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public abstract class SingleEntryThunk extends Thunk {

    @Override
    public Closure enter(StgContext context) {
        /* TODO: Have some mechanism to *ensure* that it doesn't run multiple times. */
        Closure result = thunkEnter(context);
        clear();
        return result;
    }
}
