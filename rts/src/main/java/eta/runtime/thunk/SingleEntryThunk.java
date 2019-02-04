package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public abstract class SingleEntryThunk extends Thunk {

    @Override
    public final Closure evaluate(StgContext context) {
        if (context.interrupted()) {
            context.myCapability.idleLoop(false);
        }

        /* TODO: Have some mechanism to *ensure* that it doesn't run multiple times. */

        final boolean trampoline = context.getAndSetTrampolineUnlessFirst();
        Closure result = null;
        try {
            result = thunkEnter(context);
        } finally {
            context.trampoline = trampoline;
        }
        return result;
    }
}
