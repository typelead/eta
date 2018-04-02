package eta.runtime.thunk;

import eta.Closure;
import eta.Thunk;
import eta.runtime.stg.StgContext;

public abstract class SingleEntryThunk extends Thunk {

    @Override
    public final Closure evaluate(StgContext context) {
        if (Thread.interrupted()) {
            context.myCapability.idleLoop(false);
        }
        /* TODO: Have some mechanism to *ensure* that it doesn't run multiple times. */
        boolean trampoline = context.trampoline;
        if (context.firstTime) {
            context.firstTime = false;
        } else {
            context.trampoline = false;
        }
        Closure result = null;
        try {
            result = thunkEnter(context);
        } finally {
            context.trampoline = trampoline;
        }
        clear();
        return result;
    }
}
