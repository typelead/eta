package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public abstract class UpdatableThunk extends Thunk {

    @Override
    public final Closure evaluate(StgContext context) {
        do {
            if (indirectee == null) {
                if (context.interrupted()) {
                    context.myCapability.idleLoop(false);
                }
                final UpdateInfo ui = context.pushUpdate(this);
                final boolean trampoline = context.getAndSetTrampolineUnlessFirst();
                Closure result = null;
                try {
                    result = thunkEnter(context);
                } catch (Exception e) {
                    if (handleException(context, e)) continue;
                } finally {
                    context.popUpdate();
                    context.trampoline = trampoline;
                }
                if (ui.marked) {
                    return updateCode(context, result);
                } else {
                    updateWithIndirection(result);
                    return result;
                }
            } else {
                return blackHole(context);
            }
        } while (true);
    }
}
