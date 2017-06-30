package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.exception.EtaAsyncException;

public abstract class UpdatableThunk extends Thunk {

    @Override
    public Closure enter(StgContext context) {
        if (indirectee == null) {
            UpdateInfo ui = context.pushUpdate(this);
            Closure result = null;
            try {
                result = thunkEnter(context);
            } catch (EtaAsyncException ea) {
                if (ea.stopHere == ui) {
                    return enter(context);
                } else {
                    throw ea;
                }
            } finally {
                Thunk popped = context.popUpdate();
                assert popped == this;
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
    }
}
