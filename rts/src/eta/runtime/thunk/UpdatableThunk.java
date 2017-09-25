package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.exception.Exception;
import eta.runtime.exception.EtaException;
import eta.runtime.exception.EtaAsyncException;

public abstract class UpdatableThunk extends Thunk {

    @Override
    public Closure evaluate(StgContext context) {
        do {
            if (indirectee == null) {
                if (Thread.interrupted()) {
                    context.myCapability.idleLoop(false);
                }
                UpdateInfo ui = context.pushUpdate(this);
                Closure result = null;
                try {
                    result = thunkEnter(context);
                } catch (java.lang.Exception e) {
                    if (e instanceof EtaAsyncException) {
                        if (((EtaAsyncException) e).stopHere == ui) {
                            continue;
                        } else {
                            throw e;
                        }
                    } else {
                        EtaException e_;
                        if (e instanceof EtaException) {
                            e_ = (EtaException) e;
                        } else {
                            e_ = Exception.toEtaException(context.currentTSO, e);
                        }
                        throw e_;
                    }
                } finally {
                    Thunk popped = context.popUpdate();
                    assert popped == this:
                        "Popped " + popped + " from update stack and expected [UpdatableThunk] " + this;
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
