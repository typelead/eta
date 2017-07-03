package eta.runtime.thunk;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import eta.runtime.exception.EtaAsyncException;

public class CAF extends Thunk {

    public CAF() {
        super();
    }

    public CAF(Closure indirectee) {
        super(indirectee);
    }

    @Override
    public Closure enter(StgContext context) {
        do {
            if (indirectee == null) {
                boolean claimed = claim(context.currentTSO);
                if (!claimed) continue;
                UpdateInfo ui = context.pushUpdate(this);
                Closure result;
                try {
                    result = thunkEnter(context);
                } catch (EtaAsyncException ea) {
                    if (ea.stopHere == ui) {
                        return enter(context);
                    } else {
                        throw ea;
                    }
                } finally {
                    /* TODO: Are there additional conditions to take care of
                             when an exception is thrown while evaluating a thunk? */
                    Thunk popped = context.popUpdate();
                    assert popped == this;
                }
                return updateCode(context, result);
            } else {
                return blackHole(context);
            }
        } while (true);
    }

    /* By default, if the single-argument constructor is used, it will just redirect
       to the indirectee. Normally, it will be overriden by non-trivial top-level
       thunks. */
    @Override
    public Closure thunkEnter(StgContext context) {
        return indirectee.enter(context);
    }

    @Override
    public void clear() {
        if (!Thunk.shouldKeepCAFs()) {
            super.clear();
        }
    }

    /* Initializing CAFs */
    public final boolean claim(TSO tso) {
        if (tryLock()) {
            setIndirection(tso);
            if (Thunk.shouldKeepCAFs()) {
                Thunk.revertibleCAFList.offer(this);
            }
            return true;
        } else return false;
    }
}
