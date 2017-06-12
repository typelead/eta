package eta.runtime.thunk;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

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
                try {
                    Closure result = thunkEnter(context);
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
                return updateCode(context, result);
            } else {
                return blackHole(context);
            }
        } while (true);
    }

    @Override
    public void clear() {
        if (!Thunk.shouldKeepCAFs()) {
            super.clear();
        }
    }

    /* Initializing CAFs */
    public final Thunk claim(TSO tso) {
        if (caf.tryLock()) {
            caf.setIndirection(tso);
            if (Thunk.shouldKeepCAFs()) {
                Thunk.revertibleCAFList.offer(caf);
            }
            return true;
        } else return false;
    }
}
