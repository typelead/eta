package eta.runtime.thunk;

import eta.Closure;
import eta.Thunk;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;

public class CAF extends Thunk {

    public CAF() {
        super();
    }

    public CAF(Closure indirectee) {
        super(indirectee);
    }

    @Override
    public final Closure evaluate(StgContext context) {
        for (;;) {
            if (indirectee == null) {
                if (Thread.interrupted()) {
                    context.myCapability.idleLoop(false);
                }
                TSO tso = context.currentTSO;
                if (!claim(tso)) continue;
                UpdateInfo ui = context.pushUpdate(this);
                Closure result = null;
                boolean trampoline = context.trampoline;
                if (context.firstTime) {
                    context.firstTime = false;
                } else {
                    context.trampoline = false;
                }
                try {
                    result = thunkEnter(context);
                } catch (java.lang.Exception e) {
                    if (Thunk.handleException(context, e, ui)) continue;
                } finally {
                    context.popUpdate();
                    context.trampoline = trampoline;
                }
                return updateCode(context, result);
            } else {
                return blackHole(context);
            }
        }
    }

    /* By default, if the single-argument constructor is used, it will just redirect
       to the indirectee. Normally, it will be overriden by non-trivial top-level
       thunks. */
    @Override
    public Closure thunkEnter(StgContext context) {
        return indirectee.enter(context);
    }

    @Override
    public final void clear() {}

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
