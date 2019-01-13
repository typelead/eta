package eta.runtime.thunk;

import eta.runtime.stg.Closure;
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
                if (context.interrupted()) {
                    context.myCapability.idleLoop(false);
                }
                final TSO tso = context.currentTSO;
                if (!claim(tso)) continue;
                final UpdateInfo ui = context.pushUpdate(this);
                final boolean trampoline = context.getAndSetTrampolineUnlessFirst();
                Closure result = null;
                try {
                    result = thunkEnter(context);
                } catch (java.lang.Exception e) {
                    if (handleException(context, e)) continue;
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
