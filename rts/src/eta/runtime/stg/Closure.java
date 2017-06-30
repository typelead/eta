package eta.runtime.stg;

import java.io.Serializable;

import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.thunk.Thunk;
import eta.runtime.message.MessageBlackHole;

import static eta.runtime.RuntimeLogging.barf;

public abstract class Closure implements Serializable {

    public abstract Closure enter(StgContext context);

    public Closure getEvaluated() { return null; }

    public Closure evaluate(StgContext context) {
        Closure eval = getEvaluated();
        if (eval == null) {
            if (Thread.interrupted()) {
                context.myCapability.idleLoop(false);
            }
            return enter(context);
        } else {
            return eval;
        }
    }

    /* Applications */
    public Closure applyV(StgContext context) {
        barf("ap_v");
        return null;
    }

    public Closure applyN(StgContext context, int n) {
        barf("ap_n");
        return null;
    }

    public Closure applyL(StgContext context, long l) {
        barf("ap_l");
        return null;
    }

    public Closure applyF(StgContext context, float f) {
        barf("ap_f");
        return null;
    }

    public Closure applyD(StgContext context, double d) {
        barf("ap_d");
        return null;
    }

    public Closure applyO(StgContext context, Object o) {
        barf("ap_o");
        return null;
    }

    public Closure applyP(StgContext context, Closure p) {
        barf("ap_p");
        return null;
    }

    public Closure applyPV(StgContext context, Closure p) {
        barf("ap_pv");
        return null;
    }

    public Closure applyPP(StgContext context, Closure p1, Closure p2) {
        barf("ap_pp");
        return null;
    }

    public Closure applyPPV(StgContext context, Closure p1, Closure p2) {
        barf("ap_ppv");
        return null;
    }

    public Closure applyPPP(StgContext context, Closure p1, Closure p2, Closure p3) {
        barf("ap_ppp");
        return null;
    }

    public Closure applyPPPV(StgContext context, Closure p1, Closure p2, Closure p3) {
        barf("ap_pppv");
        return null;
    }

    public Closure applyPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        barf("ap_pppp");
        return null;
    }

    public Closure applyPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        barf("ap_ppppp");
        return null;
    }

    public Closure applyPPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        barf("ap_pppppp");
        return null;
    }
}
