package eta.runtime.stg;

import java.io.Serializable;

import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.thunk.Thunk;
import eta.runtime.message.MessageBlackHole;
import static eta.runtime.RtsMessages.barf;

public abstract class Closure implements Serializable {

    public abstract Closure enter(StgContext context);

    public Closure getEvaluated() { return null; }

    public Closure evaluate(StgContext context) {
        Closure eval = getEvaluated();
        if (eval == null) {
            return enter(context);
        } else {
            return eval;
        }
    }

    public boolean blackHole(Thunk bh, Capability cap,
                             MessageBlackHole msg) { return false; }
    public boolean isFizzledSpark() { return true; }

    /* Applications */
    public Closure applyV(StgContext context) {
        barf("ap_v");
    }

    public Closure applyN(StgContext context, int n) {
        barf("ap_n");
    }

    public Closure applyL(StgContext context, long l) {
        barf("ap_l");
    }

    public Closure applyF(StgContext context, float f) {
        barf("ap_f");
    }

    public Closure applyD(StgContext context, double d) {
        barf("ap_d");
    }

    public Closure applyO(StgContext context, Object o) {
        barf("ap_o");
    }

    public Closure applyP(StgContext context, Closure p) {
        barf("ap_p");
    }

    public Closure applyPV(StgContext context, Closure p) {
        barf("ap_pv");
    }

    public Closure applyPP(StgContext context, Closure p1, Closure p2) {
        barf("ap_pp");
    }

    public Closure applyPPV(StgContext context, Closure p1, Closure p2) {
        barf("ap_ppv");
    }

    public Closure applyPPP(StgContext context, Closure p1, Closure p2, Closure p3) {
        barf("ap_ppp");
    }

    public Closure applyPPPV(StgContext context, Closure p1, Closure p2, Closure p3) {
        barf("ap_pppv");
    }

    public Closure applyPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        barf("ap_pppp");
    }

    public Closure applyPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        barf("ap_ppppp");
    }

    public Closure applyPPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        barf("ap_pppppp");
    }
}
