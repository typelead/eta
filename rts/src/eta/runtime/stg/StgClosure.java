package eta.runtime.stg;

import java.io.Serializable;

import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.thunk.StgThunk;
import eta.runtime.message.MessageBlackHole;
import static eta.runtime.RtsMessages.barf;

public abstract class StgClosure implements Serializable {

    public abstract StgClosure enter(StgContext context);

    public StgClosure getEvaluated() { return null; }

    public StgClosure evaluate(StgContext context) {
        StgClosure eval = getEvaluated();
        if (eval == null) {
            return enter(context);
        } else {
            return eval;
        }
    }

    public void doUpdateThunk(Capability cap, StgTSO tso) {
        cap.checkBlockingQueues(tso);
    }
    public boolean blackHole(StgThunk bh, Capability cap,
                             MessageBlackHole msg) { return false; }
    public boolean isFizzledSpark() { return true; }

    /* Applications */
    public StgClosure applyV(StgContext context) {
        barf("ap_v");
    }

    public StgClosure applyN(StgContext context, int n) {
        barf("ap_n");
    }

    public StgClosure applyL(StgContext context, long l) {
        barf("ap_l");
    }

    public StgClosure applyF(StgContext context, float f) {
        barf("ap_f");
    }

    public StgClosure applyD(StgContext context, double d) {
        barf("ap_d");
    }

    public StgClosure applyO(StgContext context, Object o) {
        barf("ap_o");
    }

    public StgClosure applyP(StgContext context, StgClosure p) {
        barf("ap_p");
    }

    public StgClosure applyPV(StgContext context, StgClosure p) {
        barf("ap_pv");
    }

    public StgClosure applyPP(StgContext context, StgClosure p1, StgClosure p2) {
        barf("ap_pp");
    }

    public StgClosure applyPPV(StgContext context, StgClosure p1, StgClosure p2) {
        barf("ap_ppv");
    }

    public StgClosure applyPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        barf("ap_ppp");
    }

    public StgClosure applyPPPV(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        barf("ap_pppv");
    }

    public StgClosure applyPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4) {
        barf("ap_pppp");
    }

    public StgClosure applyPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5) {
        barf("ap_ppppp");
    }

    public StgClosure applyPPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5, StgClosure p6) {
        barf("ap_pppppp");
    }
}
