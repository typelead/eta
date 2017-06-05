package eta.runtime.stg;

import java.io.Serializable;

import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.thunk.StgThunk;
import eta.runtime.message.MessageBlackHole;
import static eta.runtime.RtsMessages.barf;

public class StgClosure implements Serializable {

    public void enter(StgContext context) {
        context.R(1, this);
    }

    public StgClosure getEvaluated() { return null; }

    public void evaluate(StgContext context) {
        StgClosure eval = getEvaluated();
        if (eval == null) {
            enter(context);
        } else {
            context.R(1, eval);
        }
    }

    public void doUpdateThunk(Capability cap, StgTSO tso) {
        cap.checkBlockingQueues(tso);
    }
    public boolean blackHole(StgThunk bh, Capability cap,
                             MessageBlackHole msg) { return false; }
    public boolean isFizzledSpark() { return true; }

    /* Applications */
    public void applyV(StgContext context) {
        barf("ap_v");
    }

    public void applyN(StgContext context, int n) {
        barf("ap_n");
    }

    public void applyL(StgContext context, long l) {
        barf("ap_l");
    }

    public void applyF(StgContext context, float f) {
        barf("ap_f");
    }

    public void applyD(StgContext context, double d) {
        barf("ap_d");
    }

    public void applyO(StgContext context, Object o) {
        barf("ap_o");
    }

    public void applyP(StgContext context, StgClosure p) {
        barf("ap_p");
    }

    public void applyPV(StgContext context, StgClosure p) {
        barf("ap_pv");
    }

    public void applyPP(StgContext context, StgClosure p1, StgClosure p2) {
        barf("ap_pp");
    }

    public void applyPPV(StgContext context, StgClosure p1, StgClosure p2) {
        barf("ap_ppv");
    }

    public void applyPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        barf("ap_ppp");
    }

    public void applyPPPV(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        barf("ap_pppv");
    }

    public void applyPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4) {
        barf("ap_pppp");
    }

    public void applyPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5) {
        barf("ap_ppppp");
    }

    public void applyPPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5, StgClosure p6) {
        barf("ap_pppppp");
    }
}
