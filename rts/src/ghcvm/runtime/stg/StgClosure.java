package ghcvm.runtime.stg;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.apply.Void;
import ghcvm.runtime.message.MessageBlackHole;
import static ghcvm.runtime.RtsMessages.barf;

public class StgClosure {

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
    public boolean blackHole(Capability cap, MessageBlackHole msg) { return false; }
    public boolean isTrecHeader() { return false; }

    /* Applications */
    public void apply(StgContext context, Void v) {
        barf("ap_v");
    }

    public void apply(StgContext context, int n) {
        barf("ap_n");
    }

    public void apply(StgContext context, long l) {
        barf("ap_l");
    }

    public void apply(StgContext context, float f) {
        barf("ap_f");
    }

    public void apply(StgContext context, double d) {
        barf("ap_d");
    }

    public void apply(StgContext context, Object o) {
        barf("ap_o");
    }

    public void apply(StgContext context, StgClosure p) {
        barf("ap_p");
    }

    public void apply(StgContext context, StgClosure p, Void v) {
        barf("ap_pv");
    }

    public void apply(StgContext context, StgClosure p, Void v, Object o) {
        barf("ap_pvo");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2) {
        barf("ap_pp");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2, Void v) {
        barf("ap_ppv");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2, Void v, Object o) {
        barf("ap_ppvo");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        barf("ap_ppp");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, Void v) {
        barf("ap_pppv");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, Void v, Object o) {
        barf("ap_pppvo");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4) {
        barf("ap_pppp");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5) {
        barf("ap_ppppp");
    }

    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5, StgClosure p6) {
        barf("ap_pppppp");
    }
}
