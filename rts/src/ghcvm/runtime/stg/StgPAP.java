package ghcvm.runtime.stg;

import ghcvm.runtime.*;
import static ghcvm.runtime.RtsMessages.*;

public class StgPAP extends StgFun {
    public byte noArgs;

    public StgPAP(StgClosure fun, byte arity, byte noArgs) {
        super(fun, arity);
        this.noArgs = noArgs;
    }

    @Override
    public void enter(StgContext context) {
        barf("PAP object enetered!");
    }

    @Override
    public void preEnter(StgContext context) {

        if (arity == 1) {
            context.R2 = Stg.ap_v;
            Stg.PAP_apply.enter(context);
        } else {

        }
    }
}
