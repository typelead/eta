package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public class Apply {
    public static StgClosure ap_v_fast = new StgClosure() {

            @Override
            public void enter(StgContext context) {
                // TODO: Implement

            }
        };
    public static StgClosure PAP_apply = new StgClosure() {

            @Override
            public void enter(StgContext context) {
                StgPAP pap = (StgPAP) context.R1;
                // TODO: Code to reload the stack from pap
                context.R1 = pap.fun;
                context.R1.enter(context);
            }
        };

}
