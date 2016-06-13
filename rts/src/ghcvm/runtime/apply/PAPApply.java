package ghcvm.runtime.apply;

import ghcvm.runtime.*;
import ghcvm.runtime.closure.*;
import static ghcvm.runtime.RtsMessages.*;

public class PAPApply extends StgClosure {

    @Override
    public void enter(StgContext context) {
        StgPAP pap = (StgPAP) context.R1;
        // Code to reload the stack from pap
        context.R1 = pap.fun;
        context.R1.enter(context);
    }
}
