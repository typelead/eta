package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.RtsFun;
import eta.runtime.stg.AbstractArgumentStack;

public class Apply {
    public static RtsFun PAP_apply = new PAPApply();

    private static class PAPApply extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgPAP pap = (StgPAP) context.R(1);
            AbstractArgumentStack stack = (AbstractArgumentStack) context.O(1);
            context.merge(stack);
            pap.fun.enter(context);
        }
    }
}
