package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.RtsFun;
import eta.runtime.stg.AbstractArgumentStack;

public class Apply {
    public static RtsFun ap_0_fast = new Ap0Fast();
    public static RtsFun ap_v_fast = new ApVFast();
    public static RtsFun ap_n_fast = new ApNFast();
    public static RtsFun ap_l_fast = new ApLFast();
    public static RtsFun ap_f_fast = new ApFFast();
    public static RtsFun ap_d_fast = new ApDFast();
    public static RtsFun ap_o_fast = new ApOFast();
    public static RtsFun ap_p_fast = new ApPFast();
    public static RtsFun ap_pv_fast = new ApPVFast();
    public static RtsFun ap_pvo_fast = new ApPVOFast();
    public static RtsFun ap_pp_fast = new ApPPFast();
    public static RtsFun ap_ppv_fast = new ApPPVFast();
    public static RtsFun ap_ppvo_fast = new ApPPVOFast();
    public static RtsFun ap_ppp_fast = new ApPPPFast();
    public static RtsFun ap_pppv_fast = new ApPPPVFast();
    public static RtsFun ap_pppvo_fast = new ApPPPVOFast();
    public static RtsFun ap_pppp_fast = new ApPPPPFast();
    public static RtsFun ap_ppppp_fast = new ApPPPPPFast();
    public static RtsFun ap_pppppp_fast = new ApPPPPPPFast();
    public static RtsFun PAP_apply = new PAPApply();

    private static class Ap0Fast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            fun.evaluate(context);
        }
    }

    private static class ApVFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            fun.apply(context, Void._void);
        }
    }

    private static class ApNFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            int n = context.I(1);
            fun.apply(context, n);
        }
    }

    private static class ApLFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            long l = context.L(1);
            fun.apply(context, l);
        }
    }

    private static class ApFFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            float f = context.F(1);
            fun.apply(context, f);
        }
    }

    private static class ApDFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            double d = context.D(1);
            fun.apply(context, d);
        }
    }

    private static class ApOFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            Object o = context.O(1);
            fun.apply(context, o);
        }
    }

    private static class ApPFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p = context.R(2);
            fun.apply(context, p);
        }
    }

    private static class ApPVFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p = context.R(2);
            fun.apply(context, p, Void._void);
        }
    }

    private static class ApPVOFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p = context.R(2);
            Object o = context.O(1);
            fun.apply(context, p, null, o);
        }
    }

    private static class ApPPFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            fun.apply(context, p1, p2);
        }
    }

    private static class ApPPVFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            fun.apply(context, p1, p2, Void._void);
        }
    }

    private static class ApPPVOFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            Object o = context.O(1);
            fun.apply(context, p1, p2, null, o);
        }
    }

    private static class ApPPPFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            StgClosure p3 = context.R(4);
            fun.apply(context, p1, p2, p3);
        }
    }

    private static class ApPPPVFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            StgClosure p3 = context.R(4);
            fun.apply(context, p1, p2, p3, Void._void);
        }
    }

    private static class ApPPPVOFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            StgClosure p3 = context.R(4);
            Object o = context.O(1);
            fun.apply(context, p1, p2, p3, null, o);
        }
    }

    private static class ApPPPPFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            StgClosure p3 = context.R(4);
            StgClosure p4 = context.R(5);
            fun.apply(context, p1, p2, p3, p4);
        }
    }

    private static class ApPPPPPFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            StgClosure p3 = context.R(4);
            StgClosure p4 = context.R(5);
            StgClosure p5 = context.R(6);
            fun.apply(context, p1, p2, p3, p4, p5);
        }
    }

    private static class ApPPPPPPFast extends RtsFun {
        @Override
        public void enter(StgContext context) {
            /* TODO: Verify implementation */
            StgClosure fun = context.R(1);
            StgClosure p1 = context.R(2);
            StgClosure p2 = context.R(3);
            StgClosure p3 = context.R(4);
            StgClosure p4 = context.R(5);
            StgClosure p5 = context.R(6);
            StgClosure p6 = context.R(7);
            fun.apply(context, p1, p2, p3, p4, p5, p6);
        }
    }

    private static class PAPApply extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgPAP pap = (StgPAP) context.R(1);
            AbstractArgumentStack stack = (AbstractArgumentStack) context.O(1);
            context.merge(stack);
            StgClosure fun = pap.fun;
            fun.enter(context);
        }
    }
}
