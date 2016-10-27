package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.RtsFun;
import eta.runtime.stg.AbstractArgumentStack;

public class Apply {
    public static RtsFun ap_0_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                fun.evaluate(context);
            }
        };

    public static RtsFun ap_v_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                fun.apply(context, Void._void);
            }
        };

    public static RtsFun ap_n_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                int n = context.I(1);
                fun.apply(context, n);
            }
        };

    public static RtsFun ap_l_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                long l = context.L(1);
                fun.apply(context, l);
            }
        };

    public static RtsFun ap_f_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                float f = context.F(1);
                fun.apply(context, f);
            }
        };

    public static RtsFun ap_d_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                double d = context.D(1);
                fun.apply(context, d);
            }
        };

    public static RtsFun ap_o_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                Object o = context.O(1);
                fun.apply(context, o);
            }
        };

    public static RtsFun ap_p_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p = context.R(2);
                fun.apply(context, p);
            }
        };

    public static RtsFun ap_pv_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p = context.R(2);
                fun.apply(context, p, Void._void);
            }
        };

    public static RtsFun ap_pvo_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p = context.R(2);
                Object o = context.O(1);
                fun.apply(context, p, null, o);
            }
        };

    public static RtsFun ap_pp_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                fun.apply(context, p1, p2);
            }
        };

    public static RtsFun ap_ppv_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                fun.apply(context, p1, p2, Void._void);
            }
        };

    public static RtsFun ap_ppvo_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                Object o = context.O(1);
                fun.apply(context, p1, p2, null, o);
            }
        };

    public static RtsFun ap_ppp_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                StgClosure p3 = context.R(4);
                fun.apply(context, p1, p2, p3);
            }
        };

    public static RtsFun ap_pppv_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                StgClosure p3 = context.R(4);
                fun.apply(context, p1, p2, p3, Void._void);
            }
        };

    public static RtsFun ap_pppvo_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                StgClosure p3 = context.R(4);
                Object o = context.O(1);
                fun.apply(context, p1, p2, p3, null, o);
            }
        };

    public static RtsFun ap_pppp_fast = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                StgClosure p3 = context.R(4);
                StgClosure p4 = context.R(5);
                fun.apply(context, p1, p2, p3, p4);
            }
        };

    public static RtsFun ap_ppppp_fast = new RtsFun() {
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
        };

    public static RtsFun ap_pppppp_fast = new RtsFun() {
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
        };

    public static RtsFun PAP_apply = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgPAP pap = (StgPAP) context.R(1);
                AbstractArgumentStack stack = (AbstractArgumentStack) context.O(1);
                context.merge(stack);
                StgClosure fun = pap.fun;
                fun.enter(context);
            }
        };
}
