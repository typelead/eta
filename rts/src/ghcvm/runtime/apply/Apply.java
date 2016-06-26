package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public class Apply {
    public static StgClosure ap_0_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure entered = fun.evaluate(context);
                context.R(1, entered);
            }
        };

    public static StgClosure ap_v_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                fun.apply(context, Void._void);
            }
        };

    public static StgClosure ap_n_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                int n = context.I(1);
                fun.apply(context, n);
            }
        };

    public static StgClosure ap_l_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                long l = context.L(1);
                fun.apply(context, l);
            }
        };

    public static StgClosure ap_f_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                float f = context.F(1);
                fun.apply(context, f);
            }
        };

    public static StgClosure ap_d_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                double d = context.D(1);
                fun.apply(context, d);
            }
        };

    public static StgClosure ap_o_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                Object o = context.O(1);
                fun.apply(context, o);
            }
        };

    public static StgClosure ap_p_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p = context.R(2);
                fun.apply(context, p);
            }
        };

    public static StgClosure ap_pv_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p = context.R(2);
                fun.apply(context, p, Void._void);
            }
        };

    public static StgClosure ap_pvo_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p = context.R(2);
                Object o = context.O(1);
                fun.apply(context, p, null, o);
            }
        };

    public static StgClosure ap_pp_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                fun.apply(context, p1, p2);
            }
        };

    public static StgClosure ap_ppv_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                fun.apply(context, p1, p2, Void._void);
            }
        };

    public static StgClosure ap_ppvo_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                Object o = context.O(1);
                fun.apply(context, p1, p2, null, o);
            }
        };

    public static StgClosure ap_ppp_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                StgClosure p3 = context.R(4);
                fun.apply(context, p1, p2, p3);
            }
        };

    public static StgClosure ap_pppv_fast = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure fun = context.R(1);
                StgClosure p1 = context.R(2);
                StgClosure p2 = context.R(3);
                StgClosure p3 = context.R(4);
                fun.apply(context, p1, p2, p3, Void._void);
            }
        };

    public static StgClosure ap_pppvo_fast = new StgClosure() {
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

    public static StgClosure ap_pppp_fast = new StgClosure() {
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

    public static StgClosure ap_ppppp_fast = new StgClosure() {
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

    public static StgClosure ap_pppppp_fast = new StgClosure() {
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

    public static StgClosure PAP_apply = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgPAP pap = (StgPAP) context.R(1);
                context.merge(pap.argStack);
                StgClosure fun = pap.fun;
                fun.enter(context);
            }
        };
}
