package mapandsum;

import ghcvm.runtime.RtsConfig;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.apply.StgFun;
import ghcvm.runtime.thunk.StgThunk;

public class Main {

    /* (map f xs) thunk that is used by map_closure */
    private static class sat_s2Ew extends StgInd {
        private final StgClosure f;
        private final StgClosure xs;

        public sat_s2Ew(final StgClosure f, final StgClosure x) {
            this.f = f;
            this.xs = xs;
        }

        @Override
        public void thunkEnter(StgContext context) {
            context.R(2, f);
            context.R(3, xs);
            map_closure.enter(context);
        }
    }

    /* map f list = ... */
    public static StgClosure map_closure = new StgFun() {
        @Override
        public int getArity() { return 2; }

        @Override
        public void enter(StgContext context) {
            /* f */
            StgClosure R2 = context.R(2);
            /* list */
            StgClosure R3 = context.R(3);
            /* Force list to WHNF (equivalent to case list) */
            StgConstr R1 = (StgConstr) R3.getEvaluated();
            if (list == null) {
                /* Evaluate list if not in WHNF */
                R3.enter(context);
                R1 = (StgConstr) context.R(1);
            }
            /* Perform case analysis of the evaluated thunk */
            switch (R1.getTag()) {
                /* x : xs */
                case 1:
                    /* x */
                    StgClosure _s2Et = R1.get(1);
                    /* xs */
                    StgClosure _s2Eu = R1.get(2);
                    /* f (from above) */
                    StgClosure _s2Eq = R2;
                    /* map f xs */
                    StgThunk map_thunk = new sat_s2Ew(_s2Eq, _s2Eu);
                    /* f x */
                    StgThunk f_thunk = new Ap2Upd(_s2Eq, _s2Et);
                    /* f x : map f xs */
                    StgConstr ret = new ZC(f_thunk, map_thunk);
                    /* Return (f x : map f xs) */
                    context.R(1, ret);
                    break;

                /* [] */
                case 2:
                    /* Return [] */
                    context.R(1, base.ghc.tuple.ZMZN_closure);
                    break;
            }
        }
    };

    /* enumFromTo 1 10 */
    public static StgClosure caf1_closure = new StgIndStatic() {
            @Override
            public void thunkEnter(StgContext context) {
                /* Pass direct primitive integer arguments */
                context.I(1, 1);
                context.I(2, 10);
                MinimalBase.zdwenumFromTo_closure.enter(context);
            }
        };

    /* \x -> x + 1 */
    public static StgClosure caf2_closure = new StgFun() {
        @Override
        public int getArity() { return 1; }

        @Override
        public void enter(StgContext context) {
            /* x */
            StgClosure R2 = context.R(2);
            /* Force x to WHNF */
            StgConstr R1 = (StgConstr) R2.getEvaluated();
            if (R1 == null) {
                /* Evaluate x if not in WHNF */
                R2.enter(context);
                R1 = (StgConstr) context.R(1);
            }
            /* Grab the first field of I# & add 1 */
            int _s2EA = R1.get(1) + 1;
            /* Construct a new Int and ... */
            StgClosure ret = new MinimalBase.Izh(_s2Ea);
            /* and return it */
            context.R(1, ret);
        }
    };

    /* map (\x -> x + 1) $ enumFromTo 1 10 */
    public static StgClosure caf_closure = new StgIndStatic() {
        @Override
        public void thunkEnter(StgContext context) {
            context.R(2, caf2_closure);
            context.R(3, caf1_closure);
            map_closure.enter(context);
        }
    };

    /* sum xs = ... */
    public static StgClosure sum_closure = new StgFun() {
        @Override
        public int getArity() { return 1; }

        @Override
        public void enter(StgContext context) {
            zdwsum_closure.enter(context);
            int iret = context.I(1);
            StgClosure ret = new MinimalBase.Izh(iret);
            context.R(1, ret);
        }
    };

    /* $wsum xs = ...
       Note: This function was generated as part of the
             worker-wrapper transformation. */
    public static StgClosure zdwsum_closure = new StgFun(1) {
        @Override
        public int getArity() { return 1; }

        @Override
        public void enter(StgContext context) {
            /* xs */
            StgClosure R2 = context.R(2);
            /* Force xs to WHNF */
            StgConstr R1 = (StgConstr) R2.getEvaluated();
            if (R1 == null) {
                /* Evaluate xs if not in WHNF */
                R2.enter(context);
                R1 = (StgConstr) context.R(1);
            }
            switch (R1.getTag()) {
                case 1:
                    /* Return 0 */
                    context.I(1, 0);
                    break;
                /* x : xs' */
                case 2:
                    /* xs' */
                    StgClosure _s2EE = R1.get(2);
                    /* x */
                    R2 = R1.get(1);
                    /* Force x to WHNF */
                    StgConstr R1 = (StgConstr) R2.getEvaluated();
                    if (R1 == null) {
                        /* Evaluate x if not in WHNF */
                        R2.enter(context);
                        R1 = (StgConstr) context.R(1);
                    }
                    /* Grab x from I# x */
                    int x = R1.get(1);
                    /* Call $wsum xs' */
                    context.R(2, _s2EE);
                    zdwsum_closure.enter(context);
                    /* Add return value from $wsum xs'
                        with x... */
                    int sum = x + context.I(1);
                    /* and return it */
                    context.I(1, sum);
                    break;
            }
        }
    };

    /* main1 = do {...} */
    public static StgClosure main1_closure = new StgFun() {
        @Override
        public int getArity() { return 0; }

        @Override
        public void enter(StgContext context) {
            /* Call $wsum caf */
            context.R(2, caf_closure);
            zdwsum_closure.enter(context);
            int sum = context.I(1);
            /* Print the return value */
            Print.printInt(sum);
            /* Return () */
            context.R(1, base.ghc.tuple.ZLZR_closure);
        }
    };

    /* main = do {...} */
    public static StgClosure main_closure = new StgClosure() {
        @Override
        public void enter(StgContext context) {
            main1_closure.enter(context);
        }
    };

    public static void main(String[] args) {
        RtsConfig config = RtsConfig.getDefault();
        config.rtsHsMain = true;
        ExitCode exitCode = hsMain(args, main_closure, config);
        System.exit(exitCode.code());
    }
}
