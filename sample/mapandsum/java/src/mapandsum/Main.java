package mapandsum;

import ghcvm.runtime.Rts;
import ghcvm.runtime.RtsConfig;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgConstr;
import ghcvm.runtime.apply.StgFun;
import ghcvm.runtime.thunk.Ap2Upd;
import ghcvm.runtime.thunk.StgThunk;
import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.thunk.StgIndStatic;
import static ghcvm.runtime.Rts.ExitCode;
import static mapandsum.MinimalBase.Izh;
import static ghczmprim.ghc.Types.ZMZN_closure;
import static ghczmprim.ghc.Types.ZC;
import static ghczmprim.ghc.Tuple.ZLZR_closure;

public class Main {

    /* (map f xs) thunk that is used by map_closure */
    private static class sat_s2Ew extends StgInd {
        private final StgClosure f;
        private final StgClosure xs;

        public sat_s2Ew(final StgClosure f, final StgClosure xs) {
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
            /* Force list to WHNF */
            StgConstr R1 = (StgConstr) R3.evaluate(context);
            /* Perform case analysis of the evaluated thunk */
            switch (R1.getTag()) {
                /* [] */
                case 1:
                    /* Return [] */
                    context.R(1, ZMZN_closure);
                    break;
                /* x : xs */
                case 2:
                    /* x */
                    ZC R1_ = (ZC) R1;
                    StgClosure _s2Et = R1_.get1();
                    /* xs */
                    StgClosure _s2Eu = R1_.get2();
                    /* f (from above) */
                    StgClosure _s2Eq = R2;
                    /* map f xs */
                    sat_s2Ew map_thunk = new sat_s2Ew(_s2Eq, _s2Eu);
                    /* f x */
                    Ap2Upd f_thunk = new Ap2Upd(_s2Eq, _s2Et);
                    /* f x : map f xs */
                    ZC ret = new ZC(f_thunk, map_thunk);
                    /* Return (f x : map f xs) */
                    context.R(1, ret);
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
            StgConstr R1 = (StgConstr) R2.evaluate(context);
            Izh R1_ = (Izh) R1;
            /* Grab the first field of I# & add 1 */
            int _s2EA = R1_.get1() + 1;
            /* Construct a new Int and ... */
            StgClosure ret = new Izh(_s2EA);
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
            StgClosure ret = new Izh(iret);
            context.R(1, ret);
        }
    };

    /* $wsum xs = ...
       Note: This function was generated as part of the
             worker-wrapper transformation. */
    public static StgClosure zdwsum_closure = new StgFun() {
        @Override
        public int getArity() { return 1; }

        @Override
        public void enter(StgContext context) {
            /* xs */
            StgClosure R2 = context.R(2);
            /* Force xs to WHNF */
            StgConstr R1 = (StgConstr) R2.evaluate(context);
            switch (R1.getTag()) {
                /* [] */
                case 1:
                    /* Return 0 */
                    context.I(1, 0);
                    break;
                /* x : xs' */
                case 2:
                    /* xs' */
                    ZC R1_ = (ZC) R1;
                    StgClosure _s2EE = R1_.get2();
                    /* x */
                    R2 = R1_.get1();
                    /* Force x to WHNF */
                    R1 = (StgConstr) R2.evaluate(context);
                    /* Grab x from I# x */
                    Izh R1__ = (Izh) R1;
                    int x = R1__.get1();
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
            /* Print the return value */
            Print.printIntzh(context);
            /* Return () */
            context.R(1, ZLZR_closure);
        }
    };

    /* main = do {...} */
    public static StgClosure main_closure = new StgFun() {
        @Override
        public int getArity() { return 1; }

        @Override
        public void enter(StgContext context) {
            main1_closure.enter(context);
        }
    };

    public static void main(String[] args) {
        RtsConfig config = RtsConfig.getDefault();
        config.rtsHsMain = true;
        ExitCode exitCode = Rts.hsMain(args, main_closure, config);
        System.exit(exitCode.code());
    }
}
