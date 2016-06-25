package mapandsum;

import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgConstr;
import ghcvm.runtime.apply.StgFun;
import ghcvm.runtime.thunk.StgThunk;
import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.thunk.StgIndStatic;
import static ghczmprim.ghc.Types.ZMZN_closure;
import static ghczmprim.ghc.Types.ZC;

public class MinimalBase {

    public static abstract class Int$ extends StgConstr {}

    public static final class Izh extends Int$ {
        private final int x;

        public Izh(int x) {
            this.x = x;
        }

        @Override
        public int getTag() {
            return 1;
        }

        public int get1() {
            return x;
        }
    }

    private static class sat_sC8 extends StgInd {
        private final StgClosure x1;
        private final int x2;
        private final int x3;

        private sat_sC8(StgClosure x1, int x2, int x3) {
            this.x1 = x1;
            this.x2 = x2;
            this.x3 = x3;
        }

        @Override
        public void thunkEnter(StgContext context) {
            int _sC3 = x3;
            if (_sC3 == x2) {
                // Return []
                context.R(1, ZMZN_closure);
            } else {
                int R2 = _sC3 + 1;
                StgClosure R1 = x1;
                context.I(1, R2);
                R1.enter(context);
            }
        }
    }

    private static class go_sC2 extends StgFun {
        private final int x1;

        private go_sC2(int x1) {
            this.x1 = x1;
        }

        @Override
        public int getArity() { return 1; }

        @Override
        public void enter(StgContext context) {
            /* R1 */
            StgClosure R1 = this;
            /* R2 */
            int i1 = context.I(1);
            int _sBZ = x1;
            sat_sC8 thunk = new sat_sC8(R1, _sBZ, i1);
            Izh int_ = new Izh(i1);
            ZC ret = new ZC(int_, thunk);
            context.R(1, ret);
        }
    }

    /* $wenumFromTo */
    public static StgClosure zdwenumFromTo_closure = new StgFun() {
        @Override
        public int getArity() { return 2; }

        @Override
        public void enter(StgContext context) {
            /* R2 */
            int i1 = context.I(1);
            /* R3 */
            int i2 = context.I(2);
            /* %MO_S_Gt_W64(R2, R3) */
            if (i1 > i2) {
                /* Return [] */
                context.R(1, ZMZN_closure);
            } else {
                go_sC2 fun = new go_sC2(i2);
                fun.enter(context);
            }
        }
    };
}
