package mapandsum;

import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.apply.StgFun;
import ghcvm.runtime.thunk.StgThunk;

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

        public sat_sC8(StgClosure x1, int x2, int x3) {
            this.x1 = x1;
            this.x2 = x2;
            this.x3 = x3;
        }

        @Override
        public void thunkEnter(StgContext context) {
            int _sC3 = x3;
            if (_sC3 == x2) {
                // Return []
                context.R(1, base.ghc.types.ZMZN_closure);
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

        public go_sC2(int x1) {
            this.x1 = x1;
        }

        @Override
        public void enter(StgContext context) {
            /* R1 */
            StgClosure R1 = this;
            /* R2 */
            int i1 = context.I(1);
            int _sBZ = x1;
            sat_sC8 thunk = new sat_sC8(R1, _sBZ, R2);
            Izh int_ = new Izh(R2);
            ZC ret = new ZC(int_, thunk);
            context.R(1, ret);
        }
    }

    /* $wenumFromTo */
    public static StgClosure zdwenumFromTo_closure = new StgFun(2) {
            @Override
            public void enter(StgContext context) {
                /* R2 */
                int i1 = context.I(1);
                /* R3 */
                int i2 = context.I(2);
                /* %MO_S_Gt_W64(R2, R3) */
                if (i1 > i2) {
                    context.R(1, base.ghc.types.ZMZN_closure);
                } else {
                    go_SC2 fun = new go_SC2(i2);
                    fun.enter(context);
                }
            }
        };
}
