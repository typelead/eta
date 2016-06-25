package ghczmprim.ghc;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgConstr;

public class Types {

    /* data [a]  */
    public static abstract class ZMZN$ extends StgConstr {}

    /* [] */
    public static final class ZMZN extends ZMZN$ {
        @Override
        public int getTag() { return 1; }
    }

    /* Shared static closure for nullary constructor */
    public static ZMZN ZMZN_closure = new ZMZN();

    /* (:) a [a] */
    public static final class ZC extends ZMZN$ {
        private StgClosure x1;
        private StgClosure x2;

        public ZC(final StgClosure x1, final StgClosure x2) {
            this.x1 = x1;
            this.x2 = x2;
        }

        @Override
        public int getTag() { return 2; }

        public StgClosure get1() { return x1; }
        public StgClosure get2() { return x2; }
    }
}
