package ghczmprim.ghc;

import ghcvm.runtime.stg.StgConstr;

public class Tuple {

    /* data () */
    public static abstract class ZLZR$ extends StgConstr {}

    /* () */
    public static final class ZLZR extends ZLZR$ {
        @Override
        public int getTag() { return 1; }
    }

    /* Shared static closure for nullary constructor */
    public static ZLZR ZLZR_closure = new ZLZR();
}
