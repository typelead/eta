package eta.runtime.parallel;

import eta.runtime.stg.RtsFun;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;

public class Parallel {
    public static RtsFun getSpark = new GetSpark();
    public static RtsFun numSparks = new NumSpark();

    private static class GetSpark extends RtsFun {
        @Override
        public void enter(StgContext context) {
            barf("getSpark RTS primop not implemented!");
        }
    }

    private static class NumSpark extends RtsFun {
        @Override
        public void enter(StgContext context) {
            barf("numSparks RTS primop not implemented!");
        }
    }
}
