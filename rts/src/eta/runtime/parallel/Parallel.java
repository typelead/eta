package eta.runtime.parallel;

import eta.runtime.stg.RtsFun;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;

public class Parallel {

    public static RtsFun getSpark = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                barf("getSpark RTS primop not implemented!");
            }
        };

    public static RtsFun numSparks = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                barf("numSparks RTS primop not implemented!");
            }
        };
}
