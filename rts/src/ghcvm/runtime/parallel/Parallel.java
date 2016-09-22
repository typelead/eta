package ghcvm.runtime.parallel;

import ghcvm.runtime.stg.RtsFun;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.RtsMessages.barf;

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
