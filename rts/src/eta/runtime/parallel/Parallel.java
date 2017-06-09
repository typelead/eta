package eta.runtime.parallel;

import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;

public class Parallel {

    public static Closure getSpark(StgContext context) {
        Closure spark = context.myCapability.findSpark();
        if (spark != null) {
            context.I(1, 1);
            return spark;
        } else {
            context.I(1, 0);
            return Closures.False_closure;
        }
    }

    public static Closure numSparks(StgContext context) {
        context.I(1, context.myCapability.sparkPoolSize());
        return null;
    }
}
