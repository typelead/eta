package eta.runtime.parallel;

import java.util.Deque;
import java.util.concurrent.LinkedBlockingDeque;

import eta.runtime.Runtime;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import static eta.runtime.RuntimeLogging.debugScheduler;

public class Parallel {
    public static final Deque<Closure> globalSparkPool = new LinkedBlockingDeque<Closure>(Runtime.getMaxLocalSparks());
    public static final SparkCounters globalSparkStats = new SparkCounters();

    static {
        Runtime.parallelClassLoaded = true;
    }

    public static Closure getSpark(StgContext context) {
        Closure spark = findSpark(context.myCapability);
        if (spark != null) {
            context.I1 = 1;
            return spark;
        } else {
            context.I1 = 0;
            return Closures.False;
        }
    }

    public static int numSparks(StgContext context) {
        return globalSparkPoolSize();
    }

    public static Closure findSpark(Capability cap) {
        if (!cap.emptyRunQueue()) {
            return null;
        }
        boolean retry;
        do {
            retry = false;
            Closure spark = globalSparkPool.pollLast();
            while (spark != null && spark.getEvaluated() != null) {
                globalSparkStats.fizzled.getAndIncrement();
                spark = globalSparkPool.pollLast();
            }
            if (spark != null) {
                globalSparkStats.converted.getAndIncrement();
                return spark;
            }
            if (!emptyGlobalSparkPool()) {
                retry = true;
            }
        } while(retry);
        if (Runtime.debugScheduler()) {
            debugScheduler("No Sparks stolen.");
        }
        return null;
    }

    public static boolean emptyGlobalSparkPool() {
        return globalSparkPool.isEmpty();
    }

    public static int globalSparkPoolSize() {
        return globalSparkPool.size();
    }

    public static boolean anySparks() {
        return !emptyGlobalSparkPool();
    }

    public static boolean submitSpark(Closure spark) {
        return globalSparkPool.offerFirst(spark);
    }
}
