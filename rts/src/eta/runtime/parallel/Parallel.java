package eta.runtime.parallel;

import java.util.Deque;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import static eta.runtime.RuntimeLogging.barf;

public class Parallel {
    public static final Deque<Closure> globalSparkPool = new LinkedBlockingDeque<Closure>(Runtime.getMaxLocalSparks());
    public static final SparkCounters globalSparkStats = new SparkCounters();

    public static Closure getSpark(StgContext context) {
        Closure spark = findSpark(context.myCapability);
        if (spark != null) {
            context.I(1, 1);
            return spark;
        } else {
            context.I(1, 0);
            return Closures.False;
        }
    }

    public static Closure numSparks(StgContext context) {
        context.I(1, context.myCapability.sparkPoolSize());
        return null;
    }

    public static Closure findSpark(Capability cap) {
        if (!cap.emptyRunQueue() || !cap.returningTasks.isEmpty()) {
            return null;
        }
        boolean retry;
        do {
            retry = false;
            Closure spark = sparks.pollLast();
            while (spark != null && spark.getEvaluated() != null) {
                globalSparkStats.fizzled.getAndIncrement();
                spark = sparks.pollLast();
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
            debugBelch("{Scheduler} No Sparks stolen.");
        }
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
}
