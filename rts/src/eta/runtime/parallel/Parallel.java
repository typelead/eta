package eta.runtime.parallel;

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
            return Closures.False_closure;
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
                globalSparkStats.fizzled++;
                spark = sparks.pollLast();
            }
            if (spark != null) {
                globalSparkStats.converted++;
                return spark;
            }
            if (!emptyGlobalSparkPool()) {
                retry = true;
            }
        } while(retry);
        if (RuntimeOptions.DebugFlags.scheduler) {
            debugBelch("{Scheduler} No Sparks stolen.");
        }
    }

    public static emptyGlobalSparkPool() {
        return globalSparkPool.isEmpty();
    }

    public static int globalSparkPoolSize() {
        return globalSparkPool.size();
    }
}
