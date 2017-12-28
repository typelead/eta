package eta.runtime.parallel;

import java.util.concurrent.atomic.AtomicLong;

public class SparkCounters {
    public AtomicLong created    = new AtomicLong();
    public AtomicLong dud        = new AtomicLong();
    public AtomicLong overflowed = new AtomicLong();
    public AtomicLong converted  = new AtomicLong();
    public AtomicLong fizzled    = new AtomicLong();
}
