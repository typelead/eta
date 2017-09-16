package eta.runtime.concurrent;

import java.util.Deque;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;

public class MVar extends Value {
    public AtomicBoolean lock              = new AtomicBoolean(false);
    public BlockingQueue<Closure> valQueue = new ArrayBlockingQueue<Closure>(1, true);

    public MVar(Closure value) {
        if (value != null) {
            valQueue.offer(value);
        }
    }

    public Closure tryTake() {
        return valQueue.poll();
    }

    public boolean tryPut(Closure closure) {
        return valQueue.offer(closure);
    }

    public Closure read() {
        Closure val;
        do {
            val = tryRead();
        } while (val == null);
        return val;
    }

    public Closure tryRead() {
        return valQueue.peek();
    }
}
