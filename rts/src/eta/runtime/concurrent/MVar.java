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

    public Closure take() throws InterruptedException {
        return valQueue.take();
    }

    public void put(Closure closure) throws InterruptedException {
        valQueue.put(closure);
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

    public final void lock() {
        while (!tryLock()) {}
    }

    public final void unlock() {
        lock.set(false);
    }

    public final boolean tryLock() {
        return lock.compareAndSet(false, true);
    }
}
