package eta.runtime.concurrent;

import java.util.Deque;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.concurrent.Concurrent.SPIN_COUNT;

public class MVar extends Value {
    public BlockingQueue<Closure> valQueue = new ArrayBlockingQueue<Closure>(1, true);
    public AtomicBoolean lock = new AtomicBoolean(false);

    public MVar(Closure value) {
        if (value != null) {
            valQueue.offer(value);
        }
    }

    @Override
    public Closure enter(StgContext context) {
        barf("MVAR object entered!");
        return null;
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
        do {
            int i = 0;
            do {
                boolean old = lock.getAndSet(true);
                if (!old) return;
            } while (++i < SPIN_COUNT);
            Thread.yield();
        } while (true);
    }

    public final void unlock() {
        lock.set(false);
    }

    public final boolean tryLock() {
        return lock.getAndSet(true);
    }
}
