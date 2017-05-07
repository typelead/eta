package eta.runtime.concurrent;

import java.util.Deque;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.concurrent.Concurrent.SPIN_COUNT;

public class StgMVar extends StgClosure {
    public BlockingQueue<StgClosure> valQueue = new ArrayBlockingQueue<StgClosure>(1, true);
    public AtomicBoolean lock = new AtomicBoolean(false);

    public StgMVar(StgClosure value) {
        if (value != null) {
            valQueue.offer(value);
        }
    }

    @Override
    public void enter(StgContext context) {
        barf("MVAR object entered!");
    }

    public StgClosure take() throws InterruptedException {
        return valQueue.take();
    }

    public void put(StgClosure closure) throws InterruptedException {
        valQueue.put(closure);
    }

    public StgClosure read() {
        StgClosure val;
        do {
            val = tryRead();
        } while (val == null);
        return val;
    }

    public StgClosure tryRead() {
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

    @Override
    public StgClosure getEvaluated() { return this; }
}
