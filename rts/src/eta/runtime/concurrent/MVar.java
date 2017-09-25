package eta.runtime.concurrent;

import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentLinkedQueue;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Value;

public class MVar extends Value {
    public Queue<Closure> valQueue = new ArrayBlockingQueue<Closure>(1, true);
    public Queue<TSO> listeners    = new ConcurrentLinkedQueue<TSO>();

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

    public void addListener(TSO tso) {
        listeners.offer(tso);
    }

    public TSO grabListener() {
        return listeners.poll();
    }
}
