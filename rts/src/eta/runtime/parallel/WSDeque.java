package eta.runtime.parallel;

import java.util.concurrent.atomic.AtomicLong;

import static eta.runtime.RtsMessages.barf;

public class WSDeque<E> {
    /* TODO: Maybe try using long w/ unsafe.compareAndSetLong rather than AtomicLong? */
    private Object[] elements;
    private AtomicLong top = new AtomicLong(0);
    private volatile long bottom;
    private volatile long topBound;
    private long moduloSize;
    private long size;

    private static long roundUp2(long val) {
        long rounded = 1;
        if (val == 0) {
            barf("DeQue,roundUp2: invalid size 0 requested");
        }
        do {
            rounded = rounded << 1;
        } while (0 != (val = val>>1));
        return rounded;
    }

    public WSDeque(long size) {
        /* ASSUMPTION: size > 0 */
        int realSize = (int) roundUp2(size);
        this.size = realSize;
        this.elements = new Object[realSize];
        this.moduloSize = realSize - 1;
    }

    public E pop() {
        long b = bottom;
        b--;
        bottom = b;
        long t = top.get();
        topBound = t;
        long curSize = b - t;
        if (curSize < 0) {
            bottom = t;
            return null;
        }
        @SuppressWarnings("unchecked")
        E removed = (E) elements[(int)(b & moduloSize)];
        if (curSize > 0) {
            return removed;
        }
        if (!top.compareAndSet(t, t + 1)) {
            removed = null;
        }
        bottom = t + 1;
        topBound = t + 1;
        return removed;
    }

    public E trySteal() {
        long t = top.get();
        long b = bottom;
        if (b - t <= 0) {
            return null;
        }
        @SuppressWarnings("unchecked")
        E stolen = (E) elements[(int)(t & moduloSize)];
        if (!top.compareAndSet(t, t + 1)) {
            return null;
        }
        return stolen;
    }

    public E steal() {
        E stolen;
        do {
            stolen = trySteal();
        } while (stolen == null && !isEmpty());
        return stolen;
    }

    public boolean push(Object elem) {
        long b = bottom;
        long t = topBound;
        if (b - t >= moduloSize) {
            t = top.get();
            topBound  = t;
            if (b - t >= moduloSize) {
                return false;
            }
        }
        elements[(int)(b & moduloSize)] = elem;
        bottom = b + 1;
        return true;
    }

    public long size() {
        return (bottom - top.get());
    }

    public boolean isEmpty() {
        return size() <= 0;
    }

    public void discardElements() {
        top.set(bottom);
    }

}
