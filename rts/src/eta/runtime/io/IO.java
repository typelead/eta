package eta.runtime.io;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.PhantomReference;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.Ap2Upd;
import eta.runtime.thunk.SelectorPUpd;

public class IO {

    /* I/O primitive operations */
    public static Closure decodeFloat_Int(StgContext context, float f) {
        int bits = Float.floatToRawIntBits(f);
        int s = ((bits >> 31) == 0) ? 1 : -1;
        int e = ((bits >> 23) & 0xff);
        int m = (e == 0) ?
            (bits & 0x7fffff) << 1 :
            (bits & 0x7fffff) | 0x800000;
        context.I(1, s * m);
        context.I(2, e - 150);
        return null;
    }

    public static Closure atomicModifyMutVar(StgContext context, MutVar mv, Closure f) {
        Ap2Upd z = new Ap2Upd(f, null);
        SelectorPUpd y = new SelectorPUpd(1, z);
        SelectorPUpd r = new SelectorPUpd(2, z);
        do {
            Closure x = mv.value;
            z.p2 = x;
            if (!mv.cas(x, y)) {
                continue;
            }
            mv.value = y;
            break;
        } while (true);
        return r;
    }

    public static Closure casMutVar(StgContext context, MutVar mv, Closure old, Closure update) {
        if (mv.cas(old, update)) {
            context.I(1, 0);
            return update;
        } else {
            context.I(1, 1);
            return mv.value;
        }
    }

    /* Managing ByteArrays */
    public static ReferenceQueue<ByteArray> byteArrayRefQueue = new ReferenceQueue<ByteArray>();
    public static AtomicBoolean byteArrayFreeLock = new AtomicBoolean();

    public static Map<PhantomReference<ByteArray>, Long> byteArrayRefMap
        = new ConcurrentHashMap<PhantomReference<ByteArray>, Long>();

    public static void recordByteArray(ByteArray byteArray) {
        long address = byteArray.bufferAddress;
        PhantomReference<ByteArray> byteArrayRef
            = new PhantomReference<ByteArray>(byteArray, byteArrayRefQueue);
        byteArrayRefMap.put(byteArrayRef, address);
    }

    @SuppressWarnings("unchecked")
    public static void checkForGCByteArrays() {
        /* This check can be skipped if another thread is doing it. */
        if (byteArrayFreeLock.compareAndSet(false, true)) {
            try {
                PhantomReference<ByteArray> ref;
                while ((ref = (PhantomReference<ByteArray>) byteArrayRefQueue.poll())
                       != null) {
                    Long address = byteArrayRefMap.get(ref);
                    if (address != null) {
                        MemoryManager.free(address);
                    }
                }
            } finally {
                byteArrayFreeLock.set(false);
            }
        }
    }

    /* Managing Non-Blocking I/O */

}
