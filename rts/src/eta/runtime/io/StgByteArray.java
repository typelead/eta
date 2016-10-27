package eta.runtime.io;

import java.util.TreeMap;
import java.util.Map;
import java.nio.ByteBuffer;
import java.lang.ref.WeakReference;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;

public final class StgByteArray extends StgClosure {

    private static TreeMap<Integer, WeakReference<ByteBuffer>> addressMap =
        new TreeMap<Integer, WeakReference<ByteBuffer>>();

    private static Object lock = new Object();

    private static int nextAddress;

    public static StgByteArray create(int n) {
        return create(n, false);
    }

    public static StgByteArray create(int n, boolean pinned) {
        return create(n, 0, pinned);
    }

    public static StgByteArray create(int n, int alignment, boolean pinned) {
        ByteBuffer buf;
        synchronized (lock) {
            // TODO: Handle alignment
            // TODO: Collect allocation statistics
            // TODO: Revamp this implementation once the GC layer is added
            if (pinned) {
                buf = ByteBuffer.allocateDirect(n); // Off-heap memory
            } else {
                buf = ByteBuffer.allocate(n); // Heap memory
            }
            // addressMap.put(nextAddress, new WeakReference<ByteBuffer>(buf));
            // TODO: Add a check to crash if nextAddress overflows
            nextAddress += n;
        }
        return new StgByteArray(buf);
    }

    // public static ByteBuffer getBuffer(int address) {
    //     Map.Entry<Integer, WeakReference<ByteBuffer>>
    //         entry = addressMap.floorEntry(address);
    //     int pos = address - entry.getKey();
    //     ByteBuffer ref = entry.getValue().get();
    //     if (ref != null) {
    //         return (ByteBuffer) ref.duplicate().position(pos);
    //     } else {
    //         barf("Invalid address!");
    //         return null;
    //     }
    // }

    // TODO: Is synchronization necessary?
    public ByteBuffer buf;

    private StgByteArray(ByteBuffer buf) {
        this.buf = buf;
    }

    @Override
    public StgClosure getEvaluated() { return this; }

    @Override
    public void enter(StgContext context) {
        barf("StgByteArray object entered!");
    }

    public static void copyAddrToByteArray( ByteBuffer src, StgClosure destArray
                                          , int offset, int n) {
        ByteBuffer dest = (ByteBuffer) ((StgByteArray) destArray).buf;
        dest.position(offset);
        dest.put(src);
        src.rewind();
        dest.rewind();
    }

    public static void copyByteArrayToAddr( StgClosure srcArray, int offset
                                          , ByteBuffer dest, int n) {
        ByteBuffer src = (ByteBuffer) ((StgByteArray) srcArray).buf;
        src.position(offset);
        src.limit(offset + n);
        dest.put(src);
        dest.rewind();
        src.clear();
    }

    public static void copyByteArray( StgClosure srcArray, int srcOffset
                                    , StgClosure destArray, int destOffset, int n) {
        ByteBuffer src = (ByteBuffer) ((StgByteArray) srcArray).buf;
        ByteBuffer dest = (ByteBuffer) ((StgByteArray) destArray).buf;
        src.position(srcOffset);
        src.limit(srcOffset + n);
        dest.position(destOffset);
        dest.put(src);
        dest.rewind();
        src.clear();
    }
}
