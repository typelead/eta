package eta.runtime.io;

import java.util.TreeMap;
import java.util.Map;
import java.nio.ByteBuffer;
import java.lang.ref.WeakReference;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;

public final class StgByteArray extends StgValue {

    public static StgByteArray create(int n) {
        return create(n, false);
    }

    public static StgByteArray create(int n, boolean pinned) {
        return create(n, 0, pinned);
    }

    public static StgByteArray create(int n, int alignment, boolean pinned) {
        return new StgByteArray(MemoryManager.allocateBuffer(n, pinned));
    }

    public ByteBuffer buf;

    private StgByteArray(ByteBuffer buf) {
        this.buf = buf;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("StgByteArray object entered!");
        return null;
    }

    /* MemoryManager-Sensitive */
    public static void copyAddrToByteArray( ByteBuffer src, StgByteArray destArray
                                          , int offset, int n) {
        ByteBuffer dest = destArray.buf.duplicate();
        src = src.duplicate();
        dest.position(offset + 4);
        dest.put(src);
    }

    /* MemoryManager-Sensitive */
    public static void copyByteArrayToAddr( StgByteArray srcArray, int offset
                                          , ByteBuffer dest, int n) {
        ByteBuffer src = srcArray.buf.duplicate();
        dest = dest.duplicate();
        src.position(offset + 4);
        src.limit(offset + 4 + n);
        dest.put(src);
    }

    /* MemoryManager-Sensitive */
    public static void copyByteArray( StgByteArray srcArray, int srcOffset
                                    , StgByteArray destArray, int destOffset
                                    , int n) {
        ByteBuffer src  = srcArray.buf.duplicate();
        ByteBuffer dest = destArray.buf.duplicate();
        src.position(srcOffset + 4);
        src.limit(srcOffset + 4 + n);
        dest.position(destOffset + 4);
        dest.put(src);
    }
}
