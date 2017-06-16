package eta.runtime.io;

import java.util.TreeMap;
import java.util.Map;
import java.nio.ByteBuffer;
import java.lang.ref.WeakReference;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RuntimeLogging.barf;

public final class ByteArray extends Value {

    public static ByteArray create(int n) {
        return create(n, false);
    }

    public static ByteArray create(int n, boolean pinned) {
        return create(n, 0, pinned);
    }

    public static ByteArray create(int n, int alignment, boolean pinned) {
        long address = MemoryManager.allocateBuffer(n, pinned);
        ByteArray byteArray = new ByteArray(address, n);
        IO.recordByteArray(byteArray);
        return byteArray;
    }

    public int  size;
    public long bufferAddress;

    private ByteArray(long bufferAddress, int size) {
        this.size          = size;
        this.bufferAddress = bufferAddress;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("ByteArray object entered!");
        return null;
    }

    public static void copyAddrToByteArray( long srcAddress, ByteArray destArray
                                          , int offset, int n) {
        ByteBuffer dest = MemoryManager.getBoundedBuffer(destArray.bufferAddress);
        ByteBuffer src  = MemoryManager.getBoundedBuffer(srcAddress);
        dest.position(dest.position() + offset);
        dest.limit(dest.position() + n);
        dest.put(src);
    }

    public static void copyByteArrayToAddr( ByteArray srcArray, int offset
                                          , long destAddress, int n) {
        ByteBuffer src  = MemoryManager.getBoundedBuffer(srcArray.bufferAddress);
        ByteBuffer dest = MemoryManager.getBoundedBuffer(destAddress);
        src.position(src.position() + offset);
        src.limit(src.position() + n);
        dest.put(src);
    }

    public static void copyByteArray( ByteArray srcArray, int srcOffset
                                    , ByteArray destArray, int destOffset
                                    , int n) {
        ByteBuffer src  = MemoryManager.getBoundedBuffer(srcArray.bufferAddress);
        ByteBuffer dest = MemoryManager.getBoundedBuffer(destArray.bufferAddress);
        src.position(src.position() + srcOffset);
        src.position(src.position() + n);
        dest.position(dest.position() + destOffset);
        dest.put(src);
    }
}
