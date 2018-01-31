package eta.runtime.io;

import java.util.Arrays;
import java.util.TreeMap;
import java.util.Map;
import java.nio.ByteBuffer;
import java.lang.ref.WeakReference;

import eta.runtime.stg.Closure;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;
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
        if (n > 0) {
            IO.recordByteArray(byteArray);
        }
        return byteArray;
    }

    public int  size;
    public long bufferAddress;

    private ByteArray(long bufferAddress, int size) {
        this.size          = size;
        this.bufferAddress = bufferAddress;
    }

    @Override
    public String toString() {
        ByteBuffer buf = getBuffer();
        byte[] bytes = new byte[size];
        buf.get(bytes);
        return "ByteArray " + Arrays.toString(bytes);
    }

    public ByteBuffer getBuffer() {
        return MemoryManager.getBoundedBuffer(bufferAddress);
    }

    public ByteBuffer getBuffer(int offset, int n) {
        return MemoryManager.getBoundedBuffer(bufferAddress, offset, n);
    }

    public static ByteBuffer copyAddrToByteArray( long srcAddress, ByteArray destArray
                                                , int offset, int n) {
        return MemoryManager.copy(srcAddress,offset,
                                  destArray.getBuffer(),offset,n);
    }

    public static ByteBuffer copyByteArrayToAddr( ByteArray srcArray, int offset
                                                , long destAddress, int n) {
        return MemoryManager.copy(srcArray.getBuffer(),offset,
                                  destAddress,offset,n);
    }

    public static ByteBuffer copyByteArray( ByteArray srcArray, int srcOffset
                                          , ByteArray destArray, int destOffset
                                          , int n) {
        return MemoryManager.copyByteBuffer(srcArray.getBuffer(),srcOffset,
                                  destArray.getBuffer(),destOffset,n);
    }

    public static ByteBuffer copyByteArray( ByteArray srcArray, int srcOffset
                                          , ByteBuffer dest, int destOffset
                                          , int n) {
        return MemoryManager.copyByteBuffer(srcArray.getBuffer(),srcOffset,
                                  dest,destOffset,n);
    }

    public static ByteBuffer copyByteArray( ByteBuffer src, int srcOffset
                                           , ByteArray destArray, int destOffset
                                           , int n) {
        return MemoryManager.copyByteBuffer(src,srcOffset,
                                  destArray.getBuffer(),destOffset,n);
    }

    public static ByteBuffer copyByteArray(ByteArray srcArray, ByteArray destArray, int size) {
        return copyByteArray(srcArray, 0, destArray, 0, size);
    }

    public static ByteBuffer copyByteArray(ByteBuffer srcBuffer, ByteArray destArray, int size) {
        return copyByteArray(srcBuffer, 0, destArray,0, size);
    }

    public static ByteBuffer copyByteArray(ByteArray srcArray, ByteBuffer destBuffer, int size) {
        return copyByteArray(srcArray, 0, destBuffer, 0, size);
    }

    public static int compare(ByteArray b1, ByteArray b2, int n) {
        return MemoryManager.compare(b1.getBuffer(), b2.getBuffer(), n);
    }

    public static int compare(ByteArray b1, int o1, ByteArray b2, int o2, int n) {
        return MemoryManager.compare(b1.getBuffer(o1,n), b2.getBuffer(o2,n), n);
    }

    public static int compare(ByteArray b1, int o1, long a2, int o2, int n) {
        return MemoryManager.compare(b1.getBuffer(o1,n), a2, o2, n);
    }

    public static int compare(long a1, int o1, ByteArray b2, int o2, int n) {
        return MemoryManager.compare(a1, o1, b2.getBuffer(o2,n), n);
    }
}
