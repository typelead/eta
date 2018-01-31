package eta.runtime.io;

import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Map;
import java.util.Queue;
import java.util.NavigableMap;
import java.util.IdentityHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.atomic.AtomicBoolean;

import java.io.UnsupportedEncodingException;

import java.nio.ByteBuffer;

import eta.runtime.Runtime;
import eta.runtime.stg.Capability;
import eta.runtime.stg.WeakPtr;
import eta.runtime.storage.ManagedHeap;
import eta.runtime.storage.CachedBlock;
import eta.runtime.storage.Block;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.RuntimeLogging.debugMemoryManager;

public class MemoryManager {

    /* TODO: Replace these with configurable parameters */
    /* Should be sufficiently large to make nursery allocations infrequent. */
    private static final int MANAGED_HEAP_NURSERY_SIZE = 1024;
    /* Should be equal to page size in bytes. */
    private static final int MANAGED_HEAP_BLOCK_SIZE   = 4096;
    /* Should be equal to cache line size in bytes. */
    private static final int MANAGED_HEAP_MINIBLOCK_SIZE = 64;
    private static ManagedHeap globalManagedHeap =
        new ManagedHeap(MANAGED_HEAP_NURSERY_SIZE,
                        MANAGED_HEAP_BLOCK_SIZE,
                        MANAGED_HEAP_MINIBLOCK_SIZE);

    /* The shared empty buffer */
    public final static long nullAddress = 0L;
    public final static ByteBuffer emptyBuffer = ByteBuffer.allocate(0);
    
    public static ManagedHeap getHeap() {
        return globalManagedHeap;
    }

    /** Allocating Off-Heap Memory **/

    /* Buffer Allocation */
    public static long allocateBuffer(int n, boolean direct) {
        return globalManagedHeap.allocateBuffer(n, direct, Capability.getLocal(false));
    }

    /** Freeing Off-Heap Memory **/

    public static void maybeFreeNativeMemory() {
        /* Free memory blocks associated with ByteArrays if the ByteArray itself
           has been garbage collected. */
        IO.checkForGCByteArrays();

        /* Check for any WeakPtr keys that have been GC'd and run both the
           Eta finalizers and Java finalizers. */
        WeakPtr.checkForGCWeakPtrs();
    }

    public static void free(long address) {
        globalManagedHeap.attemptFree(address);
    }


    /* FIXME: For some reason, cleanup() causes bugs in the future invocations
       of the Eta RTS from the same JVM. */
    /* This is dangerous if done at any time other than shutdown.
       It will wipe out all information and free any remaining data. */
    public static void cleanup() {
        globalManagedHeap =
            new ManagedHeap(MANAGED_HEAP_NURSERY_SIZE,
                            MANAGED_HEAP_BLOCK_SIZE,
                            MANAGED_HEAP_MINIBLOCK_SIZE);
    }

    /** Byte Buffer API to MemoryManager **/

    /* This is meant to be a read-only buffer, do not modify it.
       You should almost never have a use for this. */
    private static Block getBlock(long address) {
        Block block = CachedBlock.getBlock(address);
        if (block == null) {
            block = getBlockSlow(address);
        }
        return block;
    }

    private static Block getBlockSlow(long address) {
        Block block = globalManagedHeap.getBlock(address);
        CachedBlock.setBlock(block);
        return block;
    }

    /* When doing bulk operations, this can be useful. It returns a ByteBuffer
       positioned at the place referred to by the address. It's duplicated so the
       user is free to change the position as necessary.

       Throws an exception  if the block that corresponds to the address has been freed. */
    public static ByteBuffer getBoundedBuffer(long address) {
        if (address == nullAddress)
            return emptyBuffer;
        return getBlock(address).getBoundedBuffer(address);
    }

    public static int allocatedSize(long address) {
        return getBlock(address).allocatedSize(address);
    }

    /** Read APIs **/
    public static byte get(long address) {
        return getBlock(address).get(address);
    }

    public static short getShort(long address) {
        return getBlock(address).getShort(address);
    }

    public static char getChar(long address) {
        return getBlock(address).getChar(address);
    }

    public static int getInt(long address) {
        return getBlock(address).getInt(address);
    }

    public static long getLong(long address) {
        return getBlock(address).getLong(address);
    }

    public static float getFloat(long address) {
        return getBlock(address).getFloat(address);
    }

    public static double getDouble(long address) {
        return getBlock(address).getDouble(address);
    }

    /** Write APIs **/
    public static void put(long address, byte val) {
        getBlock(address).put(address, val);
    }

    public static void putShort(long address, short val) {
        getBlock(address).putShort(address, val);
    }

    public static void putChar(long address, char val) {
        getBlock(address).putChar(address, val);
    }

    public static void putInt(long address, int val) {
        getBlock(address).putInt(address, val);
    }

    public static void putLong(long address, long val) {
        getBlock(address).putLong(address, val);
    }

    public static void putFloat(long address, float val) {
        getBlock(address).putFloat(address, val);
    }

    public static void putDouble(long address, double val) {
        getBlock(address).putDouble(address, val);
    }

    /** Monitoring **/

    public static void dumpMemoryManager() {
        throw new IllegalArgumentException("Implement dumpMemoryManager.");
    }

    public static void dumpMemoryManagerVerbose() {
        throw new IllegalArgumentException("Implement dumpMemoryManagerVerbose.");
    }

    /** Allocating constant strings **/
    public static ConcurrentHashMap<String, Long> loadedStrings
        = new ConcurrentHashMap<String, Long>();

    public static long loadString(String s, String charset) throws UnsupportedEncodingException {
        Long address = loadedStrings.get(s);
        if (address == null) {
            byte[] bytes    = s.getBytes(charset);
            address         = allocateBuffer(bytes.length, false);
            ByteBuffer dest = getBoundedBuffer(address);
            dest.put(bytes);
            loadedStrings.put(s, address);
        }
        return address;
    }

    public static long loadStrings(String[] ss, String charset) throws UnsupportedEncodingException {
        Long address = loadedStrings.get(ss[0]);
        if (address == null) {
            byte[][] bytess = new byte[ss.length][];
            int size = 0;
            for (int i = 0; i < ss.length; i++) {
                byte[] ssBytes = ss[i].getBytes(charset);
                size += ssBytes.length;
                bytess[i] = ssBytes;
            }
            address         = allocateBuffer(size, false);
            ByteBuffer dest = getBoundedBuffer(address);
            for (int i = 0; i < ss.length; i++) {
                dest.put(bytess[i]);
            }
            loadedStrings.put(ss[0], address);
        }
        return address;
    }

    public static long loadStringLatin1(String s) throws UnsupportedEncodingException
    {
        return loadString(s, "ISO-8859-1");
    }

    public static long loadStringsLatin1(String[] ss) throws UnsupportedEncodingException
    {
        return loadStrings(ss, "ISO-8859-1");
    }

    public static long loadStringUTF8(String s) throws UnsupportedEncodingException
    {
        return loadString(s, "UTF-8");
    }

    public static long loadStringsUTF8(String[] ss) throws UnsupportedEncodingException
    {
        return loadStrings(ss, "UTF-8");
    }

    public static ByteBuffer getBoundedBuffer(long ptr, int offset, int length) {
        ByteBuffer buf = MemoryManager.getBoundedBuffer(ptr);
        buf.position(buf.position() + offset);
        buf.limit(buf.position() + length);
        return buf;
    }

    public static byte[] getBytes(long ptr, int offset, int length) {
        ByteBuffer buf = getBoundedBuffer(ptr,offset,length);
        byte[] b = new byte[buf.remaining()];
        buf.get(b);
        return b;
    }
    
    public static ByteBuffer copyByteBuffer( ByteBuffer src, ByteBuffer dest, int n) {
        return copyByteBuffer(src, 0, dest, 0, n);
    }
    
    public static ByteBuffer copyByteBuffer( ByteBuffer src, int srcOffset
                                           , ByteBuffer dest, int destOffset
                                           , int n) {
        dest = dest.duplicate();
        src = src.duplicate();
        return copyByteBufferDirect(src, srcOffset, dest, destOffset, n);
    }

     public static ByteBuffer copyByteBufferDirect( ByteBuffer src, int srcOffset
                                           , ByteBuffer dest, int destOffset
                                           , int n) {
        src.position(src.position() + srcOffset);
        src.limit(src.position() + n);
        dest.position(dest.position() + destOffset);
        dest.put(src);
        return dest;
    }
    
    public static ByteBuffer copy( long srcAddress, int srcOffset
                                 , ByteBuffer dest, int destOffset
                                 , int n) {
        ByteBuffer src = getBoundedBuffer(srcAddress);
        return copyByteBuffer(src,srcOffset,dest,destOffset,n);
    }

    public static ByteBuffer copy( ByteBuffer src, int srcOffset
                                 , long destAddress, int destOffset
                                 , int n) {
        ByteBuffer dest = getBoundedBuffer(destAddress);
        return copyByteBuffer(src,srcOffset,dest,destOffset,n);
    }

    
    public static ByteBuffer copy( long srcAddress, int srcOffset
                                 , long destAddress, int destOffset
                                 , int size) {
        ByteBuffer src  = getBoundedBuffer(srcAddress);
        ByteBuffer dest = getBoundedBuffer(destAddress);
        return copyByteBuffer(src,srcOffset,dest,destOffset,size);
    }

    public static ByteBuffer copy(long srcAddress, long destAddress, int size) {
        return copy(srcAddress, 0, destAddress, 0, size);
    }

    public static ByteBuffer set(long address, int val, int size) {
        ByteBuffer buffer = getBoundedBuffer(address);
        while (size-- != 0) {
            buffer.put((byte) val);
        }
        return buffer;
    }
       
    public static ByteBuffer set(long address, byte[] bytes) {
        ByteBuffer buf = getBoundedBuffer(address);
        buf.put(bytes);
        buf.clear();
        return buf;
    }

    public static ByteBuffer move(long srcAddress, long destAddress, int size) {
        ByteBuffer src  = getBoundedBuffer(srcAddress);
        ByteBuffer dest = getBoundedBuffer(destAddress);
        ByteBuffer copy = ByteBuffer.allocate(size);
        src.limit(src.position() + size);
        copy.put(src);
        copy.flip();
        dest.put(copy);
        return dest;
    }

    public static int compare(long a1, long a2, int n)  {
        ByteBuffer b1 = getBoundedBuffer(a1);
        ByteBuffer b2 = getBoundedBuffer(a2);
        return compare(b1, b2, n);
    }

    public static int compare(ByteBuffer b1, ByteBuffer b2, int n)  {
        while (n-- != 0) {
            int a = b1.get() & 0xFF;
            int b = b2.get() & 0xFF;
            if (a != b) {
                return a - b;
            }
        }
        return 0;
    }

    public static int compare(long a1, int o1, long a2, int o2, int n) {
        return compare(getBoundedBuffer(a1,o1,n), getBoundedBuffer(a2,o2,n), n);
    }

    public static int compare(long a1, int o1, ByteBuffer b2, int n) {
        return compare(getBoundedBuffer(a1,o1,n), b2, n);
    }

    public static int compare(ByteBuffer b1, long a2, int o2, int n) {
        return compare(b1, getBoundedBuffer(a2,o2,n), n);
    }

    public static ByteBuffer chr(ByteBuffer b, int c, int n) {
        c = (int)((byte) c);
        int idx = chrIndex(b, c, n);
        if (idx == 0)
            return emptyBuffer;
        else
            return (ByteBuffer) b.position(b.position() + idx);
    }

    private static int chrIndex(ByteBuffer b, int c, int n) {
        c = (int)((byte) c);
        b = b.duplicate();
        while (n-- != 0) {
            if (b.get() == c) {
                break;
            }
        }
        return n;
    }
    
    public static int chrAddress(long address, int startofs, int endofs, int c) {
        int n = endofs - startofs;
        long idxFound = chrIndex(getBoundedBuffer(address, startofs, n), c, n);
        return (idxFound == 0L)? endofs : (int)(address + startofs + idxFound);
    }

    public static long allocateAndSet(byte[] bytes) {
        long address = allocateBuffer(bytes.length,true);
        set(address,bytes);
        return address;
    }
}
