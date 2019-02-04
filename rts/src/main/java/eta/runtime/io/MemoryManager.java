package eta.runtime.io;

import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;

import java.io.UnsupportedEncodingException;

import java.nio.Buffer;
import java.nio.ByteBuffer;

import eta.runtime.Runtime;
import eta.runtime.stg.Capability;
import eta.runtime.stg.WeakPtr;
import eta.runtime.storage.ManagedHeap;
import eta.runtime.storage.CachedBlock;
import eta.runtime.storage.Block;
import static eta.runtime.RuntimeLogging.*;

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
        if (n < 0) {
            throw new IllegalArgumentException("Allocated size must be positive");
        }
        if (n == 0) {
            return nullAddress;
        }
        long address = globalManagedHeap.allocateBuffer(n, direct, Capability.getLocal());
        if (Runtime.debugMemoryManager()) {
            debugMemoryManager("Allocating " + n  + " bytes " + (direct? "directly " : "") +
                               "at address " + address);
        }
        return address;
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
        if (Runtime.debugMemoryManager()) {
            debugMemoryManager("Freeing memory at address " + address);
        }
        if (address == nullAddress) {
            return;
        }
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
        boolean debug = Runtime.debugMemoryManager();
        if (debug) {
            debugMemoryManager("Doing work at " + address);
        }
        Block block = CachedBlock.getBlock(address);
        if (block == null) {
            block = getBlockSlow(address);
        }
        if (debug) {
            debugMemoryManager(address + " is at " + block);
        }
        return block;
    }

    private static Block getBlockSlow(long address) {
        Block block = globalManagedHeap.getBlock(address);
        CachedBlock.setBlock(address, block);
        return block;
    }

    /* When doing bulk operations, this can be useful. It returns a ByteBuffer
       positioned at the place referred to by the address. It's duplicated so the
       user is free to change the position as necessary.

       Throws an exception  if the block that corresponds to the address has been freed. */
    public static ByteBuffer getBoundedBuffer(long address) {
        if (address == nullAddress) {
          return emptyBuffer;
        }
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
            if (Runtime.debugStrings()) {
                debugStrings(Arrays.toString(bytes));
            }
            address         = allocateBuffer(bytes.length + 1, false);
            ByteBuffer dest = getBoundedBuffer(address);
            dest.put(bytes);
            dest.put((byte) 0);
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
            address         = allocateBuffer(size + 1, false);
            ByteBuffer dest = getBoundedBuffer(address);
            for (int i = 0; i < ss.length; i++) {
                dest.put(bytess[i]);
            }
            dest.put((byte) 0);
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
        ByteBuffer buf = getBoundedBuffer(ptr);
        ((Buffer)buf).position(buf.position() + offset);
        ((Buffer)buf).limit(buf.position() + length);
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

    public static ByteBuffer copyByteBufferDups( ByteBuffer src, ByteBuffer dest, int n) {
        return copyByteBufferDups(src, 0, dest, 0, n);
    }

    public static ByteBuffer copyByteBuffer( ByteBuffer src, int srcOffset
                                           , ByteBuffer dest, int destOffset
                                           , int n) {
        if (src == dest)
            return dest;
        ((Buffer)src).position(src.position() + srcOffset);
        ((Buffer)src).limit(src.position() + n);
        ((Buffer)dest).position(dest.position() + destOffset);
        dest.put(src);
        return dest;
    }

    public static ByteBuffer copyByteBufferDups( ByteBuffer src, int srcOffset
                                               , ByteBuffer dest, int destOffset
                                               , int n) {
        src = src.duplicate();
        dest = dest.duplicate();
        return copyByteBuffer(src, srcOffset, dest, destOffset, n);
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

    public static ByteBuffer set(long address, byte val, int size) {
        ByteBuffer buffer = getBoundedBuffer(address);
        while (size-- != 0) {
            buffer.put(val);
        }
        return buffer;
    }

    public static ByteBuffer set(long address, int val, int size) {
        return set(address, (byte) val, size);
    }


    public static ByteBuffer set(long address, byte[] bytes) {
        ByteBuffer buf = getBoundedBuffer(address);
        buf.put(bytes);
        return buf;
    }

    public static long allocateAndSet(byte[] bytes) {
        long address = allocateBuffer(bytes.length,true);
        set(address,bytes);
        return address;
    }

    public static ByteBuffer move(long srcAddress, long destAddress, int size) {
        ByteBuffer src  = getBoundedBuffer(srcAddress);
        ByteBuffer dest = getBoundedBuffer(destAddress);
        ByteBuffer copy = ByteBuffer.allocate(size);
        ((Buffer)src).limit(src.position() + size);
        copy.put(src);
        ((Buffer)copy).flip();
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

    public static ByteBuffer chr(ByteBuffer b, byte c, int n) {
        b = b.duplicate();
        int idx = chrIndex(b, c, n);
        if (idx == -1)
            return emptyBuffer;
        else
            return (ByteBuffer) ((Buffer)b).position(b.position() - 1);
    }

    public static ByteBuffer chr(ByteBuffer b, int c, int n) {
        return chr(b, (byte) c, n);
    }

    public static int chrIndex(ByteBuffer b, byte c, int n) {
        for (int i = 0; i < n ; i++) {
            byte nxt = b.get();
            if (nxt == c)
              return i;
        }
        return -1;
    }

    public static int chrIndex(ByteBuffer b, int c, int n) {
        return chrIndex(b, (byte) c, n);
    }

    public static int chrOffset(long address, int startofs, int endofs, byte c) {
        int n = endofs - startofs;
        int idxFound = chrIndex(getBoundedBuffer(address, startofs, n), c, n);
        return (idxFound == -1)? endofs : idxFound;
    }

    public static int chrOffset(long address, int startofs, int endofs, int c) {
        return chrOffset(address, startofs, endofs, (byte) c);
    }
}
