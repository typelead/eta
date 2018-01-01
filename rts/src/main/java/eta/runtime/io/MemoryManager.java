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
}
