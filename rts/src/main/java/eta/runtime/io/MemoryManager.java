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
import eta.runtime.storage.Block;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.RuntimeLogging.debugMemoryManager;

public class MemoryManager {

    /* TODO: Replace these with configurable parameters */
    private static final int MANAGED_HEAP_NURSERY_SIZE = 1024;
    private static final int MANAGED_HEAP_BLOCK_SIZE   = 4096;
    private static final int MANAGED_HEAP_MINIBLOCK_SIZE = 64;
    private static final ManagedHeap globalManagedHeap =
        new ManagedHeap(MANAGED_HEAP_NURSERY_SIZE, MANAGED_HEAP_BLOCK_SIZE);

    /** Allocating Off-Heap Memory **/


    /* Buffer Allocation */
    public static long allocateBuffer(int n, boolean direct) {
        Capability cap = Capability.getLocal(false);
        long address = cap.allocateLocal(n, direct);
        if (address == 0) {
            Block block = globalManagedHeap.allocateBlock(n, direct);
            address = block.getAddress();
            cap.setCurrentBlock(block, direct);
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
        /* TODO: Implement */
    }


    /* FIXME: For some reason, cleanup() causes bugs in the future invocations
       of the Eta RTS from the same JVM. */
    /* This is dangerous if done at any time other than shutdown.
       It will wipe out all information and free any remaining data. */
    public static void cleanup() {
        /* TODO: Implement */
    }

    /** Byte Buffer API to MemoryManager **/

    /*  This caches the last lookup for reducing the constant factor when
    you're doing a lot of writes/reads on a single buffer (the common case). */
    private static final class CachedBuffer {
        CachedBuffer(long lowerAddress, long higherAddress, ByteBuffer buffer) {
            this.lowerAddress = lowerAddress;
            this.higherAddress = higherAddress;
            this.buffer = buffer;
        }
        /* Start of previous received block. */
        final long lowerAddress;
        /* Start of the adjacent block.
           NOTE: This is the start of the NEXT block so you must do a strict comparison. */
        final long higherAddress;
        /* The cached buffer */
        final ByteBuffer buffer;
    }

    /* The cached buffer */
    private static final ThreadLocal<CachedBuffer> cachedBuffer = new ThreadLocal<CachedBuffer>();

    /* The shared empty buffer */
    private final static ByteBuffer emptyBuffer = ByteBuffer.allocate(0);

    private static ByteBuffer getBuffer(long address) {
        CachedBuffer cb = cachedBuffer.get();
        if (cb != null && address >= cb.lowerAddress && address < cb.higherAddress) {
            return cb.buffer;
        }
        return getBufferSlow(address);
    }

    private static ByteBuffer getBufferSlow(long address) {
        /* TODO: Implement */
        return null;
    }

    /* When doing bulk operations, this can be useful. It returns a ByteBuffer
       positioned at the place referred to by the address. It's duplicated so the
       user is free to change the position as necessary.

       Throws an exception  if the block that corresponds to the address has been freed. */
    public static ByteBuffer getBoundedBuffer(long address) {
        /* TODO: Implement */
        return null;
    }

    /* This returns -1 if `address` has already been freed. */
    public static int allocatedSize(long address) {
        /* TODO: Implement */
        return 0;
    }

    public static int positionIndex(long address) {
        /* TODO: Implement */
        return 0;
    }

    /** Read APIs **/
    public static byte get(long address) {
        return getBuffer(address).get(positionIndex(address));
    }

    public static short getShort(long address) {
        return getBuffer(address).getShort(positionIndex(address));
    }

    public static char getChar(long address) {
        return getBuffer(address).getChar(positionIndex(address));
    }

    public static int getInt(long address) {
        return getBuffer(address).getInt(positionIndex(address));
    }

    public static long getLong(long address) {
        return getBuffer(address).getLong(positionIndex(address));
    }

    public static float getFloat(long address) {
        return getBuffer(address).getFloat(positionIndex(address));
    }

    public static double getDouble(long address) {
        return getBuffer(address).getDouble(positionIndex(address));
    }

    /** Write APIs **/
    public static void put(long address, byte val) {
        getBuffer(address).put(positionIndex(address), val);
    }

    public static void putShort(long address, short val) {
        getBuffer(address).putShort(positionIndex(address), val);
    }

    public static void putChar(long address, char val) {
        getBuffer(address).putChar(positionIndex(address), val);
    }

    public static void putInt(long address, int val) {
        getBuffer(address).putInt(positionIndex(address), val);
    }

    public static void putLong(long address, long val) {
        getBuffer(address).putLong(positionIndex(address), val);
    }

    public static void putFloat(long address, float val) {
        getBuffer(address).putFloat(positionIndex(address), val);
    }

    public static void putDouble(long address, double val) {
        getBuffer(address).putDouble(positionIndex(address), val);
    }

    /** Monitoring **/
    // private static void printHeading(String heading) {
    //     System.out.print("***");
    //     System.out.print(heading);
    //     System.out.println("***");
    // }

    // private static void printAddressMap(String heading, Map<Long, Integer> addresses) {
    //     printHeading(heading);

    //     if (addresses.size() > 0) {
    //         for (Map.Entry<Long, Integer> entry: addresses.entrySet()) {
    //             Long address = entry.getKey();
    //             Integer size = entry.getValue();
    //             Long end     = entry.getKey() + entry.getValue() - 1;
    //             System.out.println(address + "-" + end + " [" + size + " bytes]");
    //         }
    //     } else {
    //         System.out.println("None");
    //     }
    // }

    // private static void printBlocksMap(String heading, Map<Integer,Queue<Long>> blocks) {
    //     printHeading(heading);

    //     if (blocks.size() > 0) {
    //         for (Map.Entry<Integer, Queue<Long>> entry: blocks.entrySet()) {
    //             System.out.println("Region Size: " + entry.getKey() + " bytes");
    //             Queue<Long> freeAddresses = entry.getValue();
    //             if (freeAddresses.size() > 0) {
    //                 for (Long address: freeAddresses) {
    //                     System.out.println("  " + address);
    //                 }
    //             } else {
    //                 System.out.println("  None");
    //             }
    //         }
    //     } else {
    //         System.out.println("None");
    //     }
    // }

    // public static void dumpMemoryManager() {
    //     System.out.println("***Eta-Managed Off-Heap Memory***");
    //     printAddressMap("Allocated Direct Blocks", allocatedDirectBlocks);
    //     printAddressMap("Allocated Heap Blocks", allocatedHeapBlocks);
    //     printAddressMap("Free Direct Addresses", freeDirectAddresses);
    //     printAddressMap("Free Heap Addresses", freeHeapAddresses);
    //     printBlocksMap("Free Direct Blocks", freeDirectBlocks);
    //     printBlocksMap("Free Heap Blocks", freeHeapBlocks);
    // }

    // public static void printAddressMapVerbose(Map<Long, Integer> addresses) {
    //     for (Long address: addresses.keySet()) {
    //         System.out.println("@" + address + ":");
    //         ByteBuffer buf = getBoundedBuffer(address);
    //         int limit   = buf.limit();
    //         int current = buf.position();
    //         int pos = 0;
    //         boolean found = false;
    //         for (pos = limit - 1; pos >= current; pos--) {
    //             if (buf.get(pos) != 0) {
    //                 pos++;
    //                 found = true;
    //                 break;
    //             }
    //         }
    //         if (found) {
    //             byte[] bytes = new byte[pos - current];
    //             buf.get(bytes);
    //             try {
    //                 System.out.println(new String(bytes, "ISO-8859-1"));
    //             } catch (UnsupportedEncodingException e) {}
    //             System.out.println(Arrays.toString(bytes));
    //         } else {
    //             System.out.println("Empty");
    //         }
    //         System.out.println("---------------------");
    //     }
    // }

    // public static void dumpMemoryManagerVerbose() {
    //     System.out.println("Allocated Direct Blocks:");
    //     printAddressMapVerbose(allocatedDirectBlocks);
    //     System.out.println("Allocated Heap Blocks:");
    //     printAddressMapVerbose(allocatedHeapBlocks);
    // }

    // /** Misc. Utilities **/
    // private static String renderSize(int size) {
    //     return "["+ size + " bytes]";
    // }

    // private static String renderIsDirect(boolean direct) {
    //     return direct? "Direct" : "Heap";
    // }

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
