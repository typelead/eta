package eta.runtime.io;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Queue;
import java.util.NavigableMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.atomic.AtomicBoolean;

import java.nio.ByteBuffer;

import eta.runtime.stg.WeakPtr;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.RuntimeLogging.debugMemoryManager;

public class MemoryManager {

    /** Allocating Off-Heap Memory **/

    /* TODO: Optimize this to make a specialized data structure that stores
             primitive ints & longs. */
    /* Map block sizes to addresses of free blocks */
    public static final NavigableMap<Integer, Queue<Long>> freeDirectBlocks
        = new ConcurrentSkipListMap<Integer, Queue<Long>>();
    public static final NavigableMap<Integer, Queue<Long>> freeHeapBlocks
        = new ConcurrentSkipListMap<Integer, Queue<Long>>();

    /* Locks on each size */
    public static Map<Integer, SizeLock> directSizeLocks
        = new ConcurrentHashMap<Integer, SizeLock>();

    public static AtomicBoolean directSizeLocksLock
        = new AtomicBoolean();

    public static Map<Integer, SizeLock> heapSizeLocks
        = new ConcurrentHashMap<Integer, SizeLock>();

    public static AtomicBoolean heapSizeLocksLock
        = new AtomicBoolean();

    public static SizeLock getSizeLock
        (Map<Integer, SizeLock> sizeLocks, AtomicBoolean sizeLocksLock,
         Integer key) {
        SizeLock sizeLock;
        for (;;) {
            sizeLock = sizeLocks.get(key);
            if (sizeLock == null) {
                if (sizeLocksLock.compareAndSet(false, true)) {
                    try {
                        sizeLock = new SizeLock();
                        sizeLocks.put(key, sizeLock);
                    } finally {
                        sizeLocksLock.set(false);
                    }
                } else continue;
            }
            return sizeLock;
        }
    }

    /* Map addresses to size of free blocks */
    public static final NavigableMap<Long, Integer> freeDirectAddresses
        = new ConcurrentSkipListMap<Long, Integer>();
    public static final NavigableMap<Long, Integer> freeHeapAddresses
        = new ConcurrentSkipListMap<Long, Integer>();

    /* Map addresses to size of allocated blocks */
    public static final NavigableMap<Long, Integer> allocatedDirectBlocks
        = new ConcurrentSkipListMap<Long, Integer>();
    public static final NavigableMap<Long, Integer> allocatedHeapBlocks
        = new ConcurrentSkipListMap<Long, Integer>();

    /* Actual storage of blocks */
    public static List[] blockArrays =
        { new ArrayList()
        , new ArrayList()
        , new ArrayList()
        , new ArrayList() };

    /* Locks for each blockArray */
    public static AtomicBoolean[] blockLocks =
        { new AtomicBoolean()
        , new AtomicBoolean()
        , new AtomicBoolean()
        , new AtomicBoolean() };

    /* Buffer Allocation
       This logic is rather complicated but it is done for efficiency purposes. Each
       free block size has a lock associated with it which is taken whenever a
       modification is done for that size. That way, two concurrent threads that
       are trying to allocate two different sizes can do so without waiting.
     */
    public static long allocateBuffer(int n, boolean direct) {
        assert n <= ONE_GB;
        int     newRegionSize;
        long    newAddress;
        boolean attemptedGC = false;
        NavigableMap<Integer, Queue<Long>> freeBlocks;
        NavigableMap<Long, Integer> freeAddresses;
        NavigableMap<Long, Integer> allocatedBlocks;
        Map<Integer, SizeLock> sizeLocks;
        AtomicBoolean sizeLocksLock;
        if (direct) {
            allocatedBlocks = allocatedDirectBlocks;
            freeAddresses   = freeDirectAddresses;
            freeBlocks      = freeDirectBlocks;
            sizeLocks       = directSizeLocks;
            sizeLocksLock   = directSizeLocksLock;
        } else {
            allocatedBlocks = allocatedHeapBlocks;
            freeAddresses   = freeHeapAddresses;
            freeBlocks      = freeHeapBlocks;
            sizeLocks       = heapSizeLocks;
            sizeLocksLock   = heapSizeLocksLock;
        }
        main:
        for (;;) {
            Map.Entry<Integer, Queue<Long>> freeEntry = freeBlocks.ceilingEntry(n);
            if (freeEntry != null) {
                int           regionSize = freeEntry.getKey();
                SizeLock      sizeLock   = getSizeLock(sizeLocks, sizeLocksLock, regionSize);
                Queue<Long>   freeQueue  = freeEntry.getValue();
                Long address;
                /* We attempt to acquire a permit (exists if there is a queue element)
                   and upon failure, continue the loop in case changes were made to
                   freeBlocks. */
                if (sizeLock.tryAcquire()) {
                    assert freeQueue != null;
                    address = freeQueue.poll();
                    assert address != null;
                } else {
                    if (freeQueue.isEmpty() && sizeLock.tryStartTransaction()) {
                        try {
                            freeBlocks.remove(regionSize);
                        } finally {
                            sizeLock.endTransaction();
                        }
                    }
                    continue;
                }
                newRegionSize = regionSize - n;
                newAddress    = address + n;
                allocatedBlocks.put(address, n);
                debugMemoryManager("Allocate Block @ " + address + " " +
                                   renderSize(n) + ".");
                if (newRegionSize > 0) {
                    insertFreeBlock(freeBlocks, freeAddresses, sizeLocks,
                                    sizeLocksLock, newRegionSize, newAddress);
                }
                return address;
            } else {
                int blockType = getBlockType(n);
                do {
                    AtomicBoolean    blockLock = blockLocks[blockType];
                    @SuppressWarnings("unchecked") List<ByteBuffer> blocks
                        = blockArrays[blockType];
                    if (blocks.size() == MAX_BLOCK_INDEX) {
                        if (blockType == ONE_GB_BLOCK) {
                            if (!attemptedGC) {
                                /* Attempt to free native memory by first GC'ing
                                   any garbage ByteArrays so that some native
                                   memory can be freed. */
                                System.gc();
                                /* Give some time for the GC to work so that
                                   the reference queues get populated. */
                                try {
                                    Thread.sleep(1);
                                } catch (InterruptedException e) {}
                                maybeFreeNativeMemory();
                                /* Restart the top-level loop to see if any free blocks
                                   have been allocated. */
                                attemptedGC = true;
                                debugMemoryManager("MemoryManager Space Full." +
                                                   " GC and Retry.");
                                continue main;
                            } else {
                                throw new OutOfMemoryError("The Eta MemoryManager is unable to allocate more off-heap memory.");
                            }
                        }
                        /* INVARIANT: Each increment of blockType yields the next
                                      higher blockSize. */
                        blockType += 1;
                        continue;
                    }
                    if (blockLock.compareAndSet(false, true)) {
                        long address;
                        try {
                            long blockIndex = blocks.size();
                            int  blockSize  = getBlockSize(blockType);
                            address         = (blockType  << BLOCK_TYPE_BITS)
                                            | (blockIndex << indexBits(blockType));
                            /* Avoid allocating something on the null pointer address. */
                            if (address == 0) address = 1;
                            newRegionSize = blockSize - n;
                            newAddress    = address + n;
                            blocks.add(allocateAnonymousBuffer(blockSize, direct));
                            debugMemoryManager("Create " + renderIsDirect(direct) +
                                               " Block " + renderSize(blockSize) +
                                               ".");
                            allocatedBlocks.put(address, n);
                            debugMemoryManager("Allocated Block @ " + address + " " +
                                               renderSize(n) + ".");
                            insertFreeBlock(freeBlocks, freeAddresses, sizeLocks,
                                            sizeLocksLock, newRegionSize, newAddress);
                        } finally {
                            blockLock.set(false);
                        }
                        return address;
                    } else {
                        /* If unable to lock, continue the loop to see if free blocks
                           became available in the mean time. */
                        continue;
                    }
                } while (true);
            }
        }
    }

    /* TODO: We can make inserting free blocks asynchronous to speed up allocation
             a tiny bit, since we don't need to insert them to get the allocated
             address. This can be done with message passing and adding a bit of
             work to a Capability's idleLoop that reads from the message queue
             and inserts free blocks accordingly. The tradeoff with this approach
             is that new blocks may get allocated more if these free blocks
             don't get inserted in time. */
    public static void insertFreeBlock
        (NavigableMap<Integer, Queue<Long>> freeBlocks,
         NavigableMap<Long, Integer> freeAddresses,
         Map<Integer, SizeLock> sizeLocks,
         AtomicBoolean sizeLocksLock,
         int newRegionSize, long newAddress) {
        assert newRegionSize > 0;
        /* TODO: Implement block destroying to release memory when it's no longer
                 needed. Currently, if your application suddenly allocates a lot
                 of native memory, it will stay allocated for the lifetime of
                 the application. */
        debugMemoryManager("Free Block @ " + newAddress + " " +
                           renderSize(newRegionSize) + ".");
        /* TODO: Do we need to ensure atomicity of this entire function? */
        SizeLock newSizeLock
            = getSizeLock(sizeLocks, sizeLocksLock, newRegionSize);
        while (!newSizeLock.tryStartTransaction()) {}
        Queue<Long> freeQueue = freeBlocks.get(newRegionSize);
        if (freeQueue != null) {
            freeQueue.offer(newAddress);
        } else {
            freeQueue = new ConcurrentLinkedQueue<Long>();
            freeQueue.offer(newAddress);
            freeBlocks.put(newRegionSize, freeQueue);
        }
        newSizeLock.enlarge();
        newSizeLock.endTransaction();
        freeAddresses.put(newAddress, newRegionSize);
    }

    public static ByteBuffer allocateAnonymousBuffer(int n, boolean direct) {
        return (direct?
                /* Off-Heap Memory */
                ByteBuffer.allocateDirect(n):
                /* Heap Memory */
                ByteBuffer.allocate(n));
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
        NavigableMap<Integer, Queue<Long>> freeBlocks;
        NavigableMap<Long, Integer> allocatedBlocks;
        NavigableMap<Long, Integer> freeAddresses;
        Map<Integer, SizeLock> sizeLocks;
        AtomicBoolean sizeLocksLock;
        Integer sizeInt = allocatedDirectBlocks.get(address);
        boolean direct;
        if (sizeInt == null) {
            sizeInt = allocatedHeapBlocks.get(address);
            if (sizeInt == null) {
                /* This means that `address` was already freed. */
                return;
            } else {
                allocatedBlocks = allocatedHeapBlocks;
                /* Check if `address` was already freed. */
                if (allocatedBlocks.remove(address) == null) {
                    debugMemoryManager("Freed @ " + address + " " +
                                       renderSize(sizeInt) + ".");
                    return;
                }
                direct = false;
            }
        } else {
            allocatedBlocks = allocatedDirectBlocks;
            /* Check if `address` was already freed. */
            if (allocatedBlocks.remove(address) == null) {
                debugMemoryManager("Freed @ " + address + " " + renderSize(sizeInt) +
                                   ".");
                return;
            }
            direct = true;
        }
        debugMemoryManager("Free @ " + address + " " + renderSize(sizeInt) + ".");
        int size = sizeInt.intValue();
        if (direct) {
            freeAddresses   = freeDirectAddresses;
            freeBlocks      = freeDirectBlocks;
            sizeLocks       = directSizeLocks;
            sizeLocksLock   = directSizeLocksLock;
        } else {
            freeAddresses   = freeHeapAddresses;
            freeBlocks      = freeHeapBlocks;
            sizeLocks       = heapSizeLocks;
            sizeLocksLock   = heapSizeLocksLock;
        }
        Map.Entry<Long, Integer> lowerEntry  = freeAddresses.lowerEntry(address);
        Map.Entry<Long, Integer> higherEntry = freeAddresses.higherEntry(address);
        long lowerAddress  = lowerEntry.getKey();
        int  lowerSize     = lowerEntry.getValue();
        long higherAddress = higherEntry.getKey();
        int  higherSize    = higherEntry.getValue();
        long newAddress    = address;
        int  newSize       = size;
        SizeLock lowerRegionLock  = null;
        SizeLock higherRegionLock = null;

        /* After these two checks, lowerAddress will be the starting
           point of the new freeBlock and size will be the size of
           the new block. */
        try {
            if ((lowerAddress + lowerSize) == address && sameBlock(lowerAddress, address)) {
                lowerRegionLock = getSizeLock(sizeLocks, sizeLocksLock, lowerSize);
                while (!lowerRegionLock.tryStartTransaction()) {}
                newAddress = lowerAddress;
                newSize   += lowerSize;
            }
            if ((address + size) == higherAddress && sameBlock(address, higherAddress)) {
                higherRegionLock = getSizeLock(sizeLocks, sizeLocksLock, higherSize);
                while (!higherRegionLock.tryStartTransaction()) {}
                newSize += higherSize;
            }
            Queue<Long> freeQueue;
            if (lowerRegionLock  != null) {
                freeQueue = freeBlocks.get(lowerSize);
                /* If the free block was taken by the time we got the lock. */
                if (freeQueue == null) {
                    newAddress = address;
                    newSize   -= lowerSize;
                } else {
                    /* If the free block was taken by the time we got the lock. */
                    if (!freeQueue.remove(lowerAddress)) {
                        newAddress = address;
                        newSize   -= lowerSize;
                    } else {
                        /* Success, remove a permit */
                        lowerRegionLock.unconditionalAcquire();
                    }
                }
            }
            if (higherRegionLock != null) {
                freeQueue = freeBlocks.get(higherSize);
                /* If the free block was taken by the time we got the lock. */
                if (freeQueue == null) {
                    newSize -= higherSize;
                } else {
                    /* If the free block was taken by the time we got the lock. */
                    if (!freeQueue.remove(lowerAddress)) {
                        newSize -= higherSize;
                    } else {
                        /* Success, remove a permit */
                        higherRegionLock.unconditionalAcquire();
                    }
                }
            }
        } finally {
            if (lowerRegionLock  != null) lowerRegionLock.endTransaction();
            if (higherRegionLock != null) higherRegionLock.endTransaction();
        }
        insertFreeBlock(freeBlocks, freeAddresses, sizeLocks, sizeLocksLock,
                        newSize, newAddress);
    }

    /** Addresses
        The first two bits of the address determine the block type.
        The blocks come in four variants:
        *   1 MB block - 11 free bits, 31 block bits, 20 index bits
        *  16 MB block -  7 free bits, 31 block bits, 24 index bits
        * 128 MB block -  4 free bits, 31 block bits, 27 index bits
        *   1 GB block -  1 free bits, 31 block bits, 30 index bits
        Hence, the maximum you can allocate a single block for is 1GB.
        We may want to implement a custom ByteBuffer that can handle block borders. */
    public static final long BLOCK_TYPE_MASK = 0xC000000000000000L;
    public static final int  BLOCK_TYPE_BITS = 62;
    public static final int  MAX_BLOCK_INDEX = 0x7FFFFFFF;

    public static final int ONE_MB_INDEX_BITS           = 20;
    public static final int ONE_SIX_MB_INDEX_BITS       = 24;
    public static final int ONE_TWO_EIGHT_MB_INDEX_BITS = 27;
    public static final int ONE_GB_INDEX_BITS           = 30;

    public static final int ONE_MB           = 1 << ONE_MB_INDEX_BITS;
    public static final int ONE_SIX_MB       = 1 << ONE_SIX_MB_INDEX_BITS;
    public static final int ONE_TWO_EIGHT_MB = 1 << ONE_TWO_EIGHT_MB_INDEX_BITS;
    public static final int ONE_GB           = 1 << ONE_GB_INDEX_BITS;

    public static final int ONE_MB_BLOCK           = 0;
    public static final int ONE_SIX_MB_BLOCK       = 1;
    public static final int ONE_TWO_EIGHT_MB_BLOCK = 2;
    public static final int ONE_GB_BLOCK           = 3;

    public static int getBlockType(int n) {
        assert n <= ONE_GB;
        if      (n <= ONE_MB)           return ONE_MB_BLOCK;
        else if (n <= ONE_SIX_MB)       return ONE_SIX_MB_BLOCK;
        else if (n <= ONE_TWO_EIGHT_MB) return ONE_TWO_EIGHT_MB_BLOCK;
        else if (n <= ONE_GB)           return ONE_GB_BLOCK;
        else throw new IllegalArgumentException("Cannot allocate a block size greater than 1GB!");
    }

    public static int getBlockSize(int blockType) {
        switch (blockType) {
            case 0: return ONE_MB;
            case 1: return ONE_SIX_MB;
            case 2: return ONE_TWO_EIGHT_MB;
            case 3: return ONE_GB;
            default: barf("Bad index Mask"); return -1;
        }
    }

    public static boolean sameBlock(long address1, long address2) {
        int blockType1 = blockType(address1);
        int blockType2 = blockType(address2);
        if (blockType1 != blockType2) return false;
        return blockIndex(address1, indexBits(blockType1))
            == blockIndex(address2, indexBits(blockType2));
    }

    public static int blockType(long address) {
        return (int)((address & BLOCK_TYPE_MASK) >>> BLOCK_TYPE_BITS);
    }

    public static int indexBits(int blockType) {
        switch (blockType) {
            case 0: return ONE_MB_INDEX_BITS;
            case 1: return ONE_SIX_MB_INDEX_BITS;
            case 2: return ONE_TWO_EIGHT_MB_INDEX_BITS;
            case 3: return ONE_GB_INDEX_BITS;
            default: barf("Bad index Mask"); return -1;
        }
    }

    public static int blockIndex(long address, int indexBits) {
        return (int)((address & ~BLOCK_TYPE_MASK) >>> indexBits);
    }

    public static int positionIndex(long address) {
        return (int)(address & ((1 << indexBits(blockType(address))) - 1));
    }

    public static int positionIndex(long address, int indexBits) {
        return (int)(address & ((1 << indexBits) - 1));
    }

    /** Byte Buffer API to MemoryManager **/

    /*  This caches the last lookup for reducing the constant factor when
        you're doing a lot of writes/reads on a single buffer (the common case). */
    private static class ThreadLocalLong extends ThreadLocal<Long> {
        protected Long initialValue() {
            return new Long(0L);
        }
    }

    /* Start of previous received block. */
    public static ThreadLocal<Long> cachedLowerAddress = new ThreadLocalLong();

    /* Start of the adjacent block.
       NOTE: This is the start of the NEXT block so you must do a strict comparison. */
    public static ThreadLocal<Long> cachedHigherAddress = new ThreadLocalLong();

    /* The cached buffer */
    public static ThreadLocal<ByteBuffer> cachedBuffer = new ThreadLocal<ByteBuffer>();

    public static ByteBuffer getBuffer(long address) {
        if (address >= cachedLowerAddress.get() && address < cachedHigherAddress.get()) {
            ByteBuffer cached = cachedBuffer.get();
            if (cached != null) {
                return cached;
            }
        }
        int blockType  = blockType(address);
        int indexBits  = indexBits(blockType);
        int blockIndex = blockIndex(address, indexBits);
        long lower = (blockType << BLOCK_TYPE_BITS) | (blockIndex << indexBits);
        AtomicBoolean blockLock = blockLocks[blockType];
        ByteBuffer buf = null;
        if (blockLock.compareAndSet(false, true)) {
            try {
                buf = (ByteBuffer) blockArrays[blockType].get(blockIndex);
            } finally {
                blockLock.set(false);
            }
        }
        cachedLowerAddress.set(lower);
        cachedHigherAddress.set(lower + (1 << indexBits));
        cachedBuffer.set(buf);
        return buf;
    }

    /* Helper function that will find an allocated block below the one given. */
    private static Map.Entry<Long, Integer>
        findLowerAllocatedAddress(NavigableMap<Long, Integer> allocatedBlocks
                                 ,long address) {
        Map.Entry<Long, Integer>
            lowerEntry = allocatedBlocks.floorEntry(Long.valueOf(address));
        if (lowerEntry != null &&
            (address < (lowerEntry.getKey() + lowerEntry.getValue()))) {
            return lowerEntry;
        }
        return null;
    }

    /* When doing bulk operations, this can be useful. It returns a ByteBuffer
       positioned at the place referred to by the address. It's duplicated so the
       user is free to change the position as necessary.

       Returns null if the block that corresponds to the address has been freed. */
    public static ByteBuffer getBoundedBuffer(long address) {
        Map.Entry<Long, Integer>
            lowerEntry = findLowerAllocatedAddress(allocatedDirectBlocks, address);
        if (lowerEntry == null) {
            lowerEntry = findLowerAllocatedAddress(allocatedHeapBlocks, address);
            if (lowerEntry == null) {
                return null;
            }
        }
        long lowerAddress       = lowerEntry.getKey();
        int  lowerSize          = lowerEntry.getValue();
        int blockType           = blockType(address);
        int indexBits           = indexBits(blockType);
        int blockIndex          = blockIndex(address, indexBits);
        int positionIndex       = positionIndex(address, indexBits);
        int size                = (int)(lowerAddress + lowerSize - address);
        ByteBuffer buf          = null;
        AtomicBoolean blockLock = blockLocks[blockType];
        if (blockLock.compareAndSet(false, true)) {
            try {
                buf = ((ByteBuffer) blockArrays[blockType].get(blockIndex)).duplicate();
            } finally {
                blockLock.set(false);
            }
        }
        buf.position(positionIndex);
        buf.limit(positionIndex + size);
        return buf;
    }

    /* This returns -1 if `address` has already been freed. */
    public static int allocatedSize(long address) {
        Integer sizeInt = allocatedDirectBlocks.get(address);
        if (sizeInt == null) {
            sizeInt = allocatedHeapBlocks.get(address);
            if (sizeInt == null) {
                return -1;
            }
        }
        return sizeInt.intValue();
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
    private static void printHeading(String heading) {
        System.out.print("***");
        System.out.print(heading);
        System.out.println("***");
    }

    private static void printAddressMap(String heading, Map<Long, Integer> addresses) {
        printHeading(heading);

        if (addresses.size() > 0) {
            for (Map.Entry<Long, Integer> entry: addresses.entrySet()) {
                Long address = entry.getKey();
                Integer size = entry.getValue();
                Long end     = entry.getKey() + entry.getValue() - 1;
                System.out.println(address + "-" + end + " [" + size + " bytes]");
            }
        } else {
            System.out.println("None");
        }
    }

    private static void printBlocksMap(String heading, Map<Integer,Queue<Long>> blocks) {
        printHeading(heading);

        if (blocks.size() > 0) {
            for (Map.Entry<Integer, Queue<Long>> entry: blocks.entrySet()) {
                System.out.println("Region Size: " + entry.getKey() + " bytes");
                Queue<Long> freeAddresses = entry.getValue();
                if (freeAddresses.size() > 0) {
                    for (Long address: freeAddresses) {
                        System.out.println("  " + address);
                    }
                } else {
                    System.out.println("  None");
                }
            }
        } else {
            System.out.println("None");
        }
    }

    public static void dumpMemoryManager() {
        System.out.println("***Eta-Managed Off-Heap Memory***");
        printAddressMap("Allocated Direct Blocks", allocatedDirectBlocks);
        printAddressMap("Allocated Heap Blocks", allocatedHeapBlocks);
        printAddressMap("Free Direct Addresses", freeDirectAddresses);
        printAddressMap("Free Heap Addresses", freeHeapAddresses);
        printBlocksMap("Free Direct Blocks", freeDirectBlocks);
        printBlocksMap("Free Heap Blocks", freeHeapBlocks);
    }

    /** Misc. Utilities **/
    private static String renderSize(int size) {
        return "["+ size + " bytes]";
    }

    private static String renderIsDirect(boolean direct) {
        return direct? "Direct" : "Heap";
    }
}
