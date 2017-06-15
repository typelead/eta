package eta.runtime.io;

import java.util.TreeMap;
import java.util.Map;
import java.nio.ByteBuffer;
import java.lang.ref.WeakReference;

import static eta.runtime.RuntimeLogging.barf;

public class MemoryManager {
    /* TODO: Optimize this to make a specialized data structure that stores
             primitive ints & longs. */
    /* Map block sizes to addresses of free blocks */
    public static final NavigableMap<Integer, Queue<Long>> freeDirectBlocks
        = new TreeMap<Integer, Queue<Long>>();
    public static final NavigableMap<Integer, Queue<Long>> freeHeapBlocks
        = new TreeMap<Integer, Queue<Long>>();

    /* Map addresses to size of free blocks */
    public static final NavigableMap<Long, Integer> freeDirectAddresses
        = new TreeMap<Long, Integer>();
    public static final AtomicBoolean freeDirectLock = new AtomicBoolean();
    public static final NavigableMap<Long, Integer> freeHeapAddresses
        = new TreeMap<Long, Integer>();
    public static final AtomicBoolean freeHeapLock = new AtomicBoolean();

    /* Map addresses to size of allocated blocks */
    public static final NavigableMap<Long, Integer> allocatedDirectBlocks
        = new TreeMap<Long, Integer>();
    public static final AtomicBoolean allocatedDirectLock = new AtomicBoolean();
    public static final NavigableMap<Long, Integer> allocatedHeapBlocks
        = new TreeMap<Long, Integer>();
    public static final AtomicBoolean allocatedHeapLock = new AtomicBoolean();

    public static List<ByteBuffer>[] blockArrays =
        { new ArrayList<ByteBuffer>()
        , new ArrayList<ByteBuffer>()
        , new ArrayList<ByteBuffer>()
        , new ArrayList<ByteBuffer>() };
    public static AtomicBoolean[]    blockLocks  =
        { new AtomicBoolean()
        , new AtomicBoolean()
        , new AtomicBoolean()
        , new AtomicBoolean() };
    public static Map<Long, AtomicBoolean>[] subBlockLocks =
        { new HashMap<Long, AtomicBoolean>()
        , new HashMap<Long, AtomicBoolean>()
        , new HashMap<Long, AtomicBoolean>()
        , new HashMap<Long, AtomicBoolean>() };

    /** Cached Buffer Lookup **/
    /* Start of previous received block. */
    public static ThreadLocal<Long> cachedLowerAddress = new ThreadLocal<Long>();
    /* Start of the adjacent block. */
    /* NOTE: This is the start of the NEXT block so you must do a strict comparison. */
    public static ThreadLocal<Long> cachedHigherAddress = new ThreadLocal<Long>();
    /* The cached buffer. */
    public static ThreadLocal<ByteBuffer> cachedBuffer = new ThreadLocal<ByteBuffer>();

    /* Key Locks */
    public static Map<Integer, AtomicBoolean> directKeyLocks
        = new HashMap<Integer, AtomicBoolean>();

    public static Map<Integer, AtomicBoolean> heapKeyLocks
        = new HashMap<Integer, AtomicBoolean>();

    public static AtomicBoolean getFreeKeyLock(Map<Integer, AtomicBoolean> keyLocks,
                                               Integer key) {
        AtomicBoolean keyLock = keyLocks.get(key);
        if (keyLock == null) {
            keyLock = new AtomicBoolean();
            keyLocks.put(key, keyLock);
        }
        return keyLock;
    }

    public static AtomicBoolean getSubBlockLock(long address) {
        int blockMask  = blockMask(address);
        int blockIndex = blockIndex(address, indexBits(blockMask));
        Map<Long, AtomicBoolean> blockLocks = subBlockLocks[blockMask];
        AtomicBoolean blockLock = blockLocks.get(blockIndex);
        if (blockLock == null) {
            blockLock = new AtomicBoolean();
            blockLocks.put(block, blockLock);
        }
        return blockLock;
    }

    /* Buffer Allocation
       This logic is rather complicated but it is done for efficiency purposes. Each
       free block size has a lock associated with it which is taken whenever a
       modification is done for that size. That way, two concurrent threads that
       are trying to allocate two different sizes can do so without waiting.
     */
    public static long allocateBuffer(int n, boolean direct) {
        NavigableMap<Integer, Queue<Long>> freeBlocks;
        NavigableMap<Long, Integer> freeAddresses;
        NavigableMap<Long, Integer> allocatedBlocks;
        Map<Integer, AtomicBoolean> keyLocks;
        if (direct) {
            allocatedBlocks = allocatedDirectBlocks;
            freeAddresses   = freeDirectAddresses;
            freeBlocks      = freeDirectBlocks;
            keyLocks        = directKeyLocks;
        } else {
            allocatedBlocks = allocatedHeapBlocks;
            freeAddresses   = freeHeapAddresses;
            freeBlocks      = freeHeapBlocks;
            keyLocks        = heapKeyLocks;
        }
        do {
            Map.Entry<Integer, Queue<Long>> freeEntry = freeBlocks.ceilingEntry(n);
            if (freeEntry != null) {
                int           regionSize = freeEntry.getKey();
                AtomicBoolean keyLock    = getFreeKeyLock(keyLocks, regionSize);
                Queue<Long>   freeQueue  = freeEntry.getValue();
                Long          address;
                /* To reduce contention, we attempt to lock and upon failure, proceed
                   to look for the next higher size. In the worst case, we allocate
                   a new block which isn't so bad.

                   While we could allow multiple threads to contend if there are
                   multiple elements in the queue, the logic becomes non-trivial. */
                if (keyLock.compareAndSet(false, true)) {
                    try {
                        address = freeQueue.poll();
                        /* Check if the queue was emptied while we were
                           waiting on the lock. */
                        if (address == null) continue;
                        else if (freeQueue.isEmpty()) {
                            /* While we could just leave the queues empty, over time
                               the map will get too many keys with "dead" entries, so
                               we clean it up. */
                            freeQueue.remove(regionSize);
                        }
                    } finally {
                        /* TODO: Does this finally work with that 'continue'? */
                        keyLock.set(false);
                    }
                } else {
                    /* If unable to lock, continue the loop to see if free blocks
                       became available in the mean time. */
                    continue;
                }
                int  newRegionSize = regionSize - n;
                long newAddress    = address + newRegionSize;
                insertAllocatedBlock(allocatedBlocks, direct, address, n);
                insertFreeBlock(freeBlocks, freeAddresses, keyLocks,
                                newRegionSize, newAddress);
                return address;
            } else {
                int              blockType = getBlockType(n);
                AtomicBoolean    blockLock = blockLocks[blockType];
                List<ByteBuffer> blocks    = blockArrays[blockType];
                if (blockLock.compareAndSet(false, true)) {
                    long blockIndex   = blocks.size();
                    int  blockSize    = getBlockSize(blockType);
                    long address      = (blockType  << 62)
                                      | (blockIndex << indexBits(blockType));
                    blocks.add(allocateAnonymousBuffer(blockSize, direct));
                    insertFreeBlock(freeBlocks, freeAddresses, keyLocks,
                                    startAddress + n, blockSize - n);
                    blockLock.set(false);
                    return address;
                } else {
                    /* If unable to lock, continue the loop to see if free blocks
                       became available in the mean time. */
                    continue;
                }
            }
            break;
        } while (true);
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
         Map<Integer, AtomicBoolean> keyLocks,
         int newRegionSize, long newAddress) {
        AtomicBoolean newKeyLock = getFreeKeyLock(keyLocks, newRegionSize);
        while (!newKeyLock.compareAndSet(false, true)) {}
        Queue<Long>   freeQueue  = freeBlocks.get(newRegionSize);
        if (freeQueue != null) {
            freeQueue.offer(newAddress);
        } else {
            freeQueue = new LinkedList<Long>();
            freeQueue.offer(newAddress);
            freeBlocks.put(newAddress, freeQueue);
        }
        newKeyLock.set(false);
        AtomicBoolean blockLock     = getSubBlockLock(newAddress);
        while (!blockLock.compareAndSet(false, true)) {}
        AtomicBoolean lock          = direct? freeDirectLock: freeHeapLock;
        while (!lock.compareAndSet(false, true)) {}
        Queue<Long>   freeAddresses = free.get(newAddress);
        freeAddresses.put(newAddress, newRegionSize);
        lock.set(false);
        blockLock.set(false);
    }

    public static void insertAllocatedBlock(NavigableMap<Long, Integer> allocatedBlocks,
                                            boolean direct, long address, int size) {

        AtomicBoolean lock = direct? allocatedDirectLock: allocatedHeapLock;
        while (!lock.compareAndSet(false, true)) {}
        /* We grab the block lock as well so that there are no changes to
           a given block while we are freeing/reorganizing that blocks memory. */
        AtomicBoolean blockLock = getSubBlockLock(address);
        while (!blockLock.compareAndSet(false, true)) {}
        allocatedBlocks.put(address, size);
        blockLock.set(false);
        lock.set(false);
    }

    public static long allocateAnonymousBuffer(int n, boolean direct) {
        return (direct?
                /* Off-Heap Memory */
                ByteBuffer.allocateDirect(n):
                /* Heap Memory */
                ByteBuffer.allocate(n));
    }

    /* TODO: Verify the locking and that it won't dead lock, especially the part
             where they are being removed. */
    public static void free(long address) {
        NavigableMap<Integer, Queue<Long>> freeBlocks;
        NavigableMap<Long, Integer> allocatedBlocks;
        NavigableMap<Long, Integer> freeAddresses;
        AtomicBoolean freeLock;
        Map<Integer, AtomicBoolean> keyLocks;
        AtomicBoolean freeLock;
        Integer sizeInt = allocatedDirectBlocks.get(address);
        int     size    = sizeInt.intValue();
        boolean direct  = true;
        if (size == null) {
            size = allocatedHeapBlocks.get(address);
            if (size == null) {
                /* This means that `address` was already freed. */
                return;
            }
            direct = false;
        }
        if (direct) {
            allocatedBlocks = allocatedDirectBlocks;
            freeAddresses   = freeDirectAddresses;
            freeBlocks      = freeDirectBlocks;
            freeLock        = freeDirectLock;
            keyLocks        = directKeyLocks;
        } else {
            allocatedBlocks = allocatedHeapBlocks;
            freeAddresses   = freeHeapAddresses;
            freeBlocks      = freeHeapBlocks;
            freeLock        = freeHeapLock;
            keyLocks        = heapKeyLocks;
        }
        AtomicBoolean blockLock = getSubBlockLock(address);
        while (!blockLock.compareAndSet(false, true)) {}
        while (!freeLock.compareAndSet(false, true)) {}
        Map.Entry<Long, Integer> lowerEntry  = freeAddresses.lowerEntry(address);
        Map.Entry<Long, Integer> higherEntry = freeAddresses.higherEntry(address);
        long lowerAddress  = lowerEntry.getKey();
        int  lowerSize     = lowerEntry.getValue();
        long higherAddress = higherEntry.getKey();
        int  higherSize    = higherEntry.getValue();
        AtomicBoolean lowerRegionLock;
        AtomicBoolean higherRegionLock;

        /* After these two checks, lowerAddress will be the starting
           point of the new freeBlock and size will be the size of
           the new block. */
        if ((lowerAddress + lowerSize) == address) {
            lowerRegionLock = getFreeKeyLock(keyLocks, lowerSize);
            while (!lowerRegionLock.compareAndSet(false, true)) {}
            size += lowerSize;
        } else {
            lowerAddress = address;
        }
        if ((address + size) == higherAddress) {
            higherRegionLock = getFreeKeyLock(keyLocks, higherSize);
            while (!higherRegionLock.compareAndSet(false, true)) {}
            size += higherSize;
        }
        Queue<Long> freeQueue;
        boolean revert = false;
        if (lowerRegionLock  != null) {
            freeQueue = freeBlocks.get(lowerSize);
            /* If the free block was taken by the time we got the lock. */
            if (freeQueue == null) {
                lowerAddress = address;
                size        -= lowerSize;
            } else {
                boolean removed = freeQueue.remove(lowerAddress);
                /* If the free block was taken by the time we got the lock. */
                if (!removed) {
                    lowerAddress = address;
                    size        -= lowerSize;
                }
            }
            lowerRegionLock.set(false);
        }
        if (higherRegionLock != null) {
            freeQueue = freeBlocks.get(higherSize);
            /* If the free block was taken by the time we got the lock. */
            if (freeQuee == null) {
                size -= higherSize;
            } else {
                boolean removed = freeQueue.remove(lowerAddress);
                /* If the free block was taken by the time we got the lock. */
                if (!removed) {
                    size -= higherSize;
                }
            }
            higherRegionLock.set(false);
        }
        insertFreeBlock(freeBlocks, freeAddresses, keyLocks, size, lowerAddress);
        freeLock.set(false); /* TODO: This is a fine-grained lock, should we reduce
                                  the scope for efficiency? */
        blockLock.set(false);
    }

    /** Addresses
        The blocks come in four variants:
        *   1 MB block - 42 block bits, 20 index bits
        *  16 MB block - 38 block bits, 24 index bits
        * 128 MB block - 35 block bits, 27 index bits
        *   1 GB block - 32 block bits, 30 index bits
        The first two bits of the address determine the block type.
        Hence, the maximum you can allocate a single block for is 1GB.
        We may want to implement a custom ByteBuffer that can handle block borders. */
    public static final long BLOCK_TYPE_MASK = 0xC000000000000000L;
    public static final int ONE_MB           = 1 << 20;
    public static final int ONE_SIX_MB       = 1 << 24;
    public static final int ONE_TWO_EIGHT_MB = 1 << 27;
    public static final int ONE_GB           = 1 << 30;

    public static int getBlockType(int n) {
        if      (n <= ONE_MB)           return 0;
        else if (n <= ONE_SIX_MB)       return 1;
        else if (n <= ONE_TWO_EIGHT_MB) return 2;
        else if (n <= ONE_GB)           return 3;
        else barf("Attempting to allocate a buffer larger than 1GB.");
    }

    public static int getBlockSize(int blockMask) {
        switch (blockMask) {
        case 0: return ONE_MD;
        case 1: return ONE_SIX_MB;
        case 2: return ONE_TWO_EIGHT_MB;
        case 3: return ONE_GB;
        default: barf("Bad index Mask"); return -1;
        }
    }

    public static int blockMask(long address) {
        return (int)((address & BLOCK_TYPE_MASK) >>> 62);
    }

    public static int indexBits(int blockMask) {
        switch (blockMask) {
        case 0: return 20;
        case 1: return 24;
        case 2: return 27;
        case 3: return 30;
        default: barf("Bad index Mask"); return -1;
        }
    }

    public static int blockIndex(long address, int indexBits) {
        return (int)((address & BLOCK_TYPE_MASK) >>> indexBits);
    }

    public static int positionIndex(long address) {
        return (int)(address & ((1 << indexBits(blockMask(address))) - 1));
    }

    public static int positionIndex(long address, int indexBits) {
        return (int)(address & ((1 << indexBits) - 1));
    }

    /** Byte Buffer Retrieval

        This caches the last lookup for reducing the constant factor when
        you're doing a lot of writes/reads on a single buffer (the common case). */
    public static ByteBuffer getBuffer(long address) {
        if (address >= cachedLowerAddress.get() && address < cachedHigherAddress.get()) {
            return cachedBuffer.get();
        } else {
            int blockMask  = blockMask(address);
            int indexBits  = indexBits(blockMask);
            int blockIndex = blockIndex(indexBits);
            long lower = (blockMask << 62) | (blockIndex << indexBits);
            ByteBuffer buf = blockArrays[blockMask].get(blockIndex);
            cachedLowerAddress.set(lower);
            cachedHigherAddress.set(lower + (1 << indexBits));
            cachedBuffer.set(buf);
            return buf;
        }
    }

    /* When doing bulk operations, this can be useful. It returns a ByteBuffer
       positioned at the place referred to by the address. It's duplicated so the
       user is free to change the position as necessary. */
    public static ByteBuffer getBoundedBuffer(long address) {
        int blockMask  = blockMask(address);
        int indexBits  = indexBits(blockMask);
        int blockIndex = blockIndex(indexBits);
        ByteBuffer buf = blockArrays[blockMask].get(blockIndex).duplicate();
        buf.position(positionIndex(address, indexBits));
        return buf;
    }


    public static void put(long address, ByteBuffer buf, byte value) {
        buf.put(positionIndex(address), value);
    }

    /* TODO: Write a function that returns the level of fragmentation for
             debugging/testing purposes. */
}
