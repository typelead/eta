package eta.runtime.storage;

import java.util.ArrayList;
import java.nio.ByteBuffer;

public class ManagedHeap {
    /* All of the sizes *must* be a power of 2. */
    private final int nurserySize;
    private final int blockSize;
    private final int miniBlockSize;

    /* Caching important constants */
    private final int nurseryMask;
    private final int nurseryBits;
    private final int blockMask;
    private final int blockBits;
    private final int miniBlockMask;
    private final int miniBlockBits;

    /* Nursery storage */
    private volatile Nursery[] nurseries;
    private volatile Nursery activeNursery;

    private final Object heapLock = new Object();

    public ManagedHeap(int nurserySize, int blockSize, int miniBlockSize) {
        this.nurserySize   = nurserySize;
        this.blockSize     = blockSize;
        this.miniBlockSize = miniBlockSize;

        this.nurseryMask   = nurserySize - 1;
        this.nurseryBits   = Integer.numberOfTrailingZeros(nurserySize);
        this.blockMask     = blockSize - 1;
        this.blockBits     = Integer.numberOfTrailingZeros(blockSize);
        this.miniBlockMask = miniBlockSize - 1;
        this.miniBlockBits = Integer.numberOfTrailingZeros(miniBlockSize);

        final Nursery nursery = allocateNursery(blockSize);
        nurseries = new Nursery[] { nursery };
        setActiveNursery(nursery);
    }

    public long allocateBuffer(int n, boolean direct, LocalHeap localHeap) {
        int miniblocks = (n + miniBlockMask) >>> miniBlockBits;
        int blocks     = Math.max(1, (n + blockMask) >>> blockBits);
        boolean supr   = blocks > 1;
        /* First, try allocating from the thread-local blocks. */
        long address = localHeap.allocateLocal(miniblocks, direct, supr);
        if (address == 0) {
            /* Second, try allocating a new block from the OS. */
            Block block = allocateBlock(blocks, direct);
            block.allocate(miniblocks);
            address = block.getAddress();
            localHeap.setActiveBlock(block, direct, supr);
        }
        return address;
    }

    public Block allocateBlock(int blocks, boolean direct) {
        /* TODO: Implement the free block pool */
        return activeNursery
            .allocateBlocks(blocks,
                            allocateAnonymousBuffer(blocks * blockSize, direct),
                            this);
    }

    public Nursery allocateNursery(long startAddress) {
        return new Nursery(nurserySize, blockSize, miniBlockSize, startAddress);
    }

    public Block allocateNurseries(final int baseIndex, final ByteBuffer buffer,
                                   final int rblocks, final int nblocks,
                                   Nursery nursery, long startAddress) {
        synchronized (heapLock) {
            final Nursery startNursery = nursery;
            int nurseries = (rblocks + nurseryMask) >>> nurseryBits;
            while (nurseries > 0) {
                final Nursery newNursery = allocateNursery(startAddress);
                nursery.next = newNursery;
                nursery = newNursery;
                startAddress += nurserySize * blockSize;
                nurseries--;
                addNursery(newNursery);
            }
            Block ret = startNursery.initializeWithCommonBlock(baseIndex, nblocks, buffer);
            setActiveNursery(nursery);
            heapLock.notifyAll();
            return ret;
        }
    }

    public Nursery waitForNursery(final Nursery nursery) {
        Nursery ret = null;
        synchronized (heapLock) {
            while ((ret = nursery.getFreeNext()) == null) {
                try {
                    heapLock.wait();
                } catch (InterruptedException e) {
                    // Keep going!
                }
            }
        }
        return ret;
    }

    public void addNursery(final Nursery nursery) {
        final int numNurseries = this.nurseries.length;
        final Nursery[] newNurseries = new Nursery[numNurseries + 1];
        System.arraycopy(this.nurseries, 0, newNurseries, 0, numNurseries);
        newNurseries[numNurseries] = nursery;
        this.nurseries = newNurseries;
    }

    public void setActiveNursery(final Nursery nursery) {
        this.activeNursery = nursery;
    }

    public Block getBlock(final long address) {
        if (address < blockSize) {
            throwIllegalAddressException
                (address, "Exceeded lower bound of the address space: "
                 + showAddress(blockSize));
        }
        /* We subtract `blockSize` since all addresses start from there. */
        final long normalizedAddress = address - blockSize;
        final int nurseryIndex = (int) (normalizedAddress >>> (nurseryBits + blockBits));
        final int blockIndex   = (int)((normalizedAddress >>> blockBits) & nurseryMask);
        final int numNurseries = this.nurseries.length;
        if (nurseryIndex >= numNurseries) {
            final long upperBound = numNurseries * nurserySize * blockSize;
            throwIllegalAddressException
                (address, "Exceeded upper bound of the address space: " +
                 showAddress(upperBound));
        }
        return nurseries[nurseryIndex].getBlock(blockIndex);
    }

    public void attemptFree(long address) {
        getBlock(address).sendFreeMessage(address);
    }

    public void free(long address) {
        getBlock(address).free(address);
    }

    private static void throwIllegalAddressException(long address, String message) {
        throw new IllegalArgumentException
            ("Cannot dereference " + showAddress(address) + ": " + message);
    }

    private static String showAddress(long address) {
        return "0x" + Long.toHexString(address);
    }

    private static ByteBuffer allocateAnonymousBuffer(int n, boolean direct) {
        return (direct?
                /* Off-Heap Memory */
                ByteBuffer.allocateDirect(n):
                /* Heap Memory */
                ByteBuffer.allocate(n));
    }

    /* Monitoring */
    public HeapStats getStatistics() {
        ArrayList<NurseryStats> nurseryStats = new ArrayList(this.nurseries.length);
        final Nursery[] nurseries = this.nurseries;
        for (Nursery nursery : nurseries) {
            nurseryStats.add(nursery.getStatistics());
        }
        return new HeapStats(nurserySize, blockSize, miniBlockSize, nurseryStats);
    }
}
