package eta.runtime.storage;

import java.util.Iterator;
import java.util.ArrayList;
import java.nio.ByteBuffer;
import java.util.concurrent.CopyOnWriteArrayList;

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
    private final CopyOnWriteArrayList<Nursery> nurseries
        = new CopyOnWriteArrayList<Nursery>();
    private Nursery activeNursery;

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

        setActiveNursery(allocateNursery(blockSize));
    }

    public long allocateBuffer(int n, boolean direct, LocalHeap localHeap) {
        int miniblocks = (n + miniBlockMask) >>> miniBlockBits;
        int blocks     = Math.max(1, (n + blockMask) >>> blockBits);
        boolean supr   = blocks > 1;
        /* First, try allocating from the thread-local blocks. */
        long address = localHeap.allocateLocal(miniblocks, direct, supr);
        if (address == 0) {
            /* Second, try allocating a new block form the OS. */
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

    public void setActiveNursery(Nursery nursery) {
        this.activeNursery = nursery;
        this.nurseries.add(nursery);
    }

    public Block getBlock(long address) {
        if (address < blockSize)
            throwIllegalAddressException(address,
                                         "Exceeded lower bound of the address space.");
        /* We subtract `blockSize` since all addresses start from there. */
        long normalizedAddress = address - blockSize;
        int nurseryIndex = (int) (normalizedAddress >>> (nurseryBits + blockBits));
        int blockIndex   = (int)((normalizedAddress >>> blockBits) & nurseryMask);
        int size = nurseries.size();
        Nursery nursery = null;
        if (nurseryIndex == size)
            nursery = activeNursery.getNext();
        if (nursery == null && nurseryIndex >= size) {
            throwIllegalAddressException(address,
                                         "Exceeded higher bound of the address space.");
        }
        return nurseries.get(nurseryIndex).getBlock(blockIndex);
    }

    public void attemptFree(long address) {
        getBlock(address).sendFreeMessage(address);
    }

    public void free(long address) {
        getBlock(address).free(address);
    }

    private static void throwIllegalAddressException(long address, String message) {
        throw new IllegalArgumentException("Cannot dereference 0x" + Long.toHexString(address) + ": " + message);
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
        Iterator<Nursery> it = nurseries.iterator();
        ArrayList<NurseryStats> nurseryStats = new ArrayList(nurseries.size());
        while (it.hasNext()) {
            nurseryStats.add(it.next().getStatistics());
        }
        return new HeapStats(nurserySize, blockSize, miniBlockSize, nurseryStats);
    }
}
