package eta.runtime.storage;

import java.nio.ByteBuffer;
import java.util.concurrent.CopyOnWriteArrayList;

public class ManagedHeap {
    /* Both of these sizes *must* be a power of 2. */
    private final int blockSize;
    private final int blockMask;
    private final int blockBits;

    private final CopyOnWriteArrayList<Nursery> nurseries = new CopyOnWriteArrayList<Nursery>();
    private Nursery currentNursery;
    private Block freeBlocks;

    public ManagedHeap(int nurserySize, int blockSize) {
        // this.firstNursery   = new Nursery(nurserySize, blockSize, blockSize);
        // this.currentNursery = this.firstNursery;
        this.blockSize = blockSize;
        this.blockMask = blockSize - 1;
        this.blockBits = Integer.numberOfTrailingZeros(blockSize);
    }

    public Block allocateBlock(int n, boolean direct) {
        int numberOfBlocks = (n >>> blockBits) + (((n & blockMask) == 0)? 0 : 1);
        int bufferSize     = numberOfBlocks * blockSize;
        ByteBuffer buffer  = allocateAnonymousBuffer(bufferSize, direct);
        return currentNursery.allocateBlocks(numberOfBlocks, buffer);
    }

    private static ByteBuffer allocateAnonymousBuffer(int n, boolean direct) {
        return (direct?
                /* Off-Heap Memory */
                ByteBuffer.allocateDirect(n):
                /* Heap Memory */
                ByteBuffer.allocate(n));
    }
}
