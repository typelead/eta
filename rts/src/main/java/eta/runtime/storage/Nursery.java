package eta.runtime.storage;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

public class Nursery {

    private final long startAddress;
    private final Block[] blocks;

    /* Cached constants */
    private final int numBlocks;
    private final int blockSize;
    private final int miniBlockSize;

    private final AtomicInteger nextIndex;
    private volatile Nursery next;
    private static final AtomicReferenceFieldUpdater<Nursery, Nursery> nextUpdater
        = AtomicReferenceFieldUpdater.newUpdater(Nursery.class, Nursery.class, "next");

    public Nursery(int size, int blockSize, int miniBlockSize, long startAddress) {
        this.blocks        = new Block[size];
        this.numBlocks     = size;
        this.blockSize     = blockSize;
        this.miniBlockSize = miniBlockSize;
        this.startAddress  = startAddress;
        this.nextIndex     = new AtomicInteger();

        for (int i = 0; i < size; i++) {
            blocks[i] = new Block();
        }
    }

    public Nursery getNext() {
        return next;
    }

    public Block allocateBlocks(int n, ByteBuffer buffer, ManagedHeap heap) {
        int baseIndex = nextIndex.getAndAdd(n);
        if (baseIndex + n > numBlocks)
            return allocateBlocksFromNext(n, buffer, heap);
        long address = startAddress + baseIndex * blockSize;
        Block startBlock = blocks[baseIndex];
        startBlock.init(address, blockSize, miniBlockSize, buffer);
        for (int i = 1; i < n; i++) {
            blocks[baseIndex + i] = startBlock;
        }
        return blocks[baseIndex];
    }

    public Block getBlock(int blockIndex) {
        return blocks[blockIndex];
    }

    private Block allocateBlocksFromNext(int n, ByteBuffer buffer, ManagedHeap heap) {
        Nursery curNext = next;
        if (curNext == null) {
            Nursery nursery =
                heap.allocateNursery(startAddress + numBlocks * blockSize);
            if (nextUpdater.compareAndSet(this, null, nursery)) {
                heap.setActiveNursery(nursery);
                return nursery.allocateBlocks(n, buffer, heap);
            } else {
                curNext = next;
            }
        }
        return curNext.allocateBlocks(n, buffer, heap);
    }

    /* Monitoring */
    public NurseryStats getStatistics() {
        ArrayList<BlockStats> blockStats = new ArrayList<BlockStats>(numBlocks);
        for (Block block: blocks) {
            if (block.isActive()) {
                blockStats.add(block.getStatistics());
            } else {
                break;
            }
        }
        return new NurseryStats(startAddress, blockStats);
    }
}
