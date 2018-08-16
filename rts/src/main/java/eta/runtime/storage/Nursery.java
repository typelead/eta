package eta.runtime.storage;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;

public class Nursery {

    private final long startAddress;
    private final Block[] blocks;

    /* Cached constants */
    private final int numBlocks;
    private final int blockSize;
    private final int miniBlockSize;

    private final AtomicInteger nextIndex;
    volatile Nursery next;

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

    public boolean isFree() {
        return nextIndex.get() < numBlocks;
    }

    public Nursery getFreeNext() {
        Nursery nursery = next;
        while (nursery != null && !nursery.isFree()) {
            nursery = nursery.next;
        }
        return nursery;
    }

    public Block allocateBlocks(final int n, final ByteBuffer buffer, final ManagedHeap heap) {
        final int baseIndex = nextIndex.getAndAdd(n);
        if (baseIndex + n >= numBlocks) {
            final int m = numBlocks - baseIndex;
            if (m > 0) {
                /* This is the thread that first crossed the threshold. */
                /* The +1 is to handle the case where `baseIndex + n == numBlocks`. */
                return heap.allocateNurseries(baseIndex, buffer, n - m + 1, n, this,
                                              startAddress + blockSize * numBlocks);
            } else {
                /* Competing threads, must wait for the above if next is not populated. */
                return heap.waitForNursery(this).allocateBlocks(n, buffer, heap);
            }
        } else {
            return initializeWithCommonBlock(baseIndex, n, buffer);
        }
    }

    public Block initializeWithCommonBlock(final int baseIndex, final int n,
                                           final ByteBuffer buffer) {
        final Block startBlock = blocks[baseIndex];
        final long address = startAddress + baseIndex * blockSize;
        startBlock.init(address, miniBlockSize, buffer);
        initializeBlocks(baseIndex, startBlock, n);
        return startBlock;
    }

    /* ASSUMES that numBlocks will be the same for all subsequent nurseries! */
    public void initializeBlocks(final int baseIndex, final Block startBlock, int n) {
        /* Fill in the rest of the blocks in the current nursery. */
        final int remaining = numBlocks - baseIndex;
        final int m = Math.min(remaining, n);
        initializeWithBlock(baseIndex, m, startBlock, false);
        n -= m;
        if (n > 0) {
            Nursery nursery = next;
            /* Fill in the remaining nurseries. */
            while (n > 0) {
                final int blocks = Math.min(n, numBlocks);
                nursery.initializeWithBlock(0, blocks, startBlock, true);
                n -= numBlocks;
                nursery = nursery.next;
            }
        }
    }

    public void initializeWithBlock(final int baseIndex, final int nblocks,
                                    final Block startBlock, final boolean updateIndex) {
        int i = 0;
        for (; i < nblocks; i++) {
            blocks[baseIndex + i].initWith(startBlock);
        }
        if (updateIndex) {
            nextIndex.set(baseIndex + i);
        }
    }

    public Block getBlock(int blockIndex) {
        return blocks[blockIndex];
    }

    /* Monitoring */
    public NurseryStats getStatistics() {
        ArrayList<BlockStats> blockStats = new ArrayList<BlockStats>(numBlocks);
        long lastAddress = 0;
        for (Block block: blocks) {
            if (block.isActive()) {
                long address = block.getAddress();
                if (address == lastAddress) continue;
                blockStats.add(block.getStatistics());
                lastAddress = address;
            } else {
                break;
            }
        }
        return new NurseryStats(startAddress, blockStats);
    }
}
