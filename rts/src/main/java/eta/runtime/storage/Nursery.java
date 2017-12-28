package eta.runtime.storage;

import java.nio.ByteBuffer;
import java.util.concurrent.atomic.AtomicInteger;

public class Nursery {
    private final Block[] blocks;
    private final int numBlocks;
    private final int blockSize;
    private final long startAddress;
    private final AtomicInteger nextIndex;
    public volatile Nursery next;

    public Nursery(int size, int blockSize, long startAddress) {
        this.blocks = new Block[size];
        this.numBlocks = size;
        for (int i = 0; i < size; i++) {
            blocks[i] = new Block();
        }
        this.blockSize = blockSize;
        this.startAddress = startAddress;
        this.nextIndex  = new AtomicInteger();
    }

    public Block allocateBlocks(int n, ByteBuffer buffer) {
        int baseIndex = nextIndex.getAndAdd(n);
        if (baseIndex < blockSize) {
            if (baseIndex + n >= numBlocks) {
                /* This is the boundary case, and this thread is responsible for
                   allocating the next Nursery while the other threads wait
                   on this. */
                /* TODO: Figure out a way to address potential (minor) loss of
                   the address space at the boundary. */
                next = new Nursery(numBlocks, blockSize,
                                   startAddress + numBlocks * blockSize);
                return next.allocateBlocks(n, buffer);
            } else {
                /* Common case */
                long address = startAddress + baseIndex * blockSize;
                int bufferOffset = 0;
                for (int i = 0; i < n;
                     i++, address += blockSize, bufferOffset += blockSize) {
                    blocks[baseIndex + i]
                        .init(address, blockSize, buffer, bufferOffset);
                }
                return blocks[baseIndex];
            }
        } else {
            /* baseIndex >= numBlocks
               Must wait on the next Nursery to get allocated. */
            while (next == null) { Thread.yield(); }
            return next.allocateBlocks(n, buffer);
        }
    }
}
