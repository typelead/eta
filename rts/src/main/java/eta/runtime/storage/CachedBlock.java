package eta.runtime.storage;

public class CachedBlock {
    /* The cached block */
    private Block block1;
    /* Start of previous received block. */
    private long lowerAddress1;
    /* Start of the adjacent block. */
    private long higherAddress1;

    /* The cached block */
    private Block block2;

    /* Start of previous received block. */
    private long lowerAddress2;
    /* Start of the adjacent block. */
    private long higherAddress2;

    /* We have two cached buffers because it was observed that in practice, there are
       cases where you read from one address, do some transformation, and write to another
       address in a different block. In such cases, the cache would get invalidated constantly
       making it unnecessary overhead. */

    /* Controls which cached block to overwrite next. */
    private boolean nextInvalidation;

    private CachedBlock() {}

    /* Returns null if address is not within range. */
    private Block get(long address) {
        /* TODO: Make this more efficient using the knowledge that the difference is constant
                 (blockSize) and that blockSize will be a power of 2. */
        if (address >= lowerAddress1 && address < higherAddress1) {
            return block1;
        } else if (address >= lowerAddress2 && address < higherAddress2) {
            return block2;
        } else {
            return null;
        }
    }

    void set(Block block, long lowerAddress, long higherAddress) {
        if (nextInvalidation) {
            this.block2 = block;
            this.lowerAddress2 = lowerAddress;
            this.higherAddress2 = higherAddress;
        } else {
            this.block1 = block;
            this.lowerAddress1 = lowerAddress;
            this.higherAddress1 = higherAddress;
        }
        nextInvalidation = !nextInvalidation;
    }

    /* Returns null if cache miss. */
    public static Block getBlock(long address) {
        return cachedBlock.get().get(address);
    }

    public static void setBlock(long address, Block block) {
        block.fillCache(address, cachedBlock.get());
    }

    private static final ThreadLocal<CachedBlock> cachedBlock
        = new ThreadLocal<CachedBlock>() {
                public CachedBlock initialValue() {
                    return new CachedBlock();
                }
            };
}
