package eta.runtime.storage;

public class CachedBlock {
    /* The cached block */
    private Block block;
    /* Start of previous received block. */
    private long lowerAddress;
    /* Start of the adjacent block. */
    private long higherAddress;

    private CachedBlock() {}

    /* Returns null if address is not within range. */
    private Block get(long address) {
        if (address >= lowerAddress && address < higherAddress) {
            return block;
        }
        return null;
    }

    void set(Block block, long lowerAddress, long higherAddress) {
        this.block = block;
        this.lowerAddress = lowerAddress;
        this.higherAddress = higherAddress;
    }

    /* Returns null if cache miss. */
    public static Block getBlock(long address) {
        CachedBlock cb = cachedBlock.get();
        if (cb == null) return null;
        return cb.get(address);
    }

    public static void setBlock(Block block) {
        CachedBlock cb = cachedBlock.get();
        if (cb == null) {
            cb = new CachedBlock();
            cachedBlock.set(cb);
        }
        block.fillCache(cb);
    }

    private static final ThreadLocal<CachedBlock> cachedBlock
        = new ThreadLocal<CachedBlock>();
}
