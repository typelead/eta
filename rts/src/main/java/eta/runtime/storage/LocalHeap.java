package eta.runtime.storage;

public interface LocalHeap {
    long allocateLocal(int miniblocks, boolean direct, boolean supr);
    void setActiveBlock(Block block, boolean direct, boolean supr);
}
