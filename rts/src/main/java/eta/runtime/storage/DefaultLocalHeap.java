package eta.runtime.storage;

public class DefaultLocalHeap implements LocalHeap {
    private static LocalHeap INSTANCE = new DefaultLocalHeap();

    public static LocalHeap getInstance() {
        return INSTANCE;
    }

    @Override
    public long allocateLocal(int _miniblocks, boolean _direct, boolean _supr) {
        return 0;
    }

    @Override
    public void setActiveBlock(Block block, boolean direct, boolean supr) {}
}
