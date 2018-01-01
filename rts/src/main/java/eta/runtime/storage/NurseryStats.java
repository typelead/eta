package eta.runtime.storage;

import java.util.List;

public class NurseryStats {
    long startAddress;
    List<BlockStats> blockStats;

    public NurseryStats(long startAddress, List<BlockStats> blockStats) {
        this.startAddress = startAddress;
        this.blockStats   = blockStats;
    }

    public long getTotalBytesAllocated() {
        /* TODO: Implement */
        return 0;
    }

    public long getTotalBytesFree() {
        /* TODO: Implement */
        return 0;
    }

    public long getStartAddress() {
        return startAddress;
    }

    public List<BlockStats> getBlockStats() {
        return blockStats;
    }
}
