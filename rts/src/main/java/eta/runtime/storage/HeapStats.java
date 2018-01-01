package eta.runtime.storage;

import java.util.List;

public class HeapStats {
    int nurserySize;
    int blockSize;
    int miniBlockSize;
    List<NurseryStats> nurseryStats;

    public HeapStats(int nurserySize, int blockSize, int miniBlockSize, List<NurseryStats> nurseryStats) {
        this.nurserySize   = nurserySize;
        this.blockSize     = blockSize;
        this.miniBlockSize = miniBlockSize;
        this.nurseryStats  = nurseryStats;
    }

    public long getTotalBytesAllocated() {
        /* TODO: Implement */
        return 0;
    }

    public long getTotalBytesFree() {
        /* TODO: Implement */
        return 0;
    }

    public int getNurserySize() {
        return nurserySize;
    }

    public int getBlockSize() {
        return blockSize;
    }

    public int getMiniBlockSize() {
        return miniBlockSize;
    }

    public List<NurseryStats> getNurseryStats() {
        return nurseryStats;
    }
}
