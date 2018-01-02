package eta.runtime.storage;

import java.util.List;

import static eta.runtime.util.Report.*;

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

    public void generateReport(StringBuilder sb) {
        format(sb, "Start Address: %d", startAddress);
        blankLine(sb);
        format(sb, "Blocks:");
        blankLine(sb);
        int i = 0;
        for (BlockStats stats: blockStats) {
            format(sb, "Block %d:", i);
            blankLine(sb);
            stats.generateReport(sb);
            blankLine(sb);
            i++;
        }
    }
}
