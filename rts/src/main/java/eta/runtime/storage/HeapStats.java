package eta.runtime.storage;

import java.util.List;

import static eta.runtime.util.Report.*;

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

    public String generateReport() {
        StringBuilder sb = new StringBuilder();
        generateReport(sb);
        return sb.toString();
    }

    public void generateReport(StringBuilder sb) {
        header(sb, "Eta Memory Manager");
        blankLine(sb);
        header(sb, "Native Heap Statistics");
        blankLine(sb);
        format(sb, "  Nursery Size: %d bytes", nurserySize);
        format(sb, "    Block Size: %d bytes", blockSize);
        format(sb, "MiniBlock Size: %d bytes", miniBlockSize);
        format(sb, "Total Nurseries: %d", nurseryStats.size());
        blankLine(sb);
        header(sb, "Nurseries");
        blankLine(sb);
        int i = 0;
        for (NurseryStats stats: nurseryStats) {
            format(sb, "Nursery %d:", i);
            blankLine(sb);
            stats.generateReport(sb);
            blankLine(sb);
            i++;
        }
    }

}
