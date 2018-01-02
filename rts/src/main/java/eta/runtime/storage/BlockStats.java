package eta.runtime.storage;

import java.util.List;

import static eta.runtime.util.Report.*;

public class BlockStats {
    long startAddress;
    int capability;
    boolean direct;
    List<Span> spans;

    public BlockStats(long startAddress, int capability, boolean direct, List<Span> spans) {
        this.startAddress = startAddress;
        this.capability   = capability;
        this.direct       = direct;
        this.spans        = spans;
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

    public boolean isDirect() {
        return direct;
    }

    public List<Span> getSpans() {
        return spans;
    }

    public void generateReport(StringBuilder sb) {
        format(sb, "Start Address: %d", startAddress);
        format(sb, "Owner Capability: %d", capability);
        format(sb, "Direct: %b", direct);
        blankLine(sb);
        format(sb, "Spans:");
        blankLine(sb);
        int i = 0;
        for (Span span: spans) {
            format(sb, "Span %d:", i);
            blankLine(sb);
            span.generateReport(sb);
            blankLine(sb);
            i++;
        }
    }
}
