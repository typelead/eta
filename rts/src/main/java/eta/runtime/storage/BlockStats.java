package eta.runtime.storage;

import java.util.List;

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
}
