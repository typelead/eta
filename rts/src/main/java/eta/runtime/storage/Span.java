package eta.runtime.storage;

import java.util.Arrays;

import static eta.runtime.util.Report.*;

public class Span {
    final boolean allocated;
    final int size;
    byte[] bytes;

    public Span(boolean allocated, int size) {
        this.allocated = allocated;
        this.size      = size;
    }

    public boolean isAllocated() {
        return allocated;
    }

    public int getSize() {
        return size;
    }

    public byte[] getBytes() {
        return bytes;
    }

    @Override
    public String toString() {
        return "Span { allocated = " + allocated + ", size = " + size + ", bytes = "
             + Arrays.toString(bytes) + " }";
    }

    public void generateReport(StringBuilder sb) {
        String status = allocated? "allocated" : "free";
        String miniblocks = (size > 1)? "miniblocks" : "miniblock";
        format(sb, " %d %s %s", size, status, miniblocks);
        if (bytes != null) {
            format(sb, "%s", Arrays.toString(bytes));
        }
    }
}
