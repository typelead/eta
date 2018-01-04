package eta.runtime.storage;

import java.util.List;
import java.util.LinkedList;

/* Consists of 2-bit elements which have the following representation:
   - 00 (0x0) - Non-terminal free bit
   - 01 (0x1) - Terminal free bit
   - 11 (0x3) - Non-terminal allocated bit
   - 10 (0x2) - Terminal allocated bit */
public class AllocationVector {

    private static final int HIGH_BIT_MASK = 0x80000000;
    private final byte[] vector;

    /* State's binary representation can be of two forms:
       - 0...: The corresponding block is only capable of straight-line allocation
               and the numerical value of this corresponds to the next available index.
       - 1...: The corresponding block has interspersed free regions and the value
               after masking the leading bit corresponds to the maximum length of
               contiguous free blocks. */
    private int state;

    public AllocationVector(int numElements) {
        /* TODO: Should we push these checks up? */
        if ((numElements % 4) != 0) {
            throw new IllegalArgumentException("AllocationVector must have a number of elements that is a multiple of 4.");
        } else if (numElements <= 0) {
            throw new IllegalArgumentException("AllocationVector must have a number of elements that is non-negative.");
        }
        vector = new byte[numElements >>> 2];
        vector[vector.length - 1] = 0x40;
        state = 0;
    }

    public final byte[] getBytes() {
        return vector;
    }

    public final boolean isEmpty() {
        return state == 0;
    }

    public final int size() {
        return vector.length << 2;
    }

    /* Fast, straight-line allocation */
    public final int allocate(int n) {
        int idx = state;
        if (idx + n > size())
            return -1;
        allocate(idx, n, false);
        state += n;
        return idx;
    }

    public final void allocate(int i, int n) {
        allocate(i, n, true);
    }

    public final void allocate(int i, int n, boolean recompute) {
        int off = i & 0x3;
        int idx = i >>> 2;
        while (n-- > 0) {
            vector[idx] |= 0x3 << (off << 1);
            if (n == 0) {
                vector[idx] &= ~(0x1 << (off << 1));
            } else if (off == 3) {
                off = 0;
                idx++;
            } else {
                off++;
            }
        }
        if (recompute) {
            recomputeState();
        }
    }

    public final int free(int i) {
        int n = 1;
        int off, idx;
        if (i > 0) {
            i--;
            off = i & 0x3;
            idx = i >>> 2;
            if (bits(vector[idx], off) == 0x1) {
                vector[idx] &= ~(0x3 << (off << 1));
            }
            i++;
        }
        off = i & 0x3;
        idx = i >>> 2;
        int val = vector[idx];
        while (bits(val, off) != 0x2) {
            val &= ~(0x3 << (off << 1));
            n++;
            if (off == 3) {
                off = 0;
                vector[idx] = (byte) val;
                idx++;
                val = vector[idx];
            } else {
                off++;
            }
        }
        int prevIdx = idx;
        int prevOff = off << 1;
        val &= ~(0x3 << prevOff);
        if (off == 3) {
            off = 0;
            idx++;
        } else {
            off++;
        }
        if (idx >= vector.length || bits(vector[idx], off) != 0x0) {
            val |= 0x1 << prevOff;
        }
        vector[prevIdx] = (byte) val;
        recomputeState();
        return n;
    }

    public final int allocatedSize(int index) {
        int len = vector.length;
        int off = 0;
        int idx = index;
        int val = vector[idx];

        int n = 0;
        boolean stop = false;
        while (!stop && idx < len) {
            switch (bits(val, off)) {
                // Non-terminal allocated bit
                case 3:
                    n++;
                    break;
                // Terminal allocated bit
                case 2:
                    n++;
                    return n;
                default:
                    break;
            }
            if (off == 3) {
                off = 0;
                idx++;
                val = vector[idx];
            } else {
                off++;
            }
        }
        throw new IllegalArgumentException("Cannot compute allocated size of illegal index " + index + ".");
    }

    public final int findFree(int blocks) {
        int len = vector.length;
        int off = 0;
        int idx = 0;
        int val = vector[idx];

        int n = 0;
        int startIdx = -1;
        while (idx < len) {
            switch (bits(val, off)) {
              // Non-terminal free bit
              case 0:
                  if (startIdx == -1) {
                      startIdx = idx * 4 + off;
                  }
                  n++;
                  break;
              // Terminal free bit
              case 1:
                  if (startIdx == -1) {
                      startIdx = idx * 4 + off;
                  }
                  n++;
                  if (n >= blocks) return startIdx;
                  n = 0;
                  startIdx = -1;
                  break;
              default:
                  break;
            }
            if (off == 3) {
                off = 0;
                idx++;
                val = vector[idx];
            } else {
                off++;
            }
        }
        if (n > 0) {
            throw new IllegalStateException("AllocationVector is missing a terminal free bit!");
        }
        return -1;
    }

    public final int findFreeAndAllocate(int blocks) {
        if ((state & HIGH_BIT_MASK) == 0) {
            return allocate(blocks);
        } else {
            int freeIdx = -1;
            if ((state & ~HIGH_BIT_MASK) >= blocks) {
                freeIdx = findFree(blocks);
                if (freeIdx != -1) {
                    allocate(freeIdx, blocks, true);
                }
            }
            return freeIdx;
        }
    }

    /* This function is responsible for making sure the state is kept up-to-date. */
    private final void recomputeState() {
        int len = vector.length;
        int numElements = len << 2;
        int off = 0;
        int idx = 0;
        int val = vector[idx];

        int allocated = 0;
        int freed = 0;
        int freeIdx = -1;
        int maxFreed = 0;
        int maxFreeIdx = -1;
        while (idx < len) {
            switch (bits(val, off)) {
                // Non-terminal free bit
                case 0:
                    if (freeIdx == -1) {
                        freeIdx = idx;
                    }
                    freed++;
                    break;
                // Terminal free bit
                case 1:
                    freed++;
                    if (freeIdx == -1) {
                        freeIdx = idx;
                    }
                    if (freed > maxFreed) {
                        maxFreed = freed;
                        maxFreeIdx = freeIdx;
                    }
                    freed = 0;
                    freeIdx = -1;
                    break;
                default:
                    allocated++;
                    break;
            }
            if (off == 3) {
                off = 0;
                idx++;
                if (idx < len) {
                    val = vector[idx];
                }
            } else {
                off++;
            }
        }

        if ((maxFreeIdx == -1 || maxFreeIdx + maxFreed == numElements)
            && maxFreed + allocated == numElements) {
            state = allocated;
        } else {
            state = HIGH_BIT_MASK | maxFreed;
        }
    }

    private static int bits(int val, int off) {
        off <<= 1;
        return (val & (0x3 << off)) >>> off;
    }

    /* Monitoring */
    public final List<Span> getSpans() {
        LinkedList<Span> spans = new LinkedList<Span>();
        int len = vector.length;
        int numElements = len << 2;
        int off = 0;
        int idx = 0;
        int val = vector[idx];

        int spanSize = 0;
        while (idx < len) {
            switch (bits(val, off)) {
                // Non-terminal free bit
                case 0:
                    spanSize++;
                    break;
                // Terminal free bit
                case 1:
                    spanSize++;
                    spans.add(new Span(false, spanSize));
                    spanSize = 0;
                    break;
                // Non-terminal allocated bit
                case 3:
                    spanSize++;
                    break;
                // Terminal allocated bit
                case 2:
                    spanSize++;
                    spans.add(new Span(true, spanSize));
                    spanSize = 0;
                    break;
                default:
                    break;
            }
            if (off == 3) {
                off = 0;
                idx++;
                if (idx < len) {
                    val = vector[idx];
                }
            } else {
                off++;
            }
        }
        return spans;
    }
}
