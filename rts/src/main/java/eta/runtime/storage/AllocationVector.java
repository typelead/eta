package eta.runtime.storage;

/* Consists of 2-bit elements which have the following representation:
   - 00 (0x0) - Non-terminal free bit
   - 01 (0x1) - Terminal free bit
   - 11 (0x3) - Non-terminal allocated bit
   - 10 (0x2) - Terminal allocated bit */
public class AllocationVector {
    private final byte[] vector;

    public AllocationVector(int numElements) {
        vector = new byte[(numElements + 0x3) >>> 2];
        vector[vector.length - 1] = (byte)(0x1 << (((numElements - 1) & 0x3) << 1));
    }

    public final void allocate(int i, int n) {
        int off = i & 0x3;
        int idx = i >>> 2;
        while (n-- > 1) {
            vector[idx] |= 0x3 << (off << 1);
            if (off == 3) {
                off = 0;
                idx++;
            } else {
                off++;
            }
        }
        off <<= 1;
        vector[idx] = (byte)((vector[idx] & ~(0x3 << off))
                                          |  (0x2 << off));
    }

    private final int bits(int val, int off) {
        off <<= 1;
        return (val & (0x3 << off)) >>> off;
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
        return n;
    }

    public final byte[] getBytes() {
        return vector;
    }
}
