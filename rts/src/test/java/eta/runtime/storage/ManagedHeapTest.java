package eta.runtime.storage;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

import java.util.Iterator;
import java.util.List;

import eta.runtime.stg.Capability;

public class ManagedHeapTest {

    static final int nurserySize   = 1024;
    static final int blockSize     = 4096;
    static final int miniBlockSize = 64;
    ManagedHeap heap;
    Capability  cap;

    List<BlockStats> getBlocks() {
        return heap.getStatistics().getNurseryStats().iterator().next().getBlockStats();
    }

    void assertBlocks(int numBlocks, boolean[][] allocated, int[][] sizes) {
        List<BlockStats> blockStats = getBlocks();
        assertEquals(numBlocks, blockStats.size());
        Iterator<BlockStats> it = blockStats.iterator();
        for (int i = 0; i < numBlocks; i++) {
            Iterator<Span> spans = it.next().getSpans().iterator();
            boolean[] bools = allocated[i];
            int[] ints = sizes[i];
            for (int j = 0; spans.hasNext(); j++) {
                Span span = spans.next();
                assertEquals(ints[j], span.getSize());
                assertEquals(bools[j], span.isAllocated());
            }
        }
    }


    @Before
    public void init() {
        heap = new ManagedHeap(nurserySize, blockSize, miniBlockSize);
        cap = Capability.getLocal();
        cap.cleanupLocalHeap();
    }

    @Test
    public void testManagedHeapDirectVsHeap() {
        long address1 = heap.allocateBuffer(7, false, cap);
        long address2 = heap.allocateBuffer(65, false, cap);
        long address3 = heap.allocateBuffer(65, true, cap);
        long address4 = heap.allocateBuffer(7, true, cap);
        assertBlocks(2,
                     new boolean[][] {
                         new boolean[] {true, true, false},
                         new boolean[] {true, true, false}
                     }
                     ,
                     new int[][] {
                         new int[] {1,2,61},
                         new int[] {2,1,61}
                     });
        heap.free(address1);
        heap.free(address3);
        assertBlocks(2,
                     new boolean[][] {
                         new boolean[] {false, true, false},
                         new boolean[] {false, true, false}
                     }
                     ,
                     new int[][] {
                         new int[] {1,2,61},
                         new int[] {2,1,61}
                     });
        long address5 = heap.allocateBuffer(63, false, cap);
        long address6 = heap.allocateBuffer(63, true, cap);
        assertBlocks(2,
                     new boolean[][] {
                         new boolean[] {true, true, false},
                         new boolean[] {true, false, true, false}
                     }
                     ,
                     new int[][] {
                         new int[] {1,2,61},
                         new int[] {1,1,1,61}
                     });
        heap.free(address2);
        heap.free(address4);
        heap.free(address5);
        heap.free(address6);
        assertBlocks(2,
                     new boolean[][] {
                         new boolean[] {false},
                         new boolean[] {false}
                     }
                     ,
                     new int[][] {
                         new int[] {64},
                         new int[] {64}
                     });
    }

    @Test
    public void testManagedHeapSuperBlocks() {
        long address = heap.allocateBuffer(blockSize * 2 + 1, false, cap);
        assertEquals(3 * blockSize, heap.getBlock(address).getSize());
        long[] addresses = new long[blockSize];
        addresses[0] = address;
        for (int i = 1; i < blockSize; i++) {
            addresses[i] = heap.allocateBuffer(1, false, cap);
        }
        for (int i = 0; i < blockSize; i++) {
            heap.free(addresses[i]);
        }
    }

    @Test
    public void testManagedHeapEmptyBuffer() {
        assertEquals(blockSize, heap.allocateBuffer(0, false, cap));
        assertEquals(2 * blockSize, heap.allocateBuffer(0, true, cap));
        assertBlocks(2,
                     new boolean[][] {
                         new boolean[] {false},
                         new boolean[] {false}
                     }
                     ,
                     new int[][] {
                         new int[] {64},
                         new int[] {64}
                     });
    }
}
