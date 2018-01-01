package eta.runtime.util;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static eta.runtime.TestUtils.*;

public class MPSCLongQueueTest {

    MPSCLongQueue queue;

    @Before
    public void init() {
        queue = new MPSCLongQueue();
    }

    @Test
    public void testMPSCInit() {
        long sequence = 0;
        assertEquals(false, queue.canRead(sequence));
        queue.write(123);
        assertEquals(true, queue.canRead(sequence));
        assertEquals(123, queue.read(sequence++));
        assertEquals(true, queue.canRead(sequence - 1));
        assertEquals(false, queue.canRead(sequence));
    }

    @Test
    public void testMPSCMulti() {
        int chunkSize = MPSCLongQueue.DEFAULT_CHUNK_SIZE;
        long sequence = 0;
        for (int i = 0; i < chunkSize; i++) {
            queue.write(i);
        }
        for (int i = 0; i < chunkSize; i++) {
            assertEquals(true, queue.canRead(sequence));
            assertEquals(i, queue.read(sequence++));
        }
        int chunkSizeInc = chunkSize + 1;
        assertEquals(false, queue.canRead(sequence));
        queue.write(chunkSizeInc);
        assertEquals(true, queue.canRead(sequence));
        assertEquals(chunkSizeInc, queue.read(sequence++));
    }

    /* We put the timeout in case a deadlock happens. */
    @Test(timeout = 1000)
    public void testMPSCConcurrent() {
        final int chunkSize = MPSCLongQueue.DEFAULT_CHUNK_SIZE;
        final int limit = 2 * chunkSize - 1;
        Thread consumer = new Thread() {
                @Override
                public void run() {
                    for (long sequence = 0; sequence < limit; sequence++) {
                        if (queue.canRead(sequence)) {
                            assertEquals((int)sequence, queue.read(sequence));
                        }
                    }
                }
            };
        Thread producer = new Thread() {
                @Override
                public void run() {
                    for (int i = 0; i < limit; i++) {
                        queue.write(i);
                    }
                }
            };
        for(;;) {
            try {
                consumer.join();
                producer.join();
                break;
            } catch (InterruptedException ie) {
            }
        }
    }
}
