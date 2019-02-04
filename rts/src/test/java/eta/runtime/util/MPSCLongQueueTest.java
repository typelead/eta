package eta.runtime.util;

import java.util.Deque;
import java.util.Map;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import static eta.runtime.TestUtils.*;
import java.util.concurrent.ConcurrentLinkedQueue;

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
        assertEquals(true, queue.canRead(sequence++));
        assertEquals(123, queue.read());
        assertEquals(false, queue.canRead(sequence - 1));
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
            assertEquals(true, queue.canRead(sequence++));
            assertEquals(i, queue.read());
        }
        int chunkSizeInc = chunkSize + 1;
        assertEquals(false, queue.canRead(sequence));
        queue.write(chunkSizeInc);
        assertEquals(true, queue.canRead(sequence++));
        assertEquals(chunkSizeInc, queue.read());
    }

    @Test
    public void testMPSCMultiLarge() {
        final int chunkSize = MPSCLongQueue.DEFAULT_CHUNK_SIZE;
        final int size = 10 * chunkSize;
        for (int i = 0; i < size; i++) {
            queue.write(i);
        }
        long sequence = 0;
        for (int i = 0; i < chunkSize; i++) {
            assertEquals(true, queue.canRead(sequence++));
            assertEquals(i, queue.read());
        }
    }

    /* We put the timeout in case a deadlock happens. */
    @Test(timeout = 1000)
    public void testMPSCConcurrent() throws Throwable {
        final Deque<Throwable> exceptions = new LinkedList<Throwable>();
        final int chunkSize = MPSCLongQueue.DEFAULT_CHUNK_SIZE;
        final int limit = 2 * chunkSize - 1;
        Thread consumer = new ExceptionalThread(exceptions) {
                @Override
                public void innerRun() {
                    for (long sequence = 0; sequence < limit; sequence++) {
                        while (!queue.canRead(sequence));
                        assertEquals(sequence, queue.read());
                    }
                }
            };
        Thread producer = new ExceptionalThread(exceptions) {
                @Override
                public void innerRun() {
                    for (int i = 0; i < limit; i++) {
                        queue.write(i);
                    }
                }
            };
        consumer.start();
        producer.start();
        consumer.join();
        producer.join();
        Throwable t = null;
        if ((t = exceptions.pollFirst()) != null) {
            throw t;
        }
    }

    @Test(timeout = 2000)
    public void testMPSCConcurrentLarge() throws Throwable {
        final Deque<Throwable> exceptions = new LinkedList<Throwable>();
        final Collection<HashMap<Long,Long>> seqMessagesOut_ =
            new ConcurrentLinkedQueue<HashMap<Long,Long>>();
        final HashMap<Long,Long> seqMessagesIn = new HashMap<Long,Long>();
        final AtomicInteger doneCount = new AtomicInteger();
        final int chunkSize = MPSCLongQueue.DEFAULT_CHUNK_SIZE;
        final int size = 1000 * chunkSize;
        int i;
        int j;
        final long[] array1 = new long[size];
        for (i = 0, j = 1; i < size; i++) {
            array1[i] = 7 * (j++);
        }
        final long[] array2 = new long[size];
        for (i = 0, j = 1; i < size; i++) {
            long val = 0;
            do {
                val = 11 * (j++);
            } while ((val % 7) == 0);
            array2[i] = val;
        }
        final long[] array3 = new long[size];
        for (i = 0, j = 1; i < size; i++) {
            long val = 0;
            do {
                val = 13 * (j++);
            } while ((val % 7) == 0 || (val % 11) == 0);
            array3[i] = val;
        }
        Thread producer1 =
            new ProducerThread(exceptions, array1, doneCount, seqMessagesOut_);
        Thread producer2 =
            new ProducerThread(exceptions, array2, doneCount, seqMessagesOut_);
        Thread producer3 =
            new ProducerThread(exceptions, array3, doneCount, seqMessagesOut_);
        Thread consumer = new ExceptionalThread(exceptions) {
                @Override
                public void innerRun() {
                    int i1 = 0;
                    int i2 = 0;
                    int i3 = 0;
                    for (long sequence = 0; sequence < 3 * size; sequence++) {
                        while (!queue.canRead(sequence)) {
                            if (doneCount.get() == 3) {
                                queue.dump();
                                assertTrue("One or more messages were dropped.", false);
                            }
                        }
                        long r = queue.read();
                        seqMessagesIn.put(sequence, r);
                        boolean oneOf = false;
                        if (i1 < size && r == array1[i1]) {
                            i1++;
                            oneOf = true;
                        } else if (i2 < size && r == array2[i2]) {
                            i2++;
                            oneOf = true;
                        } else if (i3 < size && r == array3[i3]) {
                            i3++;
                            oneOf = true;
                        }
                        assertTrue("Message was received out-of-order! "
                                + i1 + " " + i2 + " " + i3 + " - " + r, oneOf);
                    }
                }
            };
        consumer.start();
        producer1.start();
        producer2.start();
        producer3.start();
        consumer.join();
        producer1.join();
        producer2.join();
        producer3.join();
        Throwable t = null;
        if ((t = exceptions.pollFirst()) != null) {
            throw t;
        }

        final HashMap<Long,Long> seqMessagesOut = new HashMap<Long,Long>();
        for (HashMap<Long,Long> hm : seqMessagesOut_) {
            seqMessagesOut.putAll(hm);
        }

        for (Map.Entry<Long, Long> entry : seqMessagesIn.entrySet()) {
            final long seq = entry.getKey();
            final long readVal = entry.getValue();
            final long writtenVal = seqMessagesOut.get(seq);
            assertEquals("Read value is not the same as the written value!",
                         readVal, writtenVal);
        }
    }

    private class ProducerThread extends ExceptionalThread {
        private final long[] messages;
        private final AtomicInteger doneCount;
        private final HashMap<Long, Long> seqMessages = new HashMap<Long, Long>();
        private final Collection<HashMap<Long,Long>> seqMessagesOut;

        public ProducerThread(final Collection<Throwable> exceptions,
                              final long[] messages, final AtomicInteger doneCount,
                              final Collection<HashMap<Long,Long>> seqMessagesOut) {
            super(exceptions);
            this.messages = messages;
            this.doneCount = doneCount;
            this.seqMessagesOut = seqMessagesOut;
        }

        @Override
        public void innerRun() {
            final int size = messages.length;
            for (int i = 0; i < size; i++) {
                final long message = messages[i];
                long seq = queue.write(message);
                seqMessages.put(seq, message);
            }
            seqMessagesOut.add(seqMessages);
            doneCount.incrementAndGet();
        }
    }

    private abstract class ExceptionalThread extends Thread {
        private final Collection<Throwable> exceptions;

        public ExceptionalThread(Collection<Throwable> exceptions) {
            this.exceptions = exceptions;
        }

        @Override
        public void run() {
            try {
                innerRun();
            } catch (Throwable t) {
                exceptions.add(t);
            }

        }

        public abstract void innerRun();
    }
}
