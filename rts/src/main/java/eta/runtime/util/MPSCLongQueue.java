package eta.runtime.util;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicLongArray;
import java.util.concurrent.CopyOnWriteArrayList;

/* An unbounded multi-producer, single-consumer queue of longs which minimizes
   space usage while maintaining good performance. */
public class MPSCLongQueue {

    public static final int DEFAULT_CHUNK_SIZE = 64;
    public static final long READABLE_BIT = 1L << 63;
    private final AtomicBoolean lock = new AtomicBoolean();
    private final AtomicLong writeSequence = new AtomicLong();
    private final RingBuffer headBuffer;

    /* Cached constants */
    private final int chunkSize;
    private final int chunkMask;
    private final int chunkBits;

    /* This will be treated as a copy-on-write array
       to ensure consistent traversal. */
    private volatile RingBuffer[] buffers = new RingBuffer[0];

    /* Cached result of canRead() */
    private long readResult;

    /* chunkSize should ideally be a power of 2. */
    public MPSCLongQueue() {
        this(DEFAULT_CHUNK_SIZE);
    }

    public MPSCLongQueue(final int chunkSize) {
        this.chunkSize = chunkSize;
        this.chunkMask = chunkSize - 1;
        this.chunkBits = Integer.numberOfTrailingZeros(chunkSize);

        this.headBuffer = new RingBuffer(chunkSize);
    }

    /* Returns an index at which you can directly find the element you want to read.
       Returns -1 if nothing is there to read. */
    public boolean canRead(final long sequence) {
        final int  index     = (int)(sequence &   chunkMask);
        final long iteration =       sequence >>> chunkBits;

        /* Fast path: You're likely to find the next item on the first ring buffer. */
        if (headBuffer.isReadable(iteration, index)) {
            readResult = headBuffer.get(iteration, index);
            return true;
        } else {
            return canReadSlow(iteration, index);
        }
    }

    private boolean canReadSlow(final long iteration, final int index) {

        final RingBuffer[] buffers = this.buffers;
        final int numBuffers = buffers.length;

        /* Slow path: Search for the ring buffer with the sequence number you need. */
        for (int i = 0; i < numBuffers; i++) {
            final RingBuffer buffer = buffers[i];
            if (buffer.isReadable(iteration, index)) {
                readResult = buffer.get(iteration, index);
                return true;
            }
        }
        return false;
    }

    /* WARNING: This method is unsafe. This API assumes that the following
                pattern is followed:

       if (messages.canRead(...)) {
         long message = messages.read();
       }
     */
    public long read() {
        return readResult;
    }

    public long write(final long val) {
        final long sequence     = writeSequence.getAndIncrement();
        final long iteration    =       sequence >>> chunkBits;
        final int  index        = (int)(sequence &   chunkMask);
        if (!headBuffer.set(iteration, index, val)) {
            writeSlow(iteration, index, val);
        }
        return sequence;
    }

    private void writeSlow(final long iteration, final int index, final long val) {
        RingBuffer[] buffers;
        int numBuffers = 0;
        do {
            buffers = this.buffers;
            numBuffers = buffers.length;
            for (int i = 0; i < numBuffers; i++) {
                if (buffers[i].set(iteration, index, val)) {
                    return;
                }
            }
        } while (createNewBufferRetry(iteration, index, val, buffers, numBuffers));
    }

    private boolean createNewBufferRetry(final long iteration, final int index, final long val,
                                         final RingBuffer[] buffers, final int numBuffers) {
        if (!lock.get() && lock.compareAndSet(false, true)) {
            try {
                /* The buffers could've updated by the time we get the lock. */
                if (this.buffers != buffers) {
                    return true;
                }
                final RingBuffer newBuffer = new RingBuffer(chunkSize);
                final RingBuffer[] newBuffers = new RingBuffer[numBuffers + 1];
                System.arraycopy(buffers, 0, newBuffers, 0, numBuffers);
                newBuffers[numBuffers] = newBuffer;
                newBuffer.weakSet(iteration, index, val);
                this.buffers = newBuffers;
                return false;
            } finally {
                lock.set(false);
            }
        }
        return true;
    }

    public void dump() {
        final RingBuffer[] buffers = this.buffers;
        final int numBuffers = buffers.length;
        System.out.println("MPSCDump: NumBuffers = " + numBuffers);
        final List<Long> allMessages = new ArrayList<Long>();
        final List<Long> missingMessages = new ArrayList<Long>();
        for (int i = 0; i < numBuffers; i++) {
            allMessages.addAll(buffers[i].getReadableSequences());
        }
        Collections.sort(allMessages);
        if (allMessages.size() > 0) {
            final long min = allMessages.get(0);
            final long max = allMessages.get(allMessages.size() - 1);
            System.out.println("Min unread message: " + min);
            System.out.println("Max unread message: " + max);
            long seq = min;
            for (long sequence : allMessages) {
                if (sequence != seq) {
                    while (seq < sequence) {
                        missingMessages.add(seq);
                        seq++;
                    }
                }
                seq++;
            }
            System.out.println("Clobbered: " + missingMessages.toString());
        } else {
            System.out.println("All messages read!");
        }
    }


    private static class RingBuffer {
        /* TODO: Avoid false sharing by using 64 byte stretches at a time? */
        private final AtomicLongArray longs;
        private final AtomicLongArray available;

        /* This is a carefully chosen value (of many such values) that is never a valid
           iteration value for either reading or writing. */
        private final long WRITE_LOCK = -1L;
        private final int chunkBits;

        public RingBuffer(final int size) {
            this.longs     = new AtomicLongArray(size);
            this.available = new AtomicLongArray(size);
            this.chunkBits = Integer.numberOfTrailingZeros(size);
        }

        public long get(final long iteration, final int i) {
            long res = longs.get(i);
            available.set(i, 0);
            return res;
        }

        public boolean set(final long iteration, final int i, final long val) {
            boolean success = false;
            final long avail = available.get(i);
            if (avail == 0) {
                /* This is here to fend off the competition. This is a value that
                   can never be read nor written to. This appears to be useless since
                   it always seems to succeed. This typically does not happen unless
                   the contention rate is enormous, so if message loss can be tolerated,
                   this can be removed.
                */
                success = available.compareAndSet(i, 0, WRITE_LOCK);
                if (success) {
                    weakSet(iteration, i, val);
                }
            }
            return success;
        }

        public void weakSet(final long iteration, final int i, final long val) {
            longs.set(i, val);
            available.set(i, (READABLE_BIT | iteration));
        }

        public boolean isReadable(final long iteration, final int i) {
            return available.get(i) == (READABLE_BIT | iteration);
        }

        public List<Long> getReadableSequences() {
            final int len = available.length();
            final ArrayList<Long> messages = new ArrayList<Long>(len);
            for (int i = 0; i < len; i++) {
                final long avail = available.get(i);
                if ((avail & READABLE_BIT) != 0) {
                    final long iteration = avail & ~READABLE_BIT;
                    messages.add((iteration << chunkBits) | i);
                }
            }
            return messages;
        }
    }
}
