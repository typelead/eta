package eta.runtime.util;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicLongArray;
import java.util.concurrent.atomic.AtomicReferenceArray;

/* An zero-allocation unbounded multi-producer, single-consumer queue which tries to minimize
   contention. It allocates only to expand the internal buffer in case of bursts of traffic. */
public class MPSCReferenceQueue<E> {

    public static final int DEFAULT_CHUNK_SIZE = 64;
    public static final long READABLE_BIT = 1L << 63;
    private final AtomicBoolean lock = new AtomicBoolean();
    private final AtomicLong writeSequence = new AtomicLong();
    private final RingBuffer<E> headBuffer;

    /* Cached constants */
    private final int chunkSize;
    private final int chunkMask;
    private final int chunkBits;

    /* This will be treated as a copy-on-write array
       to ensure consistent traversal. */
    @SuppressWarnings("unchecked")
    private volatile RingBuffer<E>[] buffers = (RingBuffer<E>[])new RingBuffer[0];

    /* chunkSize should ideally be a power of 2. */
    public MPSCReferenceQueue() {
        this(DEFAULT_CHUNK_SIZE);
    }

    public MPSCReferenceQueue(final int chunkSize) {
        this.chunkSize = chunkSize;
        this.chunkMask = chunkSize - 1;
        this.chunkBits = Integer.numberOfTrailingZeros(chunkSize);

        this.headBuffer = new RingBuffer<E>(chunkSize);
    }

    /* Returns an element to read or null if no element is available. */
    public E read(final long sequence) {
        final int  index     = (int)(sequence &   chunkMask);
        final long iteration =       sequence >>> chunkBits;

        /* Fast path: You're likely to find the next item on the first ring buffer. */
        if (headBuffer.isReadable(iteration, index)) {
            return headBuffer.get(iteration, index);
        } else {
            return canReadSlow(iteration, index);
        }
    }

    private E canReadSlow(final long iteration, final int index) {

        final RingBuffer<E>[] buffers = this.buffers;
        final int numBuffers = buffers.length;

        /* Slow path: Search for the ring buffer with the sequence number you need. */
        for (int i = 0; i < numBuffers; i++) {
            final RingBuffer<E> buffer = buffers[i];
            if (buffer.isReadable(iteration, index)) {
                return buffer.get(iteration, index);
            }
        }
        return null;
    }

    public int readIndex(final long sequence) {
        final int  index     = (int)(sequence &   chunkMask);
        final long iteration =       sequence >>> chunkBits;

        /* Fast path: You're likely to find the next item on the first ring buffer. */
        if (headBuffer.isReadable(iteration, index)) {
            return index;
        } else {
            return readIndexSlow(iteration, index);
        }
    }

    private int readIndexSlow(final long iteration, final int index) {

        final RingBuffer<E>[] buffers = this.buffers;
        final int numBuffers = buffers.length;

        /* Slow path: Search for the ring buffer with the sequence number you need. */
        for (int i = 0; i < numBuffers; i++) {
            final RingBuffer<E> buffer = buffers[i];
            if (buffer.isReadable(iteration, index)) {
                return index + chunkSize * (i + 1);
            }
        }
        throw new IllegalStateException("Unable to find index at iteration: " +
                                        iteration + " index: " + index);
    }

    public long write(final E val) {
        if (val == null) {
            throw new IllegalArgumentException("Cannot write null to an MPSCReferenceQueue!");
        }
        final long sequence     = writeSequence.getAndIncrement();
        final long iteration    =       sequence >>> chunkBits;
        final int  index        = (int)(sequence &   chunkMask);
        if (!headBuffer.set(iteration, index, val)) {
            writeSlow(iteration, index, val);
        }
        return sequence;
    }

    private void writeSlow(final long iteration, final int index, final E val) {
        RingBuffer<E>[] buffers;
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

    private boolean createNewBufferRetry(final long iteration, final int index, final E val,
                                         final RingBuffer<E>[] buffers, final int numBuffers) {
        if (!lock.get() && lock.compareAndSet(false, true)) {
            try {
                /* The buffers could've updated by the time we get the lock. */
                if (this.buffers != buffers) {
                    return true;
                }
                final RingBuffer<E> newBuffer = new RingBuffer<E>(chunkSize);
                @SuppressWarnings("unchecked")
                final RingBuffer<E>[] newBuffers = (RingBuffer<E>[])
                    new RingBuffer[numBuffers + 1];
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

    /* WARNING: This function is unsafe! It may consume elements that were already
                consumed if read() calls are made simulatenously to this call. */
    public int forEach(Consumer<E> consumer) {
        int processed = headBuffer.forEach(consumer);
        for (RingBuffer<E> buffer: buffers) {
            processed += buffer.forEach(consumer);
        }
        return processed;
    }

    public void dump() {
        final RingBuffer<E>[] buffers = this.buffers;
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


    private static class RingBuffer<E> {
        /* TODO: Avoid false sharing by using 64 byte stretches at a time? */
        private final AtomicReferenceArray<E> references;
        private final AtomicLongArray available;

        /* This is a carefully chosen value (of many such values) that is never a valid
           iteration value for either reading or writing. */
        private final long WRITE_LOCK = -1L;
        private final int chunkBits;

        public RingBuffer(final int size) {
            this.references = new AtomicReferenceArray<E>(size);
            this.available  = new AtomicLongArray(size);
            this.chunkBits  = Integer.numberOfTrailingZeros(size);
        }

        public E get(final long iteration, final int i) {
            E res = references.get(i);
            /* Done to allow the reference to released early. */
            references.lazySet(i, null);
            available.set(i, 0);
            return res;
        }

        public boolean set(final long iteration, final int i, final E val) {
            boolean success = false;
            final long avail = available.get(i);
            if (avail == 0) {
                /* This is here to fend off the competition. This is a value that
                   can never be read nor written to. This typically does not happen unless
                   the contention rate is enormous, and is kept here for correctness.
                   If can be removed if message loss is harmless and the read code
                   should be adjusted accordingly to detect message loss.
                */
                success = available.compareAndSet(i, 0, WRITE_LOCK);
                if (success) {
                    weakSet(iteration, i, val);
                }
            }
            return success;
        }

        public void weakSet(final long iteration, final int i, final E val) {
            references.set(i, val);
            available.set(i, (READABLE_BIT | iteration));
        }

        public boolean isReadable(final long iteration, final int i) {
            return available.get(i) == (READABLE_BIT | iteration);
        }

        /* WARNING: This function is unsafe! It may consume elements that were already
           consumed if read() calls are made simulatenously to this call. */
        public int forEach(Consumer<E> consumer) {
            int processed = 0;
            final int len = available.length();
            for (int i = 0; i < len; i++) {
                final long avail = available.get(i);
                if ((avail & READABLE_BIT) != 0) {
                    final long iteration = avail & ~READABLE_BIT;
                    E e = get(iteration, i);
                    if (e != null) {
                        consumer.accept(e);
                        processed++;
                    }
                }
            }
            return processed;
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
