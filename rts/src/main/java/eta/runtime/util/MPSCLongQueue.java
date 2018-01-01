package eta.runtime.util;

import java.util.concurrent.atomic.AtomicIntegerArray;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicLongArray;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

/* An unbounded multi-producer, single-consume queue of longs which minimizes
   space usage while maintaining good performance. */
public class MPSCLongQueue {

    public static final int DEFAULT_CHUNK_SIZE = 64;
    private volatile Node head;
    private volatile Node tail;
    private final AtomicLong writeSequence = new AtomicLong();

    /* Cached constants */
    private final int chunkSize;
    private final int chunkMask;
    private final int chunkBits;

    private static final AtomicReferenceFieldUpdater<MPSCLongQueue, Node> headUpdater
        = AtomicReferenceFieldUpdater.newUpdater(MPSCLongQueue.class, Node.class, "head");

    private static final AtomicReferenceFieldUpdater<MPSCLongQueue, Node> tailUpdater
        = AtomicReferenceFieldUpdater.newUpdater(MPSCLongQueue.class, Node.class, "tail");

    /* chunkSize should ideally be a power of 2. */
    public MPSCLongQueue() {
        this(DEFAULT_CHUNK_SIZE);
    }

    public MPSCLongQueue(int chunkSize) {
        Node init = new Node(chunkSize, 0);
        this.head = init;
        this.tail = init;

        this.chunkSize = chunkSize;
        this.chunkMask = chunkSize - 1;
        this.chunkBits = Integer.numberOfTrailingZeros(chunkSize);
    }

    private Node findNode(int iteration) {
        Node node = head;
        while (node != null && node.iteration != iteration) {
            node = node.next;
        }
        return node;
    }

    public boolean canRead(long readSequence) {
        Node node = findNode((int)(readSequence >>> chunkBits));
        return node != null && node.isAvailable((int)(readSequence & chunkMask));
    }

    /* WARNING: This method is unsafe. This API assumes that the following
                pattern is followed:

       if (messages.canRead(...)) {
         messages.read(...);
       }
     */
    public long read(long sequence) {
        int index     = (int)(sequence & chunkMask);
        Node node     = findNode((int)(sequence >>> chunkBits));
        long result   = node.get(index);
        if (index == (chunkSize - 1)) {
            Node head = this.head;
            Node tail = this.tail;
            if (head == tail) {
                if (tailUpdater.compareAndSet(this, tail, null)) {
                    /* This CAS may fail, and that's OK. */
                    headUpdater.compareAndSet(this, head, null);
                    return result;
                }
            }
            Node headNext = null;
            while ((headNext = head.next) == null) {
                /* There's a narrow chance that the 'next' field may be null, so
                   we'll have to wait until the other thread populates it. */
                Thread.yield();
            }
            this.head = headNext;
        }
        return result;
    }

    public void write(long val) {
        long sequence = writeSequence.getAndIncrement();
        int iteration = (int)(sequence >>> chunkBits);
        int index     = (int)(sequence &   chunkMask);
        Node node     = findNode(iteration);
        if (node == null) {
            node = allocateNode(iteration);
        }
        node.set(index, val);
    }

    private Node allocateNode(int iteration) {
        Node node;
        for(;;) {
            Node oldTail = tail;
            while (oldTail != null && oldTail.iteration != iteration - 1) {
                /* This handles the case where you have more than `chunkSize`
                   concurrent writers in which case the extra writers need to
                   be throttled to avoid generating inconsistent states. */
                Thread.yield();
            }
            node = new Node(chunkSize, iteration);
            if (tailUpdater.compareAndSet(this, oldTail, node)) {
                if (oldTail == null) {
                    head = node;
                } else {
                    oldTail.next = node;
                }
            } else {
                node = tail;
                if (node == null) continue;
            }
            return node;
        }
    }

    private static class Node {
        private final AtomicLongArray longs;
        private final AtomicIntegerArray available;
        volatile int iteration;
        volatile Node next;

        public Node(int size, int iteration) {
            this.longs     = new AtomicLongArray(size);
            this.available = new AtomicIntegerArray(size);
            reset(iteration);
        }

        public long get(int i) {
            return longs.get(i);
        }

        public void set(int i, long val) {
            longs.set(i, val);
            available.set(i, 1);
        }

        public boolean isAvailable(int i) {
            return available.get(i) != 0;
        }

        public void reset(int iteration) {
            this.iteration = iteration;
        }
    }
}
