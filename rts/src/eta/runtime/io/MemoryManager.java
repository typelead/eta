package eta.runtime.io;

import java.util.TreeMap;
import java.util.Map;
import java.nio.ByteBuffer;
import java.lang.ref.WeakReference;

import static eta.runtime.RtsMessages.barf;

public class MemoryManager {
    private static int nextAddress = 0;

    private static TreeMap<Integer, WeakReference<ByteBuffer>> addressMap =
        new TreeMap<Integer, WeakReference<ByteBuffer>>();

    public static synchronized ByteBuffer allocateBuffer(int n, boolean direct) {
        ByteBuffer buf;
        if (direct) {
            buf = ByteBuffer.allocateDirect(n + 4); // Off-heap memory
        } else {
            buf = ByteBuffer.allocate(n + 4); // Heap memory
        }
        buf.putInt(nextAddress);
        addressMap.put(nextAddress, new WeakReference<ByteBuffer>(buf));
        nextAddress += n;
        return buf;
    }

    public static ByteBuffer getBuffer(int address) {
        Map.Entry<Integer, WeakReference<ByteBuffer>>
            entry = addressMap.floorEntry(address);
        /* The 4 is the offset of the address slot
           in the ByteBuffer. */
        if (entry != null) {
            WeakReference<ByteBuffer> ref = entry.getValue();
            ByteBuffer buf = ref.get();
            if (buf != null) {
                int pos = address - entry.getKey() + 4;
                return (ByteBuffer) buf.duplicate().position(pos);
            }
        }
        barf("Invalid address!");
        return null;
    }

    public static int getAddress(ByteBuffer buf) {
        if (buf == null) {
            return -1;
        } else {
            return buf.getInt(0) + buf.position() - 4;
        }
    }

    public static int bufferPos(ByteBuffer buf) {
        if (buf == null) {
            return -1;
        } else {
            return buf.position();
        }
    }
}
