package eta.runtime.io;

import java.util.TreeMap;
import java.util.Map;
import java.nio.ByteBuffer;
import java.lang.ref.WeakReference;

import static eta.runtime.RuntimeLogging.barf;

public class MemoryManager {
    private static int nextAddress = 0;

    private static TreeMap<Integer, WeakReference<ByteBuffer>> addressMap =
        new TreeMap<Integer, WeakReference<ByteBuffer>>();



    /* Special buffer to represent null pointer in eta. Only contains
       the address -1 */

    public static final ByteBuffer nullAddress = allocateBuffer(0,-1,false);

    public static synchronized ByteBuffer allocateBuffer(int n, boolean direct) {
        ByteBuffer buf = allocateBuffer(n, nextAddress, direct);
        addressMap.put(nextAddress, new WeakReference<ByteBuffer>(buf));
        nextAddress += n;
        return buf;
    }

    public static synchronized ByteBuffer allocateBuffer(int n, int address, boolean direct) {
        ByteBuffer buf = allocateAnonymousBuffer(n, direct);
        buf.putInt(address);
        return buf;
    }
    /* Use if you want to allocate a buffer that won't be using the getAddress
       method and you don't want to pollute the addressMap. */
    public static synchronized ByteBuffer allocateAnonymousBuffer(int n, boolean direct) {
        if (direct) {
            return ByteBuffer.allocateDirect(n + 4); // Off-heap memory
        } else {
            return ByteBuffer.allocate(n + 4); // Heap memory
        }
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
        return buf.getInt(0) + getPosition(buf);
    }

    public static int getPosition(ByteBuffer buf) {
        return buf.position() - 4;
    }

    private static int realPosition(int pos) {
        return pos + 4;
    }

    public static int bufGetInt(ByteBuffer buf, int pos) {
        return buf.getInt(realPosition(pos));
    }

    public static void bufPutInt(ByteBuffer buf, int pos, int value) {
        buf.putInt(realPosition(pos), value);
    }

    public static ByteBuffer bufSetPosition(ByteBuffer buf, int pos) {
        return (ByteBuffer) buf.position(realPosition(pos));
    }

    public static ByteBuffer bufSetOffset(ByteBuffer buf, int offset) {
        return (ByteBuffer) buf.position(buf.position() + offset);
    }

    public static ByteBuffer bufSetLimit(ByteBuffer buf, int limit) {
        return (ByteBuffer) buf.limit(buf.position() + limit);
    }
}
