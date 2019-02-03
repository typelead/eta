package eta.repl;

import java.nio.Buffer;
import java.nio.ByteBuffer;

import eta.runtime.io.MemoryManager;

public class Utils {

    public static ByteBuffer byteStringToByteBuffer(long address, int size) {
        ByteBuffer buf = MemoryManager.getBoundedBuffer(address);
        ((Buffer)buf).limit(buf.position() + size);
        return buf;
    }

    public static void bytesToPtr(byte[] bytes, long address) {
        MemoryManager.getBoundedBuffer(address).put(bytes);
    }

    public static byte[] byteStringToBytes(long address, int size) {
        ByteBuffer buf = MemoryManager.getBoundedBuffer(address);
        ((Buffer)buf).limit(buf.position() + size);
        byte[] bytes = new byte[size];
        buf.get(bytes);
        return bytes;
    }
}
