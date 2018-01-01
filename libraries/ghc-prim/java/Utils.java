package eta.ghc_prim;

import java.nio.ByteBuffer;

import eta.runtime.io.MemoryManager;

public class Utils {
    public static byte[] byteBufferToBytes(long address, int len) {
        ByteBuffer buffer = MemoryManager.getBoundedBuffer(address);
        byte[]     bytes  = new byte[len];
        buffer.get(bytes);
        return bytes;
    }
}
