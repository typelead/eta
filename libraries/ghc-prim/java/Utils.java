package eta.ghc_prim;

import java.nio.ByteBuffer;

import eta.runtime.io.MemoryManager;

public class Utils {
    public static byte[] byteBufferToBytes(long address) {
        ByteBuffer buffer = MemoryManager.getBoundedBuffer(address);
        byte[]     bytes  = new byte[buffer.remaining()];
        buffer.get(bytes);
        return bytes;
    }
}
