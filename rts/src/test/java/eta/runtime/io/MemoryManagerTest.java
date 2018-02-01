package eta.runtime.io;

import java.nio.ByteBuffer;

import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class MemoryManagerTest {

    @Test
    public void testCopyByteArray() {
       
    }

    private ByteBuffer newBuffer(int length) {
        return ByteBuffer.allocate(length);
    }
}
