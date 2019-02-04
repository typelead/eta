package eta.runtime.io;

import org.junit.Test;

public class ByteArrayTest {

    @Test
    public void testByteArrayFreeNull() {
        ByteArray b = ByteArray.create(0);
        b = null;
        System.gc();
        IO.checkForGCByteArrays();
    }
}
