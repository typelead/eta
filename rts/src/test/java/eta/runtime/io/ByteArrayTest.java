package eta.runtime.io;

import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class ByteArrayTest {

    @Test
    public void testByteArrayFreeNull() {
        ByteArray b = ByteArray.create(0);
        b = null;
        System.gc();
        IO.checkForGCByteArrays();
    }
}
