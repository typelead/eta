package eta.runtime;

import org.junit.Assert;

public class TestUtils {
    public static void assertArrayEquals(byte[] actual, byte[] expected) {
        StringBuilder sb = new StringBuilder();
        sb.append("[Expected] Size: " + expected.length +
                    " Contents: " + toHexString(expected) + "\n");
        sb.append("[Actual]   Size: " + actual.length +
                    " Contents: " + toHexString(actual) + "\n");
        Assert.assertArrayEquals(sb.toString(), expected, actual);
    }

    public static String toHexString(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        sb.append('[');
        if (bytes.length > 0) {
            sb.append("0x" + Integer.toHexString(bytes[0] & 0xFF));
            for (int i = 1; i < bytes.length; i++) {
                sb.append(", 0x");
                sb.append(Integer.toHexString(bytes[i] & 0xFF));
            }
        }
        sb.append(']');
        return sb.toString();
    }
}
