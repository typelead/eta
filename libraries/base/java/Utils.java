package ghcvm.base;

import java.io.IOException;
import java.io.PrintStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteOrder;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;

public class Utils {
    // TODO: Verify correctness
    public static float rintFloat(float f) {
        return (float) Math.rint((double) f);
    }

    // TODO: Verify
    public static boolean isPrintableChar(int c) {
        return ((((1 << Character.UPPERCASE_LETTER)          |
                  (1 << Character.LOWERCASE_LETTER)          |
                  (1 << Character.TITLECASE_LETTER)          |
                  (1 << Character.MODIFIER_LETTER)           |
                  (1 << Character.OTHER_LETTER)              |
                  (1 << Character.COMBINING_SPACING_MARK)    |
                  (1 << Character.OTHER_NUMBER)              |
                  (1 << Character.MODIFIER_SYMBOL)           |
                  (1 << Character.ENCLOSING_MARK)            |
                  (1 << Character.DECIMAL_DIGIT_NUMBER)      |
                  (1 << Character.DASH_PUNCTUATION)          |
                  (1 << Character.OTHER_PUNCTUATION)         |
                  (1 << Character.CONNECTOR_PUNCTUATION)     |
                  (1 << Character.MATH_SYMBOL)               |
                  (1 << Character.SPACE_SEPARATOR)           |
                  (1 << Character.OTHER_SYMBOL)              |
                  (1 << Character.END_PUNCTUATION)           |
                  (1 << Character.FINAL_QUOTE_PUNCTUATION)   |
                  (1 << Character.START_PUNCTUATION)         |
                  (1 << Character.CURRENCY_SYMBOL)           |
                  (1 << Character.INITIAL_QUOTE_PUNCTUATION) |
                  (1 << Character.LETTER_NUMBER)             |
                  (1 << Character.LETTER_NUMBER)) >> Character.getType(c)) & 1)
            != 0;
    }

    public static boolean isFloatNegativeZero(float f) {
        return f == -0.0f;
    }

    public static boolean isFloatDenormalized(float f) {
        int bits = Float.floatToRawIntBits(f);
        return ((bits >> 23) & 0xff) == 0 && (bits & 0x7fffff) != 0;
    }

    public static boolean isFloatFinite(float f) {
        int bits = Float.floatToRawIntBits(f);
        return ((bits >> 23) & 0xff) != 0xff;
    }

    public static boolean isDoubleNegativeZero(double d) {
        return d == -0.0;
    }

    public static boolean isDoubleDenormalized(double d) {
        long bits = Double.doubleToRawLongBits(d);
        return ((bits >> 52) & 0x7ffL) == 0 && (bits & 0xfffffffffffffL) != 0;
    }

    public static boolean isDoubleFinite(double d) {
        long bits = Double.doubleToRawLongBits(d);
        return ((bits >> 52) & 0x7ffL) != 0x7ffL;
    }

    public static int c_isatty(int tty) {
        return System.console() != null ? 1 : 0;
    }

    private static InputStream getInputStream(int file) {
        switch (file) {
        case 0:
            return System.in;
        default:
            throw new IllegalArgumentException("Invalid file descriptor for an InputStream");
        }
    }

    private static PrintStream getPrintStream(int file) {
        switch (file) {
        case 1:
            return System.out;
        case 2:
            return System.err;
        default:
            throw new IllegalArgumentException("Invalid file descriptor for a PrintStream");
        }
    }

    public static String c_localeEncoding() {
        return Charset.defaultCharset().name();
    }

    public static int c_write(int file, ByteBuffer buffer, int count) {
        try {
            PrintStream stream = getPrintStream(file);
            byte[] dst = new byte[count];
            buffer.get(dst, 0, count);
            stream.print(new String(dst, "US-ASCII"));
            return count;
        } catch (UnsupportedEncodingException ignored) {
            return -1;
        } catch (IllegalArgumentException ignored) {
            return -1;
        }
    }

    public static int c_read(int file, ByteBuffer buffer, int count) {
        byte[] bytes = new byte[count];
        int got;
        try {
            got = System.in.read(bytes);
            buffer.put(bytes);
        } catch (IOException ignored) {
            return -1;
        }
        return got;
    }

    public static String byteBufferToStr(ByteBuffer buffer)
        throws UnsupportedEncodingException {
        byte[] bytes = new byte[buffer.remaining()];
        buffer.get(bytes);
        return new String(bytes, "UTF-8");
    }

    public static void printBuffer(ByteBuffer buffer) {
        byte[] bytes = new byte[buffer.remaining()];
        buffer.get(bytes);
        System.out.println(Arrays.toString(bytes));
    }

    public static String getOS() {
        return System.getProperty("os.name");
    }

    public static String getArch() {
        return System.getProperty("os.arch");
    }

    public static boolean isBigEndian() {
        return ByteOrder.nativeOrder().equals(ByteOrder.BIG_ENDIAN);
    }
}
