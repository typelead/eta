package ghcvm.runtime;

import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.ListIterator;

public class Utils {
    public static <E> E peekPrevious(ListIterator<E> it) {
        E e = null;
        if (it.hasPrevious()) {
            e = it.previous();
            it.next();
        }
        return e;
    }

    public static <E> E peekNext(ListIterator<E> it) {
        E e = null;
        if (it.hasNext()) {
            e = it.next();
            it.previous();
        }
        return e;
    }

    public static int c_isatty(int tty) {
        return System.console() != null ? 1 : 0;
    }

    private static PrintStream getPrintStream(int file) {
        switch (file) {
        case 1:
            return System.out;
        case 2:
            return System.err;
        default:
            throw new IllegalArgumentException("Invalid file descriptor");
        }
    }

    public static String c_localeEncoding() {
        return Charset.defaultCharset().name();
    }

    public static long c_write(int file, ByteBuffer buffer, long count) {
        try {
            PrintStream stream = getPrintStream(file);
            byte[] dst = new byte[(int) count];
            buffer.get(dst, 0, (int) count);
            stream.print(new String(dst, "US-ASCII"));
            return count;
        } catch (UnsupportedEncodingException ignored) {
            return -1;
        } catch (IllegalArgumentException ignored) {
            return -1;
        }
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
}
