package ghcvm.runtime;

import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.util.ListIterator;
import static java.nio.charset.StandardCharsets.US_ASCII;

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
        case 0:
            return System.out;
        case 1:
            return System.err;
        default:
            throw new IllegalArgumentException("Invalid file descriptor");
        }
    }

    public static long c_write(int file, ByteBuffer buffer, long count) {
        try {
            PrintStream stream = getPrintStream(file);
            byte[] dst = new byte[(int) count];
            buffer.get(dst, 0, (int) count);
            stream.print(new String(dst, US_ASCII));
            return 0;
        } catch (IllegalArgumentException ignored) {
            return -1;
        }
    }
}
