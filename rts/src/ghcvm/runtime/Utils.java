package ghcvm.runtime;

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
}
