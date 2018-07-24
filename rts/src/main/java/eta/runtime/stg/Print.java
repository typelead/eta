package eta.runtime.stg;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import java.util.Arrays;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import eta.runtime.stg.Closure;
import eta.runtime.stg.DataCon;
import eta.runtime.apply.Function;
import eta.runtime.apply.PAP;
import eta.runtime.thunk.Thunk;

public class Print {

    public static String closureToString(Object target) {
        final Map<Object, Boolean> visited =
            new IdentityHashMap<Object, Boolean>(16);
        final Deque<Object> stack = new ArrayDeque<Object>();
        final StringBuilder sb = new StringBuilder();
        stack.offerFirst(target);
        while ((target = stack.pollFirst()) != null) {
            final Class<?> clazz = target.getClass();
            if (target instanceof Closure) {
                visited.put(target, Boolean.TRUE);
            }
            if (target instanceof PrintInstruction) {
                ((PrintInstruction)target).print(sb, visited, stack);
            } else if (target instanceof Closure) {
                if (target instanceof Function) {
                    // TODO: Maybe make this a fully qualified name?
                    final String prefix = getClosureName(clazz) + '['
                                        + ((Function)target).arity() + ']';
                    pushFields(target, clazz, prefix, sb, stack, visited);
                } else if (target instanceof Thunk) {
                    final Closure indirectee = ((Thunk)target).indirectee;
                    if (indirectee instanceof Value) {
                        // TODO: Should make some indication of an indirection?
                        stack.offerFirst(indirectee);
                    } else {
                        // TODO: Maybe make this a fully qualified name?
                        final String prefix = getClosureName(clazz) + "[_]";
                        pushFields(target, clazz, prefix, sb, stack, visited);
                    }
                } else if (target instanceof PAP) {
                    final PAP pap = (PAP) target;
                    final Function fun = pap.fun;
                    sb.append('{');
                    stack.offerFirst("}[" + pap.arity + ']');
                    pap.writeArgs(sb, writeObjectField(fun, "fun", sb, visited), visited, stack);
                } else if (target instanceof DataCon) {
                    // TODO: Maybe make this a fully qualified name?
                    pushFields(target, clazz, getClosureName(clazz), sb, stack, visited);
                } else {
                    sb.append(classAndIdentity(target));
                }
            } else {
                sb.append(target);
            }
        }
        if (sb.charAt(0) == '(') {
            return sb.substring(1, sb.length() - 1);
        } else {
            return sb.toString();
        }
    }

    public static void handleParens(final Object c, final StringBuilder sb, final String prefix,
                                    final Deque<Object> stack) {
        sb.append('(');
        sb.append(prefix);
        sb.append('#');
        sb.append(System.identityHashCode(c));
        stack.offerFirst(")");
    }

    public static boolean isValidField(final Field f, final boolean skipIndirectee) {
        return !Modifier.isStatic(f.getModifiers())
            && !(skipIndirectee && f.getName().equals("indirectee"));

    }

    public static void pushFields(final Object c, final Class<?> clazz, final String prefix,
                                  final StringBuilder sb, final Deque<Object> stack,
                                  final Map<Object, Boolean> seen) {
        boolean skipIndirectee = Thunk.class.isAssignableFrom(clazz);
        final Field[] fs = clazz.getFields();
        final int numFields = fs.length;
        boolean wrotePrefix = false;
        int i = 0;
        for (; i < numFields; i++) {
            final Field f = fs[i];
            if (isValidField(f, skipIndirectee)) {
                if (!wrotePrefix) {
                    handleParens(c, sb, prefix, stack);
                    wrotePrefix = true;
                }
                if (writeField(c, f, sb, seen) != null)  {
                    break;
                }
            }
        }
        // If we terminated early
        if (i < numFields) {
            final int start = i;
            for (i = numFields - 1; i >= start; i--) {
                final Field f = fs[i];
                if (isValidField(f, skipIndirectee)) {
                    if (!wrotePrefix) {
                        handleParens(c, sb, prefix, stack);
                        wrotePrefix = true;
                    }
                    stack.offerFirst(PrintField.create(c, f));
                }
            }
        }
        if (!wrotePrefix) {
            sb.append(prefix);
        }
    }

    public static Object writeField(final Object c, final Field f,
                                    final StringBuilder sb,
                                    final Map<Object, Boolean> seen) {
        try {
            final Class<?> type = f.getType();
            if (type.isPrimitive()) {
                writePrimitiveField(sb, f, c, type);
                return null;
            } else if (type.isArray()) {
                writeArrayField(sb, f.get(c), type.getComponentType());
                return null;
            } else {
                return writeObjectField(f.get(c), f.getName(), sb, seen);
            }
        } catch (IllegalAccessException iae) {
            throw new RuntimeException("writeField", iae);
        }
    }

    public static abstract class PrintInstruction {
        public abstract void print(StringBuilder sb, Map<Object, Boolean> seen, Deque<Object> stack);
    }

    public static class PrintField extends PrintInstruction {

        private final Object c;
        private final Field f;

        private PrintField(final Object c, final Field f) {
            this.c = c;
            this.f = f;
        }

        public static Object create(final Object c, final Field f) {
            return new PrintField(c, f);
        }

        @Override
        public void print(StringBuilder sb, Map<Object, Boolean> seen, Deque<Object> stack) {
            maybeAddPendingWithSpace(writeField(c, f, sb, seen), stack);
        }
    }

    public static class PrintObjectField extends PrintInstruction {

        private final Object c;
        private final String f;

        private PrintObjectField(final Object c, final String f) {
            this.c = c;
            this.f = f;
        }

        public static Object create(final Object c, final String f) {
            return new PrintObjectField(c, f);
        }

        @Override
        public void print(StringBuilder sb, Map<Object, Boolean> seen, Deque<Object> stack) {
            maybeAddPendingWithSpace(writeObjectField(c, f, sb, seen), stack);
        }
    }

    public static class PrintArrayField<E> extends PrintInstruction {

        private final Object c;
        private final Class<E> f;

        private PrintArrayField(final Object c, final Class<E> f) {
            this.c = c;
            this.f = f;
        }

        public static <E> Object create(final Object c, final Class<E> f) {
            return new PrintArrayField<E>(c, f);
        }

        @Override
        public void print(StringBuilder sb, Map<Object, Boolean> seen, Deque<Object> stack) {
            writeArrayField(sb, c, f);
        }
    }

    public static void maybeAddPending(Object pending, Deque<Object> stack) {
        if (pending != null) {
            stack.offerFirst(pending);
        }
    }

    public static void maybeAddPendingWithSpace(Object pending, Deque<Object> stack) {
        if (pending != null) {
            stack.offerFirst(pending);
            stack.offerFirst(" ");
        }
    }

    public static Object writeObjectField(final Object o, final String fieldName,
                                          final StringBuilder sb,
                                          final Map<Object, Boolean> seen) {
        if (o == null) {
            sb.append(" {");
            sb.append(fieldName);
            sb.append("=null}");
            return null;
        } else if ((o instanceof Closure) && seen.get(o) != null) {
            sb.append(" @");
            sb.append(getClosureName(o.getClass()));
            sb.append('#');
            sb.append(System.identityHashCode(o));
            return null;
        } else {
            if (o instanceof Closure) {
                return o;
            } else {
                final String s = o.toString();
                sb.append(' ');
                if (hasWhitespace(s)) {
                    sb.append('(');
                    sb.append(s);
                    sb.append(')');
                } else {
                    sb.append(s);
                }
                return null;
            }
        }
    }

    public static void writeArrayField(StringBuilder sb, Object c, Class<?> type) {
        String res;
        if (type == Integer.TYPE) {
            res = Arrays.toString((int[])c);
        } else if (type == Long.TYPE) {
            res = Arrays.toString((long[])c);
        } else if (type == Float.TYPE) {
            res = Arrays.toString((float[])c);
        } else if (type == Double.TYPE) {
            res = Arrays.toString((double[])c);
        } else if (type == Boolean.TYPE) {
            res = Arrays.toString((boolean[])c);
        } else if (type == Byte.TYPE) {
            res = Arrays.toString((byte[])c);
        } else if (type == Short.TYPE) {
            res = Arrays.toString((short[])c);
        } else if (type == Character.TYPE) {
            res = Arrays.toString((char[])c);
        } else {
            // TOOD: Take into account the 'seen' array.
            res = Arrays.deepToString((Object[])c);
        }
        sb.append(' ');
        sb.append(res);
    }

    public static void writePrimitiveField(StringBuilder sb, Field f, Object c, Class<?> type) {
        try {
            sb.append(' ');
            if (type == Integer.TYPE) {
                sb.append(f.getInt(c));
            } else if (type == Long.TYPE) {
                sb.append(f.getLong(c));
                sb.append('L');
            } else if (type == Float.TYPE) {
                sb.append(f.getFloat(c));
                sb.append('f');
            } else if (type == Double.TYPE) {
                sb.append(f.getDouble(c));
            } else if (type == Boolean.TYPE) {
                sb.append(f.getBoolean(c));
            } else if (type == Byte.TYPE) {
                sb.append(f.getByte(c));
                sb.append('b');
            } else if (type == Short.TYPE) {
                sb.append(f.getShort(c));
                sb.append('s');
            } else if (type == Character.TYPE) {
                sb.append(f.getChar(c));
            }
        } catch (IllegalAccessException iae) {
            throw new RuntimeException("writePrimitiveField", iae);
        }
    }

    public static String getClosureName(Class<?> cls) {
        // TODO: We may have to handle the '$' sign.
        return zdecode(cls.getSimpleName());
    }

    public static String zdecode(String zstring) {
        final StringBuilder out = new StringBuilder();
        final int encodedLen = zstring.length();
        int cp1, cp2;
        int offset = 0;
        while (offset < encodedLen) {
            cp1 = zstring.codePointAt(offset);
            offset += Character.charCount(cp1);
            if (cp1 == 'Z') {
                cp2 = zstring.codePointAt(offset);
                offset += Character.charCount(cp2);
                if (Character.isDigit(cp2)) {
                    offset = zdecodeTuple(out, zstring, offset, cp2);
                } else {
                    out.appendCodePoint(zdecodeUpper(cp2));
                }
            } else if (cp1 == 'z') {
                cp2 = zstring.codePointAt(offset);
                offset += Character.charCount(cp2);
                if (Character.isDigit(cp2)) {
                    offset = zdecodeNumEscape(out, zstring, offset, cp2);
                } else {
                    out.appendCodePoint(zdecodeLower(cp2));
                }
            } else {
                out.appendCodePoint(cp1);
            }
        }
        return out.toString();
    }

    public static int zdecodeUpper(int cp) {
        switch ((char) cp) {
            case 'L':
                return '(';
            case 'R':
                return ')';
            case 'M':
                return '[';
            case 'N':
                return ']';
            case 'C':
                return ':';
            case 'Z':
                return 'Z';
            default:
                return cp;
        }
    }

    public static int zdecodeLower(int cp) {
        switch ((char) cp) {
            case 'z':
                return 'z';
            case 'a':
                return '&';
            case 'b':
                return '|';
            case 'c':
                return '^';
            case 'e':
                return '=';
            case 'g':
                return '>';
            case 'h':
                return '#';
            case 'i':
                return '.';
            case 'l':
                return '<';
            case 'm':
                return '-';
            case 'n':
                return '!';
            case 'p':
                return '+';
            case 'q':
                return '\'';
            case 'r':
                return '\\';
            case 's':
                return '/';
            case 't':
                return '*';
            case 'v':
                return '%';
            default:
                return cp;
        }
    }

    public static int zdecodeTuple(final StringBuilder sb, final String zstring, int offset, int cp) {
        int n = Character.getNumericValue(cp);
        while (offset < zstring.length()) {
            cp      = zstring.codePointAt(offset);
            offset += Character.charCount(cp);
            if (Character.isDigit(cp)) {
                n = 10 * n + Character.getNumericValue(cp);
                continue;
            } else if (n == 0 && cp == 'T') {
                sb.append("()");
                return offset;
            } else if (cp == 'T') {
                sb.append('(');
                for (int i = 1; i < n; i++) {
                    sb.append(',');
                }
                sb.append(')');
                return offset;
            } else if (n == 1 && cp == 'H') {
                sb.append("(# #)");
                return offset;
            } else if (cp == 'H') {
                sb.append("(#");
                for (int i = 1; i < n; i++) {
                    sb.append(',');
                }
                sb.append("#)");
                return offset;
            } else {
                throw new IllegalArgumentException("The input string is not in proper Z-encoded form!");
            }
        }
        throw new IllegalArgumentException("The input string seems to be a fragment of a proper Z-encoded string.");
    }

    public static int zdecodeNumEscape(final StringBuilder sb, final String zstring, int offset, int cp) {
        int n = Character.getNumericValue(cp);
        while (offset < zstring.length()) {
            cp      = zstring.codePointAt(offset);
            offset += Character.charCount(cp);
            if (isHexDigit(cp)) {
                n = 16 * n + Character.getNumericValue(cp);
                continue;
            } else if (cp == 'U') {
                sb.appendCodePoint(n);
                return offset;
            } else {
                throw new IllegalArgumentException("The input string contains an invalid Unicode escape sequence.");
            }
        }
        throw new IllegalArgumentException("The input string is a fragmented Z-encoded string.");
    }

    public static boolean isHexDigit(final int cp) {
        return Character.isDigit(cp)
            || (cp >= 'A' && cp <= 'F')
            || (cp >= 'a' && cp <= 'f');
    }

    private static final Pattern whitespacePattern = Pattern.compile(".*\\s+.*");
    private static final ThreadLocal<Matcher> whitespaceMatcher =
        new ThreadLocal<Matcher>() {
            @Override
            public Matcher initialValue() {
                return whitespacePattern.matcher("");
            }
        };

    private static boolean hasWhitespace(final String s) {
        final Matcher m = whitespaceMatcher.get();
        m.reset(s);
        return m.matches();
    }

    public static String classAndIdentity(Object e) {
        if (e == null) {
            return "null";
        } else {
            return e.getClass() + "#" + System.identityHashCode(e);
        }
    }
}
