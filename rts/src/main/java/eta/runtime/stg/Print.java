package eta.runtime.stg;

import java.math.BigInteger;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import java.util.Arrays;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import eta.runtime.apply.Function;
import eta.runtime.apply.PAP;
import eta.runtime.thunk.Thunk;
import static eta.runtime.stg.Closures.*;

public class Print {

    public static class LeftBiasedPair<A extends Comparable<A>,B>
        implements Comparable<LeftBiasedPair<A, B>> {

        public final A x1;
        public final B x2;

        public LeftBiasedPair(final A x1, final B x2) {
            this.x1 = x1;
            this.x2 = x2;
        }

        @Override
        public int compareTo(LeftBiasedPair<A, B> o) {
            return this.x1.compareTo(o.x1);
        }
    }

    public static class PrintState {
        public final StringBuilder sb = new StringBuilder();

        private final Deque<Object> stack = new ArrayDeque<Object>();

        private final Map<Object, Boolean> seen =
            new IdentityHashMap<Object, Boolean>(16);

        private int nextInstanceNumber = 1;

        // Closure instance to instance number
        private final Map<Integer, Integer> closureInstanceIndex = new HashMap<Integer, Integer>();

        // Map from object identifier to index of string buffer - bimap dual of above
        private final Map<Integer, Integer> revBufIndex = new HashMap<Integer, Integer>();

        // Used to track top-level closures which don't need to be referenced
        private final Set<Class<?>> ignoreNoFields = new HashSet<Class<?>>();

        public int getId(final String closureType, final Integer id) {
            Integer found0 = closureInstanceIndex.get(id);
            if (found0 != null) return found0;
            int found = nextInstanceNumber++;
            closureInstanceIndex.put(id, found);
            return found;
        }

        public void insertMapping(final Object o) {
            revBufIndex.put(System.identityHashCode(o), sb.length());
        }

        public void push(Object e) {
            stack.offerFirst(e);
        }

        public Object pop() {
            return stack.pollFirst();
        }

        public void markSeen(Object c) {
            if (c instanceof Closure) {
                seen.put(c, Boolean.TRUE);
            }
        }

        public void ignoreClosure(Class<?> c) {
            ignoreNoFields.add(c);
        }

        public boolean hasSeen(Object o) {
            final Class<?> clazz = o.getClass();
            return isValidSeenClass(clazz)
                && !ignoreNoFields.contains(clazz)
                && seen.get(o) != null;
        }

        public Iterator<LeftBiasedPair<Integer, Integer>> replacementsIterator() {
            final Set<LeftBiasedPair<Integer, Integer>> replacements =
                new TreeSet<LeftBiasedPair<Integer, Integer>>();
            for (Map.Entry<Integer, Integer> entry : closureInstanceIndex.entrySet()) {
                int id = entry.getKey();
                int instanceNo = entry.getValue();
                Integer pos = revBufIndex.get(id);
                if (pos == null) {
                  throw new RuntimeException("Unable to find revBufIndex for " + id);
                }
                replacements.add(new LeftBiasedPair(pos, instanceNo));
            }
            return replacements.iterator();
        }
    }

    public static String closureToString(Object target) {
        final PrintState ps = new PrintState();
        final StringBuilder sb = ps.sb;
        ps.push(target);
        while ((target = ps.pop()) != null) {
            final Class<?> clazz = target.getClass();
            ps.markSeen(target);
            if (target instanceof PrintInstruction) {
                ((PrintInstruction)target).print(ps);
            } else if (target instanceof Closure) {
                if (target instanceof Function) {
                    final String prefix = getClosureName(clazz) + '['
                                        + ((Function)target).arity() + ']';
                    pushFields(target, clazz, prefix, ps);
                } else if (target instanceof Thunk) {
                    final Closure indirectee = ((Thunk)target).indirectee;
                    if (indirectee instanceof Value) {
                        ps.insertMapping(target);
                        ps.push(indirectee);
                    } else {
                        final String prefix = getClosureName(clazz) + "[_]";
                        pushFields(target, clazz, prefix, ps);
                    }
                } else if (target instanceof PAP) {
                    final PAP pap = (PAP) target;
                    final Function fun = pap.fun;
                    sb.append('{');
                    ps.push("}[" + pap.arity + ']');
                    pap.writeArgs(writeObjectField(fun, "fun", ps), ps);
                } else if (target instanceof DataCon) {
                    if (!handleSpecialClosure((DataCon)target, clazz, ps)) {
                        pushFields(target, clazz, getClosureName(clazz), ps);
                    }
                } else {
                    sb.append(classAndIdentity(target));
                    ps.insertMapping(target);
                }
            } else {
                sb.append(target);
            }
        }

        final Iterator<LeftBiasedPair<Integer, Integer>> it = ps.replacementsIterator();
        int off = 0;
        while (it.hasNext()) {
            final LeftBiasedPair<Integer, Integer> p = it.next();
            final int i = p.x1;
            final int j = p.x2;
            final String id = "#" + j;
            sb.insert(i + off, id);
            off += id.length();
        }

        char i0 = sb.charAt(0);
        char i1 = (sb.length() > 1)? sb.charAt(1) : '?';
        if (i0 == '(' && (Character.isDigit(i1) || Character.isLetter(i1))) {
            return sb.substring(1, sb.length() - 1);
        } else {
            return sb.toString();
        }
    }

    public static void handleSpecialChar(int cp, final StringBuilder sb) {
        if (cp == '\n') {
            sb.append("\\n");
        } else if (cp == '\r'){
            sb.append("\\r");
        } else {
            sb.appendCodePoint(cp);
        }
    }

    public static boolean handleSpecialClosure(final DataCon c, final Class<?> clazz,
                                               final PrintState ps) {
        final StringBuilder sb = ps.sb;
        try {
            if (Izh.isAssignableFrom(clazz)) {
                sb.append(unIzh.getInt(c));
            } else if (Szh.isAssignableFrom(clazz)) {
                sb.append(unSzh.getInt(c));
            } else if (Czh.isAssignableFrom(clazz)) {
                sb.append('\'');
                handleSpecialChar(unCzh.getInt(c), sb);
                sb.append('\'');
            } else if (Jzh.isAssignableFrom(clazz)) {
                sb.append(((BigInteger)unJzh.get(c)).toString());
            } else if (ZC.isAssignableFrom(clazz)) {
                final List<Closure> cs = new ArrayList<Closure>();
                cs.add(c.get(1));
                boolean printFull = false;
                Closure d = c.get(2);
                for (;;) {
                    if (ZMZN.isInstance(d)) {
                        printFull = true;
                        break;
                    } else if (ZC.isInstance(d)) {
                        final DataCon e = (DataCon) d;
                        cs.add(e.get(1));
                        d = e.get(2);
                    } else if (d instanceof Thunk) {
                        final Closure e = ((Thunk) d).indirectee;
                        if (e instanceof Value) {
                            d = e;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                if (printFull) {
                    ps.insertMapping(c);
                    String str = allChar(cs);
                    if (str != null) {
                        sb.append(str);
                    } else {
                        sb.append('[');
                        ps.push("]");
                        int i = cs.size();
                        final ListIterator<Closure> it = cs.listIterator(cs.size());
                        while (it.hasPrevious() && i > 1) {
                            ps.push(it.previous());
                            ps.push(", ");
                            i--;
                        }
                        ps.push(it.previous());
                    }
                }
                return printFull;
            } else {
                return false;
            }
        } catch (IllegalAccessException iae) {
            return false;
        }
        return true;
    }

    public static String allChar(Iterable<Closure> cs) {
        final StringBuilder sb = new StringBuilder();
        sb.append('"');
        boolean isString = true;
        for (Closure c: cs) {
            isString = isString && isChar(c, sb);
            if (!isString) break;
        }
        sb.append('"');
        return isString? sb.toString() : null;
    }

    public static boolean isChar(Closure c, final StringBuilder sb) {
        while (c instanceof Thunk) {
            c = ((Thunk) c).indirectee;
        }
        if (Czh.isInstance(c)) {
            try {
                handleSpecialChar(unCzh.getInt(c), sb);
            } catch (IllegalAccessException iae) {
                return false;
            }
            return true;
        } else {
            return false;
        }
    }

    public static void handleParens(final Object c, final String prefix, final PrintState ps) {
        final StringBuilder sb = ps.sb;
        sb.append('(');
        sb.append(prefix);
        ps.insertMapping(c);
        ps.push(")");
    }

    public static boolean isValidField(final Field f, final boolean skipIndirectee) {
        return !Modifier.isStatic(f.getModifiers())
            && !(skipIndirectee && f.getName().equals("indirectee"));

    }

    public static boolean isValidSeenClass(final Class<?> clazz) {
        if (Closure.class.isAssignableFrom(clazz)) {
            return !(Czh.isAssignableFrom(clazz)
                  || Szh.isAssignableFrom(clazz)
                  || Jzh.isAssignableFrom(clazz)
                  || Izh.isAssignableFrom(clazz));
        } else {
            return false;
        }
    }

    public static void pushFields(final Object c, final Class<?> clazz, final String prefix,
                                  final PrintState ps) {
        final boolean skipIndirectee = Thunk.class.isAssignableFrom(clazz);
        final Field[] fs = clazz.getFields();
        final int numFields = fs.length;
        boolean wrotePrefix = false;
        int i = 0;
        for (; i < numFields; i++) {
            final Field f = fs[i];
            if (isValidField(f, skipIndirectee)) {
                if (!wrotePrefix) {
                    handleParens(c, prefix, ps);
                    wrotePrefix = true;
                }
                if (writeField(c, f, ps) != null)  {
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
                        handleParens(c, prefix, ps);
                        wrotePrefix = true;
                    }
                    ps.push(PrintField.create(c, f));
                }
            }
        }
        if (!wrotePrefix) {
            ps.sb.append(prefix);
            ps.insertMapping(c);
            ps.ignoreClosure(clazz);
        }
    }

    public static Object writeField(final Object c, final Field f, final PrintState ps) {
        try {
            final Class<?> type = f.getType();
            final StringBuilder sb = ps.sb;
            if (type.isPrimitive()) {
                writePrimitiveField(sb, f, c, type);
                return null;
            } else if (type.isArray()) {
                writeArrayField(sb, f.get(c), type.getComponentType());
                return null;
            } else {
                return writeObjectField(f.get(c), f.getName(), ps);
            }
        } catch (IllegalAccessException iae) {
            throw new RuntimeException("writeField", iae);
        }
    }

    public static abstract class PrintInstruction {
        public abstract void print(PrintState ps);
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
        public void print(final PrintState ps) {
            maybeAddPendingWithSpace(writeField(c, f, ps), ps);
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
        public void print(final PrintState ps) {
            maybeAddPendingWithSpace(writeObjectField(c, f, ps), ps);
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
        public void print(final PrintState ps) {
            writeArrayField(ps.sb, c, f);
        }
    }

    public static void maybeAddPending(final Object pending, final PrintState ps) {
        if (pending != null) {
            ps.push(pending);
        }
    }

    public static void maybeAddPendingWithSpace(final Object pending, final PrintState ps) {
        if (pending != null) {
            ps.push(pending);
            ps.push(" ");
        }
    }

    public static Object writeObjectField(final Object o, final String fieldName,
                                          final PrintState ps) {

        final StringBuilder sb = ps.sb;
        if (o == null) {
            sb.append(" {");
            sb.append(fieldName);
            sb.append("=null}");
            return null;
        } else if (ps.hasSeen(o)) {
            sb.append(" @");
            final String closureType = getClosureName(o.getClass());
            sb.append(closureType);
            int id = ps.getId(closureType, System.identityHashCode(o));
            sb.append('#');
            sb.append(id);
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

    public static void writeArrayField(final StringBuilder sb, final Object c,
                                       final Class<?> type) {
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
            res = Arrays.deepToString((Object[])c);
        }
        sb.append(' ');
        sb.append(res);
    }

    public static void writePrimitiveField(final StringBuilder sb, final Field f,
                                           final Object c, final Class<?> type) {
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

    public static String getClosureName(final Class<?> cls) {
        // TODO: We may have to handle the '$' sign.
        return zdecode(cls.getSimpleName());
    }

    public static String zdecode(final String zstring) {
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

    public static int zdecodeUpper(final int cp) {
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

    public static int zdecodeLower(final int cp) {
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

    public static int zdecodeTuple(final StringBuilder sb, final String zstring, int offset,
                                   int cp) {
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

    public static int zdecodeNumEscape(final StringBuilder sb, final String zstring, int offset,
                                       int cp) {
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

    public static String classAndIdentity(final Object e) {
        if (e == null) {
            return "null";
        } else {
            return e.getClass() + "#" + System.identityHashCode(e);
        }
    }
}
