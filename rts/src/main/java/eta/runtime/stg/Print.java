package eta.runtime.stg;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import eta.runtime.stg.Closure;
import eta.runtime.stg.DataCon;
import eta.runtime.apply.Function;
import eta.runtime.apply.PAP;
import eta.runtime.thunk.Thunk;

public class Print {

    private static final ThreadLocal<IdentityHashMap<Object, Boolean>> localSeen =
        new ThreadLocal<IdentityHashMap<Object, Boolean>>() {
            @Override
            public IdentityHashMap<Object, Boolean> initialValue() {
                return new IdentityHashMap<Object, Boolean>(16);
            }
        };

    private static final ThreadLocal<Integer> localDepth =
        new ThreadLocal<Integer>() {
            @Override
            public Integer initialValue() {
                return 0;
            }
        };

    private static void incLocalDepth() {
        localDepth.set(localDepth.get() + 1);
    }

    private static void decLocalDepth() {
        localDepth.set(localDepth.get() - 1);
    }

    public static String closureToString(Closure target) {
        final int preDepth = localDepth.get();
        final StringBuilder sb = new StringBuilder();
        final Closure origTarget = target;
        final IdentityHashMap<Object, Boolean> seen = localSeen.get();
        if (preDepth == 0) {
            seen.clear();
        }
        incLocalDepth();
        // System.out.println("START: " + target.getClass() + "@" + target.hashCode());
        while (target != null) {
            final Class<?> clazz = target.getClass();
            Closure newTarget = null;
            seen.put(target, Boolean.TRUE);
            if (target instanceof Function) {
                // TODO: Maybe make this a fully qualified name?
                sb.append(getClosureName(clazz));
                sb.append('[');
                sb.append(Integer.toString(((Function)target).arity()));
                sb.append(']');
                writeFields(sb, seen, clazz, target, false);
            } else if (target instanceof Thunk) {
                final Closure indirectee = ((Thunk)target).indirectee;
                if (indirectee instanceof Value) {
                    // TODO: Should make some indication of an indirection?
                    newTarget = indirectee;
                } else {
                    // TODO: Maybe make this a fully qualified name?
                    sb.append(getClosureName(clazz));
                    sb.append("[_]");
                    writeFields(sb, seen, clazz, target, true);
                }
            } else if (target instanceof PAP) {
                final PAP pap = (PAP) target;
                Class<? extends Function> funClazz = pap.fun.getClass();
                // TODO: Maybe make this a fully qualified name?
                sb.append('{');
                sb.append(Print.getClosureName(funClazz));
                sb.append(' ');
                pap.writeArgs(sb, seen);
                sb.append('}');
                sb.append('[');
                sb.append(Integer.toString(pap.arity));
                sb.append(']');
            } else if (target instanceof DataCon) {
                // TODO: Maybe make this a fully qualified name?
                sb.append(getClosureName(clazz));
                writeFields(sb, seen, clazz, target, false);
            } else {
                throw new IllegalArgumentException
                    ("Unable to stringify " + target.getClass().getName() + " in "
                     + origTarget.getClass().getName() + "!");
            }
            target = newTarget;
        }
        decLocalDepth();
        if (preDepth == 0) {
            localSeen.get().clear();
        }
        return sb.toString();
    }

    public static void writeFields(final StringBuilder sb,
                                   final IdentityHashMap<Object, Boolean> seen,
                                   final Class<?> clazz, final Object c,
                                   final boolean skipIndirectee) {
        final Field[] fs = clazz.getFields();
        for (int i = 0; i < fs.length; i++) {
            final Field f = fs[i];
            if (!Modifier.isStatic(f.getModifiers())
             && !(skipIndirectee && f.getName().equals("indirectee"))) {
                writeField(sb, seen, f, c);
            }
        }
    }

    public static void writeField(final StringBuilder sb,
                                  final IdentityHashMap<Object, Boolean> seen,
                                  final Field f, final Object c) {
        try {
            sb.append(' ');
            final Class<?> type = f.getType();
            if (type.isPrimitive()) {
                writePrimitiveField(sb, f, c, type);
            } else if (type.isArray()) {
                writeArrayField(sb, f.get(c), type.getComponentType());
            } else {
                writeObjectField(sb, seen, f.getName(), f.get(c));
            }
        } catch (IllegalAccessException iae) {
            throw new RuntimeException("writeField", iae);
        }
    }

    public static void writeObjectField(final StringBuilder sb,
                                        final IdentityHashMap<Object, Boolean> seen,
                                        final String fieldName, final Object o) {
        if (o == null) {
            sb.append('{');
            sb.append(fieldName);
            sb.append("=null}");
        } else if (seen.get(o) != null) {
            sb.append('@');
            sb.append(getClosureName(o.getClass()));
        } else {
            // System.out.println(o.getClass());
            final String s = o.toString();
            if (hasWhitespace(s)) {
                sb.append('(');
                sb.append(s);
                sb.append(')');
            } else {
                sb.append(s);
            }
            seen.put(o, Boolean.TRUE);
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
        sb.append(res);
    }

    public static void writePrimitiveField(StringBuilder sb, Field f, Object c, Class<?> type) {
        try {
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
}
