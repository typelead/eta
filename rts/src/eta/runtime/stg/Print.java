package eta.runtime.stg;

import java.lang.reflect.Field;

import java.util.Arrays;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class Print {

    public static void writeFields(StringBuilder sb, Class<?> clazz, Object c, boolean skipFirst) {
        Field[] fs = clazz.getFields();
        int i = skipFirst? 1 : 0;
        for (; i < fs.length; i++) {
            writeField(sb, fs[i], c);
        }
    }

    public static void writeField(StringBuilder sb, Field f, Object c) {
        try {
            sb.append(' ');
            Class<?> type = f.getType();
            if (type.isPrimitive()) {
                writePrimitiveField(sb, f, c, type);
            } else if (type.isArray()) {
                writeArrayField(sb, f.get(c), type.getComponentType());
            } else {
                writeObjectField(sb, f.get(c));
            }
        } catch (IllegalAccessException iae) {
            /* TODO: We need to shift the StringBuilder back by one.
               Although, this case should never happen since we always
               generate code with public access. */
        }
    }

    public static void writeObjectField(StringBuilder sb, Object o) {
        if (o == null) sb.append("null");
        else {
            String s = o.toString();
            if (hasWhitespace(s)) {
                sb.append('(');
                sb.append(s);
                sb.append(')');
            } else {
                sb.append(s);
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
        } catch (IllegalAccessException iae) {}
    }

    public static String getClosureName(Class<? extends Closure> cls) {
        // TODO: We may have to handle the '$' sign.
        return zdecode(cls.getSimpleName());
    }

    public static String zdecode(String zstring) {
        StringBuilder out = new StringBuilder();
        int cp1, cp2;
        int encodedLen = zstring.length();
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

    public static int zdecodeTuple(StringBuilder sb, String zstring, int offset, int cp) {
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

    public static int zdecodeNumEscape(StringBuilder sb, String zstring, int offset, int cp) {
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

    public static boolean isHexDigit(int cp) {
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

    private static boolean hasWhitespace(String s) {
        Matcher m = whitespaceMatcher.get();
        m.reset(s);
        return m.matches();
    }
}
