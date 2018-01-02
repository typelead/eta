package eta.runtime.util;

import java.util.Arrays;

public class Report {
    public static void header(StringBuilder rest, String header) {
        char[] headerChars = new char[header.length() + 1];
        Arrays.fill(headerChars, '-');
        headerChars[headerChars.length - 1] = '\n';
        rest.append(headerChars);
        rest.append(header + '\n');
        rest.append(headerChars);
    }

    private static final String blankLine = "\n";

    public static void blankLine(StringBuilder sb) {
        sb.append(blankLine);
    }

    public static void format(StringBuilder sb, String format, Object... args) {
        sb.append(String.format(format + '\n', args));
    }
}
