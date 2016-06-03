package ghcvm.runtime;

import java.util.Set;
import java.util.HashSet;

public class RtsFlags {
    public static String progName;
    public static String[] progArgs;

    public static boolean debug = false;
    public static Set<String> DebugFlags = new HashSet<String>();

    public static class ParFlags {
        public static int nNodes;
    }

    public static class ConcFlags {
        public static int ctxtSwitchTicks;
    }

    public static class ModeFlags {
        public static boolean threaded = false;
        public static boolean userSignals = false;
    }

    public static class MiscFlags {
        public static boolean installSignalHandlers = false;
    }
}
