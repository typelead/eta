package ghcvm.runtime.types;

public class RtsFlags {

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
