package ghcvm.runtime.types;

public class RtsFlags {
#if defined(THREADED_RTS)
        public static class ParFlags {
            public static int nNodes;
        }
#endif
}
