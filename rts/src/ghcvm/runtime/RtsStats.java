package ghcvm.runtime;

public class RtsStats {
    public static long startInit = 0;
    public static long elapsedInit = 0;

    public static void startInit() {
        startInit = System.nanoTime();
    }

    public static void endInit() {
        elapsedInit = System.nanoTime() - startInit;
    }
}
