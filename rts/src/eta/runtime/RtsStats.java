package eta.runtime;

public class RtsStats {
    public static long startInit;
    public static long elapsedInit;

    public static void startInit() {
        startInit = System.nanoTime();
    }

    public static void endInit() {
        elapsedInit = System.nanoTime() - startInit;
    }
}
