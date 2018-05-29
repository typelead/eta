package eta.runtime.exception;

public class RuntimeInternalError extends Error {

    public RuntimeInternalError(String message) {
        // @VERSION_CHANGE@
        // @BUILD_NUMBER@
        // @BUILD_NUMBER_INTERNAL@
        super("\n[Eta v0.8.0b1] " + message + "\nPlease report this as a bug: https://github.com/typelead/eta/issues");
    }
}
