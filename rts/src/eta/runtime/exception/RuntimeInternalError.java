package eta.runtime.exception;

public class RuntimeInternalError extends Error {

    public RuntimeInternalError(String message) {
        // @VERSION_CHANGE@
        // @BUILD_NUMBER
        super("\n[Eta v0.0.9b5] " + message + "\nPlease report this as a bug: https://github.com/typelead/eta/issues");
    }
}
