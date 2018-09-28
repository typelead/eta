package eta.runtime.exception;

public class RuntimeInternalError extends Error {

    public RuntimeInternalError(String message) {
        super("\n[Eta Panic] " + message + "\nPlease report this as a bug: https://github.com/typelead/eta/issues");
    }
}
