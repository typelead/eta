package eta.runtime.stg;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

public class JavaFinalizer {
    public boolean hasEnvironment;
    public long fptr;
    public long ptr;
    public long eptr;

    public JavaFinalizer(boolean hasEnvironment, long fptr, long ptr, long eptr) {
        this.hasEnvironment = hasEnvironment;
        this.fptr           = fptr;
        this.ptr            = ptr;
        this.eptr           = eptr;
    }

    public void finalize() {
        Method m = FunPtr.get(fptr);
        if (m != null) {
            try {
                if (hasEnvironment) {
                    m.invoke(null, eptr, ptr);
                } else {
                    m.invoke(null, ptr);
                }
                /* TODO: Report the exceptions */
            } catch (IllegalAccessException e) {
                // throw e;
            } catch (IllegalArgumentException e) {
                // throw e;
            } catch (InvocationTargetException e) {
                // throw e;
            }
        }
    }
}
