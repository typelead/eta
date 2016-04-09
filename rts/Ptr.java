package ghcvm.runtime;

public class Ptr<T> {
    public T ref;

    public Ptr(T ref) {
        this.ref = ref;
    }
}
