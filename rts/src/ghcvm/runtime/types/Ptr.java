package ghcvm.runtime.types;

public class Ptr<T> {
    public T ref;

    public Ptr(T ref) {
        this.ref = ref;
    }
}
