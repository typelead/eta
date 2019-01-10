package eta.runtime.util;

/* This class is *not* thread-safe! Meant for small sets. */
public class CompactReferenceSet<E> {

    private E[] refs;
    private int mask;
    private int numElems;

    /* BUCKET_SIZE = 2 ^ BUCKET_BITS */

    private static final int BUCKET_BITS = 3;
    private static final int BUCKET_SIZE = 8;

    @SuppressWarnings("unchecked")
    /* Must be a power of 2 > 8 */
    public CompactReferenceSet(int initialCapacity) {
        this.refs = (E[]) new Object[initialCapacity];
        this.mask = (initialCapacity >>> BUCKET_BITS) - 1;
    }

    public boolean add(E e) {
        for(;;) {
            final E[] refs = this.refs;
            final int i = hashIndex(e);
            int setJ = -1;
            for (int j = 0; j < BUCKET_SIZE; j++) {
                final E ref = refs[i + j];
                if (ref == null) {
                    /* If the slot is empty.. */
                    if (setJ < 0)  {
                        /* And we haven't filled in a slot yet, save the slot. */
                        setJ = j;
                    }
                } else if (ref == e) {
                    /* If we find a duplicate, don't add it. */
                    return false;
                }
            }

            if (setJ >= 0) {
                refs[i + setJ] = e;
                this.numElems++;
                return true;
            }
            resize();
        }
    }

    @SuppressWarnings("unchecked")
    private void resize() {
        final E[] oldRefs     = this.refs;
        final int len         = oldRefs.length;

        final int newLen = len << 1;
        this.refs = (E[]) new Object[newLen];
        this.mask = (newLen >>> BUCKET_BITS) - 1;
        this.numElems = 0;

        for (int i = 0; i < len; i++) {
            final E ref = oldRefs[i];
            if (ref != null) {
                add(ref);
            }
        }
    }

    public boolean contains(E e) {
        assert e != null;
        final E[] refs = this.refs;
        final int i = hashIndex(e);
        for (int j = 0; j < BUCKET_SIZE; j++) {
            if (refs[i + j] == e) {
                return true;
            }
        }
        return false;
    }

    public boolean remove(E e) {
        assert e != null;
        final E[] refs = this.refs;
        final int i = hashIndex(e);
        for (int j = 0; j < BUCKET_SIZE; j++) {
            final int idx = i + j;
            if (refs[idx] == e) {
                refs[idx] = null;
                this.numElems--;
                return true;
            }
        }
        return false;
    }

    public void clear() {
        if (numElems > 0) {
            final E[] refs = this.refs;
            final int len  = refs.length;

            for (int i = 0; i < len; i++) {
                if (refs[i] != null) {
                    refs[i] = null;
                }
            }

            this.numElems = 0;
        }
    }

    public int size() {
        return numElems;
    }

    private int hashIndex(E e) {
        return (e.hashCode() & this.mask) * BUCKET_SIZE;
    }
}
