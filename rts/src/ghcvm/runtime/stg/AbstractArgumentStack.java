package ghcvm.runtime.stg;

import cern.colt.list.AbstractDoubleList;
import cern.colt.list.DoubleArrayList;
import cern.colt.list.AbstractFloatList;
import cern.colt.list.FloatArrayList;
import cern.colt.list.AbstractIntList;
import cern.colt.list.IntArrayList;
import cern.colt.list.AbstractLongList;
import cern.colt.list.LongArrayList;
import cern.colt.list.AbstractList;
import cern.colt.list.ObjectArrayList;

public abstract class AbstractArgumentStack {
    public ObjectArrayList closures;

    public AbstractArgumentStack() {}

    public AbstractArgumentStack(int closuresSize) {
        this(new ObjectArrayList(closuresSize));
    }

    public AbstractArgumentStack(final ObjectArrayList closures) {
        this.closures = closures;
    }

    public StgClosure R(int index) {
        return (StgClosure) closures.get(index - 1);
    }

    public void R(int index, StgClosure closure) {
        closures.set(index - 1, closure);
    }

    public boolean isSimple() { return false; }

    public void populate(AbstractArgumentStack.Builder builder) {
        builder.closures = (ObjectArrayList) closures.clone();
    }

    public static class Builder {
        public ObjectArrayList closures;
        public ObjectArrayList objects;
        public AbstractIntList ints;
        public AbstractLongList longs;
        public AbstractFloatList floats;
        public AbstractDoubleList doubles;
        public boolean simple = true;

        public void init() {
            closures = new ObjectArrayList(1);
        }

        public static Builder from(AbstractArgumentStack stack) {
            Builder builder = new Builder();
            builder.setSimple(stack.isSimple());
            stack.populate(builder);
            return builder;
        }

        public Builder addC(StgClosure closure) {
            closures.add(closure);
            return this;
        }

        public Builder add(Object object) {
            simple = false;
            if (objects == null) objects = new ObjectArrayList(1);
            objects.add(object);
            return this;
        }

        public Builder add(int i) {
            simple = false;
            if (ints == null) ints = new IntArrayList(1);
            ints.add(i);
            return this;
        }

        public Builder add(long l) {
            simple = false;
            if (longs == null) longs = new LongArrayList(1);
            longs.add(l);
            return this;
        }

        public Builder add(float f) {
            simple = false;
            if (floats == null) floats = new FloatArrayList(1);
            floats.add(f);
            return this;
        }

        public Builder add(double d) {
            simple = false;
            if (doubles == null) doubles = new DoubleArrayList(1);
            doubles.add(d);
            return this;
        }

        public AbstractArgumentStack build() {
            AbstractArgumentStack ret = null;
            if (simple) {
                ret = new SimpleArgumentStack(closures);
            } else {
                ret = new ArgumentStack(closures, objects, ints, longs, floats, doubles);
            }
            return ret;
        }

        public Builder setSimple(boolean simple) {
            this.simple = simple;
            return this;
        }
    }
}
