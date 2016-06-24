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

import ghcvm.runtime.apply.StgPAP;

public class ArgumentStack extends AbstractArgumentStack {
    public ObjectArrayList objects;
    public AbstractIntList ints;
    public AbstractLongList longs;
    public AbstractFloatList floats;
    public AbstractDoubleList doubles;

    public ArgumentStack() {
        super();
    }

    public ArgumentStack(final ObjectArrayList closures, final ObjectArrayList objects, final AbstractIntList ints, final AbstractLongList longs, AbstractFloatList floats, AbstractDoubleList doubles) {
        super(closures);
        this.objects = objects;
        this.ints = ints;
        this.longs = longs;
        this.floats = floats;
        this.doubles = doubles;
    }

    @Override
    public void populate(Builder builder) {
        super.populate(builder);
        if (objects != null) builder.objects = (ObjectArrayList) objects.clone();
        if (ints != null) builder.ints = (AbstractIntList) ints.clone();
        if (longs != null) builder.longs = (AbstractLongList) longs.clone();
        if (floats != null) builder.floats = (AbstractFloatList) floats.clone();
        if (doubles != null) builder.doubles = (AbstractDoubleList) doubles.clone();
    }

    public Object O(int index) {
        return objects.get(index);
    }

    public void O(int index, Object o) {
        if (objects == null) objects = new ObjectArrayList(1);
        objects.set(index, o);
    }

    public int I(int index) {
        return ints.get(index);
    }

    public void I(int index, int i) {
        if (ints == null) ints = new IntArrayList(1);
        ints.set(index, i);
    }

    public long L(int index) {
        return longs.get(index);
    }

    public void L(int index, long l) {
        if (longs == null) longs = new LongArrayList(1);
        longs.set(index, l);
    }

    public float F(int index) {
        return floats.get(index);
    }

    public void F(int index, float f) {
        if (floats == null) floats = new FloatArrayList(1);
        floats.set(index, f);
    }

    public double D(int index) {
        return doubles.get(index);
    }

    public void D(int index, double d) {
        if (doubles == null) doubles = new DoubleArrayList(1);
        doubles.set(index, d);
    }
}
