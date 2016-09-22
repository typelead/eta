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
    public IntArrayList ints;
    public LongArrayList longs;
    public FloatArrayList floats;
    public DoubleArrayList doubles;

    public ArgumentStack() {
        super(7);
    }

    public ArgumentStack(int closureSize) {
        super(closureSize);
    }

    public ArgumentStack(final ObjectArrayList closures,
                         final ObjectArrayList objects,
                         final IntArrayList ints,
                         final LongArrayList longs,
                         final FloatArrayList floats,
                         final DoubleArrayList doubles) {
        super(closures);
        this.objects = objects;
        this.ints = ints;
        this.longs = longs;
        this.floats = floats;
        this.doubles = doubles;
    }

    @Override
    public void dump() {
        super.dump();
        System.out.println("O" + objects);
        System.out.println("I" + ints);
        System.out.println("L" + longs);
        System.out.println("F" + floats);
        System.out.println("D" + doubles);
    }

    @Override
    public void populate(Builder builder) {
        super.populate(builder);
        if (objects != null) builder.objects = objects.copy();
        if (ints != null) builder.ints = ints.copy();
        if (longs != null) builder.longs = longs.copy();
        if (floats != null) builder.floats = floats.copy();
        if (doubles != null) builder.doubles = doubles.copy();
    }

    public Object O(int index) {
        return objects.get(index - 1);
    }

    public void O(int index, Object o) {
        if (objects == null) objects = new ObjectArrayList(1);
        objects.set(index - 1, o);
    }

    public int I(int index) {
        return ints.get(index - 1);
    }

    public void I(int index, int i) {
        if (ints == null) ints = new IntArrayList(1);
        ints.set(index - 1, i);
    }

    public long L(int index) {
        return longs.get(index - 1);
    }

    public void L(int index, long l) {
        if (longs == null) longs = new LongArrayList(1);
        longs.set(index - 1, l);
    }

    public float F(int index) {
        return floats.get(index - 1);
    }

    public void F(int index, float f) {
        if (floats == null) floats = new FloatArrayList(1);
        floats.set(index - 1, f);
    }

    public double D(int index) {
        return doubles.get(index - 1);
    }

    public void D(int index, double d) {
        if (doubles == null) doubles = new DoubleArrayList(1);
        doubles.set(index - 1, d);
    }
}
