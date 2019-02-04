package eta.runtime.stg;

import java.util.Arrays;

import static eta.runtime.stg.Print.*;

public class ArgumentStack implements Cloneable {
    public static final int NONE = 0;
    public static final int P_FLAG = 1;
    public static final int O_FLAG = 2;
    public static final int I_FLAG = 4;
    public static final int L_FLAG = 8;
    public static final int F_FLAG = 16;
    public static final int D_FLAG = 32;
    public Closure[] closures;
    public Object[] objects;
    public int[] ints;
    public long[] longs;
    public float[] floats;
    public double[] doubles;
    public byte typeFlag;

    private ArgumentStack() {}

    public ArgumentStack copy() {
        try {
            return (ArgumentStack) clone();
        } catch (CloneNotSupportedException cne) {
            return null;
        }
    }

    public static ArgumentStack createFrom(ArgumentStack from, int n) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 1;
        int[] oldInts = to.ints;
        if (oldInts != null) {
            len += oldInts.length;
        }
        int[] newInts = new int[len];
        if (oldInts != null) {
            System.arraycopy(oldInts, 0, newInts, 0, len - 1);
        }
        newInts[len - 1] = n;
        to.ints = newInts;
        to.typeFlag |= I_FLAG;
        return to;
    }

    public static ArgumentStack createFrom(ArgumentStack from, long l) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 1;
        long[] oldLongs = to.longs;
        if (oldLongs != null) {
            len += oldLongs.length;
        }
        long[] newLongs = new long[len];
        if (oldLongs != null) {
            System.arraycopy(oldLongs, 0, newLongs, 0, len - 1);
        }
        newLongs[len - 1] = l;
        to.longs = newLongs;
        to.typeFlag |= L_FLAG;
        return to;
    }

    public static ArgumentStack createFrom(ArgumentStack from, float f) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 1;
        float[] oldFloats = to.floats;
        if (oldFloats != null) {
            len += oldFloats.length;
        }
        float[] newFloats = new float[len];
        if (oldFloats != null) {
            System.arraycopy(oldFloats, 0, newFloats, 0, len - 1);
        }
        newFloats[len - 1] = f;
        to.floats = newFloats;
        to.typeFlag |= F_FLAG;
        return to;
    }

    public static ArgumentStack createFrom(ArgumentStack from, double d) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 1;
        double[] oldDoubles = to.doubles;
        if (oldDoubles != null) {
            len += oldDoubles.length;
        }
        double[] newDoubles = new double[len];
        if (oldDoubles != null) {
            System.arraycopy(oldDoubles, 0, newDoubles, 0, len - 1);
        }
        newDoubles[len - 1] = d;
        to.doubles = newDoubles;
        to.typeFlag |= D_FLAG;
        return to;
    }

    public static ArgumentStack createFrom(ArgumentStack from, Object o) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 1;
        Object[] oldObjects = to.objects;
        if (oldObjects != null) {
            len += oldObjects.length;
        }
        Object[] newObjects = new Object[len];
        if (oldObjects != null) {
            System.arraycopy(oldObjects, 0, newObjects, 0, len - 1);
        }
        newObjects[len - 1] = o;
        to.objects = newObjects;
        to.typeFlag |= O_FLAG;
        return to;
    }

    public static ArgumentStack createFromP(ArgumentStack from, Closure closure) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 1;
        Closure[] oldClosures = to.closures;
        if (oldClosures != null) {
            len += oldClosures.length;
        }
        Closure[] newClosures = new Closure[len];
        if (oldClosures != null) {
            System.arraycopy(oldClosures, 0, newClosures, 0, len - 1);
        }
        newClosures[len - 1] = closure;
        to.closures = newClosures;
        to.typeFlag |= P_FLAG;
        return to;
    }

    public static ArgumentStack createFromP(ArgumentStack from, Closure p1, Closure p2) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 2;
        Closure[] oldClosures = to.closures;
        if (oldClosures != null) {
            len += oldClosures.length;
        }
        Closure[] newClosures = new Closure[len];
        if (oldClosures != null) {
            System.arraycopy(oldClosures, 0, newClosures, 0, len - 2);
        }
        newClosures[len - 2] = p1;
        newClosures[len - 1] = p2;
        to.closures = newClosures;
        to.typeFlag |= P_FLAG;
        return to;
    }

    public static ArgumentStack createFromP(ArgumentStack from, Closure p1, Closure p2, Closure p3) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 3;
        Closure[] oldClosures = to.closures;
        if (oldClosures != null) {
            len += oldClosures.length;
        }
        Closure[] newClosures = new Closure[len];
        if (oldClosures != null) {
            System.arraycopy(oldClosures, 0, newClosures, 0, len - 3);
        }
        newClosures[len - 3] = p1;
        newClosures[len - 2] = p2;
        newClosures[len - 1] = p3;
        to.closures = newClosures;
        to.typeFlag |= P_FLAG;
        return to;
    }

    public static ArgumentStack createFromP(ArgumentStack from, Closure p1, Closure p2, Closure p3, Closure p4) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 4;
        Closure[] oldClosures = to.closures;
        if (oldClosures != null) {
            len += oldClosures.length;
        }
        Closure[] newClosures = new Closure[len];
        if (oldClosures != null) {
            System.arraycopy(oldClosures, 0, newClosures, 0, len - 4);
        }
        newClosures[len - 4] = p1;
        newClosures[len - 3] = p2;
        newClosures[len - 2] = p3;
        newClosures[len - 1] = p4;
        to.closures = newClosures;
        to.typeFlag |= P_FLAG;
        return to;
    }

    public static ArgumentStack createFromP(ArgumentStack from, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 5;
        Closure[] oldClosures = to.closures;
        if (oldClosures != null) {
            len += oldClosures.length;
        }
        Closure[] newClosures = new Closure[len];
        if (oldClosures != null) {
            System.arraycopy(oldClosures, 0, newClosures, 0, len - 5);
        }
        newClosures[len - 5] = p1;
        newClosures[len - 4] = p2;
        newClosures[len - 3] = p3;
        newClosures[len - 2] = p4;
        newClosures[len - 1] = p5;
        to.closures = newClosures;
        to.typeFlag |= P_FLAG;
        return to;
    }

    public static ArgumentStack createFromP(ArgumentStack from, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        ArgumentStack to = (from == null)? new ArgumentStack():from.copy();
        int len = 6;
        Closure[] oldClosures = to.closures;
        if (oldClosures != null) {
            len += oldClosures.length;
        }
        Closure[] newClosures = new Closure[len];
        if (oldClosures != null) {
            System.arraycopy(oldClosures, 0, newClosures, 0, len - 6);
        }
        newClosures[len - 6] = p1;
        newClosures[len - 5] = p2;
        newClosures[len - 4] = p3;
        newClosures[len - 3] = p4;
        newClosures[len - 2] = p5;
        newClosures[len - 1] = p6;
        to.closures = newClosures;
        to.typeFlag |= P_FLAG;
        return to;
    }

    public void dump() {
        System.out.println("R" + Arrays.toString(closures));
        System.out.println("O" + Arrays.toString(objects));
        System.out.println("I" + Arrays.toString(ints));
        System.out.println("L" + Arrays.toString(longs));
        System.out.println("F" + Arrays.toString(floats));
        System.out.println("D" + Arrays.toString(doubles));
    }

    public String argLens() {
        return "P["  + ((closures != null)? closures.length : 0) +
               "]O[" + ((objects  != null)? objects.length  : 0) +
               "]N[" + ((ints     != null)? ints.length     : 0) +
               "]L[" + ((longs    != null)? longs.length    : 0) +
               "]F[" + ((floats   != null)? floats.length   : 0) +
               "]D[" + ((doubles  != null)? doubles.length  : 0) + "]";
    }

    public void writeArgs(final Object pending, final PrintState ps) {
        final int numClosures = closures.length;
        int i = 0;
        if (pending == null && closures != null && numClosures > 0) {
            for (; i < numClosures; i++) {
                final Closure c = closures[i];
                Object pending2 = writeObjectField(c, Integer.toString(i), ps);
                if (pending2 != null) {
                    break;
                }
            }
        }
        if (i < numClosures || pending != null) {
            if (doubles != null && doubles.length > 0) {
                ps.push(PrintArrayField.create(doubles, double.class));
            }
            if (floats != null && floats.length > 0) {
                ps.push(PrintArrayField.create(floats, float.class));
            }
            if (longs != null && longs.length > 0) {
                ps.push(PrintArrayField.create(longs, long.class));
            }
            if (ints != null && ints.length > 0) {
                ps.push(PrintArrayField.create(ints, int.class));
            }
            if (objects != null && objects.length > 0) {
                ps.push(PrintArrayField.create(objects, Object.class));
            }
            final int start = i;
            for (i = numClosures - 1; i >= start; i--) {
                ps.push(PrintObjectField.create(closures[i], Integer.toString(i)));
            }
            if (start == 0 && pending != null) {
                ps.push(pending);
            }
        } else {
            final StringBuilder sb = ps.sb;
            if (objects != null && objects.length > 0) {
                writeArrayField(sb, objects, Object.class);
            }
            if (ints != null && ints.length > 0) {
                writeArrayField(sb, ints, int.class);
            }
            if (longs != null && longs.length > 0) {
                writeArrayField(sb, longs, long.class);
            }
            if (floats != null && floats.length > 0) {
                writeArrayField(sb, floats, float.class);
            }
            if (doubles != null && doubles.length > 0) {
                writeArrayField(sb, doubles, double.class);
            }
        }
    }
}
