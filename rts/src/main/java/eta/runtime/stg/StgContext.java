package eta.runtime.stg;

import java.util.Arrays;
import eta.Closure;
import eta.Thunk;
import eta.runtime.Capability;
import eta.runtime.thunk.UpdateInfo;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.stg.ArgumentStack.*;

public class StgContext {
    public Capability myCapability;
    public TSO currentTSO;

    // All related to trampoline
    public int tailCalls;
    public boolean trampoline;
    public boolean firstTime;
    public Closure next;

    // Exception handling
    public Closure raise;

    /* Stack fields */
    public static final int R_LIMIT = 6;
    public static final int R_OFFSET = 7;
    public Closure R1;
    public Closure R2;
    public Closure R3;
    public Closure R4;
    public Closure R5;
    public Closure R6;
    public Closure[] Rs = new Closure[6];

    public static final int O_LIMIT = 6;
    public static final int O_OFFSET = 7;
    public Object O1;
    public Object O2;
    public Object O3;
    public Object O4;
    public Object O5;
    public Object O6;
    public Object[] Os = new Object[1];

    public static final int I_LIMIT = 6;
    public static final int I_OFFSET = 7;
    public int I1;
    public int I2;
    public int I3;
    public int I4;
    public int I5;
    public int I6;
    public int[] Is = new int[1];

    public static final int L_LIMIT = 6;
    public static final int L_OFFSET = 7;
    public long L1;
    public long L2;
    public long L3;
    public long L4;
    public long L5;
    public long L6;
    public long[] Ls = new long[1];

    public static final int D_LIMIT = 6;
    public static final int D_OFFSET = 7;
    public double D1;
    public double D2;
    public double D3;
    public double D4;
    public double D5;
    public double D6;
    public double[] Ds = new double[1];

    public static final int F_LIMIT = 6;
    public static final int F_OFFSET = 7;
    public float F1;
    public float F2;
    public float F3;
    public float F4;
    public float F5;
    public float F6;
    public float[] Fs = new float[1];

    public void reset(Capability cap, TSO t) {
        myCapability = cap;
        currentTSO = t;
        raise = null;
        resetArgStack();
        resetTrampoline(0, false);
        if (t != null) t.reset();
    }

    public boolean getAndSetTrampoline() {
        boolean old = trampoline;
        trampoline = false;
        return old;
    }

    public void resetTrampoline(int tailCalls, boolean trampoline) {
        this.tailCalls  = tailCalls;
        this.trampoline = trampoline;
        this.firstTime  = false;
        this.next       = null;
    }

    public void resetArgStack() {
        resetRs();
        Arrays.fill(Rs, null);
        resetOs();
        Arrays.fill(Os, null);
    }

    public void resetRs() {
        R1 = null;
        R2 = null;
        R3 = null;
        R4 = null;
        R5 = null;
        R6 = null;
    }

    public void resetOs() {
        O1 = null;
        O2 = null;
        O3 = null;
        O4 = null;
        O5 = null;
        O6 = null;
    }

    public void saveStackTrace(Exception e, StackTraceElement[] ste) {
        currentTSO.saveStack(e, ste);
    }

    public UpdateInfo pushUpdate(Thunk updatee) {
        return currentTSO.updateInfoStack.push(updatee);
    }

    public Thunk popUpdate() {
        return currentTSO.updateInfoStack.pop();
    }

    public void merge(ArgumentStack stack) {
        byte flag = stack.typeFlag;

        if ((flag & P_FLAG) != 0) {
            Closure[] closures = stack.closures;
            int len = closures.length;
            switch (Math.min(len, R_LIMIT)) {
              case 6:
                 R6 = closures[5];
              case 5:
                 R5 = closures[4];
              case 4:
                 R4 = closures[3];
              case 3:
                 R3 = closures[2];
              case 2:
                 R2 = closures[1];
              case 1:
                 R1 = closures[0];
              default:
                break;
            }
            int newLen = len - R_LIMIT;
            if (newLen > 0) {
                if (Rs.length < newLen) {
                    Rs = new Closure[newLen];
                }
                System.arraycopy(closures, R_LIMIT, Rs, 0, newLen);
            }
        }

        if ((flag & O_FLAG) != 0) {
            Object[] objects = stack.objects;
            int len = objects.length;
            switch (Math.min(len, O_LIMIT)) {
              case 6:
                  O6 = objects[5];
              case 5:
                  O5 = objects[4];
              case 4:
                  O4 = objects[3];
              case 3:
                  O3 = objects[2];
              case 2:
                  O2 = objects[1];
              case 1:
                  O1 = objects[0];
              default:
                  break;
            }
            int newLen = len - O_LIMIT;
            if (newLen > 0) {
                if (Os.length < newLen) {
                    Os = new Object[newLen];
                }
                System.arraycopy(objects, 6, Os, 0, newLen);
            }
        }

        if ((flag & I_FLAG) != 0) {
            int[] ints = stack.ints;
            int len = ints.length;
            switch (Math.min(len, 6)) {
              case 6:
                  I6 = ints[5];
              case 5:
                  I5 = ints[4];
              case 4:
                  I4 = ints[3];
              case 3:
                  I3 = ints[2];
              case 2:
                  I2 = ints[1];
              case 1:
                  I1 = ints[0];
              default:
                  break;
            }
            int newLen = len - I_LIMIT;
            if (newLen > 0) {
                if (Is.length < newLen) {
                    Is = new int[newLen];
                }
                System.arraycopy(ints, 6, Is, 0, newLen);
            }
        }

        if ((flag & L_FLAG) != 0) {
            long[] longs = stack.longs;
            int len = longs.length;
            switch (Math.min(len, 6)) {
              case 6:
                  L6 = longs[5];
              case 5:
                  L5 = longs[4];
              case 4:
                  L4 = longs[3];
              case 3:
                  L3 = longs[2];
              case 2:
                  L2 = longs[1];
              case 1:
                  L1 = longs[0];
              default:
                  break;
            }
            int newLen = len - L_LIMIT;
            if (newLen > 0) {
                if (Ls.length < newLen) {
                    Ls = new long[newLen];
                }
                System.arraycopy(longs, 6, Ls, 0, newLen);
            }
        }

        if ((flag & F_FLAG) != 0) {
            float[] floats = stack.floats;
            int len = floats.length;
            switch (Math.min(len, 6)) {
              case 6:
                  F6 = floats[5];
              case 5:
                  F5 = floats[4];
              case 4:
                  F4 = floats[3];
              case 3:
                  F3 = floats[2];
              case 2:
                  F2 = floats[1];
              case 1:
                  F1 = floats[0];
              default:
                  break;
            }
            int newLen = len - F_LIMIT;
            if (newLen > 0) {
                if (Fs.length < newLen) {
                    Fs = new float[newLen];
                }
                System.arraycopy(floats, 6, Fs, 0, newLen);
            }
        }

        if ((flag & D_FLAG) != 0) {
            double[] doubles = stack.doubles;
            int len = doubles.length;
            switch (Math.min(len, 6)) {
              case 6:
                  D6 = doubles[5];
              case 5:
                  D5 = doubles[4];
              case 4:
                  D4 = doubles[3];
              case 3:
                  D3 = doubles[2];
              case 2:
                  D2 = doubles[1];
              case 1:
                  D1 = doubles[0];
              default:
                  break;
            }
            int newLen = len - D_LIMIT;
            if (newLen > 0) {
                if (Ds.length < newLen) {
                    Ds = new double[newLen];
                }
                System.arraycopy(doubles, 6, Ds, 0, newLen);
            }
        }
    }

    public static StgContext acquire() {
        Capability cap = Capability.getLocal();
        StgContext context = cap.context;
        if (context.currentTSO != null) {
            return context;
        } else {
            context.currentTSO = new TSO(null);
            return context;
        }
    }

    public void dump() {
        System.out.println("Context Dump");
        System.out.println("currentTSO: " + currentTSO);
        System.out.println("myCapabilitymyCapability: " + myCapability);
    }

    public Closure R(int index) {
        int idx = index - R_OFFSET;
        if (idx < Rs.length) {
            return Rs[idx];
        } else {
            barf("Attempted to access an unavailable element from the Closure argument stack.");
            return null;
        }
    }

    public void R(int index, Closure closure) {
        int idx = index - R_OFFSET;
        if (idx >= Rs.length) {
            Closure[] oldRs = Rs;
            Rs = new Closure[idx + 3];
            System.arraycopy(oldRs, 0, Rs, 0, oldRs.length);

        }
        Rs[idx] = closure;
    }

    public Object O(int index) {
        int idx = index - O_OFFSET;
        if (idx < Os.length) {
            return Os[idx];
        } else {
            barf("Attempted to access an unavailable element from the Object argument stack.");
            return null;
        }
    }

    public void O(int index, Object o) {
        int idx = index - O_OFFSET;
        if (idx >= Os.length) {
            Object[] oldOs = Os;
            Os = new Object[idx + 3];
            System.arraycopy(oldOs, 0, Os, 0, oldOs.length);

        }
        Os[idx] = o;
    }

    public int I(int index) {
        int idx = index - I_OFFSET;
        if (idx < Is.length) {
            return Is[idx];
        } else {
            barf("Attempted to access an unavailable element from the Integer argument stack.");
            return 0;
        }
    }

    public void I(int index, int i) {
        int idx = index - I_OFFSET;
        if (idx >= Is.length) {
            int[] oldIs = Is;
            Is = new int[idx + 3];
            System.arraycopy(oldIs, 0, Is, 0, oldIs.length);

        }
        Is[idx] = i;
    }

    public long L(int index) {
        int idx = index - L_OFFSET;
        if (idx < Ls.length) {
            return Ls[idx];
        } else {
            barf("Attempted to access an unavailable element from the Long argument stack.");
            return 0L;
        }
    }

    public void L(int index, long l) {
        int idx = index - L_OFFSET;
        if (idx >= Ls.length) {
            long[] oldLs = Ls;
            Ls = new long[idx + 3];
            System.arraycopy(oldLs, 0, Ls, 0, oldLs.length);

        }
        Ls[idx] = l;
    }

    public float F(int index) {
        int idx = index - F_OFFSET;
        if (idx < Fs.length) {
            return Fs[idx];
        } else {
            barf("Attempted to access an unavailable element from the Float argument stack.");
            return 0.0f;
        }
    }

    public void F(int index, float f) {
        int idx = index - F_OFFSET;
        if (idx >= Fs.length) {
            float[] oldFs = Fs;
            Fs = new float[idx + 3];
            System.arraycopy(oldFs, 0, Fs, 0, oldFs.length);

        }
        Fs[idx] = f;
    }

    public double D(int index) {
        int idx = index - D_OFFSET;
        if (idx < Ds.length) {
            return Ds[idx];
        } else {
            barf("Attempted to access an unavailable element from the Double argument stack.");
            return 0.0;
        }
    }

    public void D(int index, double d) {
        int idx = index - D_OFFSET;
        if (idx >= Ds.length) {
            double[] oldDs = Ds;
            Ds = new double[idx + 3];
            System.arraycopy(oldDs, 0, Ds, 0, oldDs.length);

        }
        Ds[idx] = d;
    }
}
