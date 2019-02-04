package eta.runtime.apply;

import eta.runtime.stg.Value;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.ArgumentStack;

import static eta.runtime.RuntimeLogging.barf;

public abstract class Function extends Value {

    public abstract int arity();

    @Override
    public Closure applyV(StgContext context) {
        int arity = arity();
        if (arity == 1) {
            barf(this + ": Expected implementation for applyV.");
            return null;
        } else {
            return new PAPSlow(this, arity - 1, null);
        }
    }

    @Override
    public Closure applyN(StgContext context, int n) {
        int arity = arity();
        if (arity == 1) {
            barf(this + ": Expected implementation for applyN.");
            return null;
        } else {
            return new PAPSlow(this, arity - 1,
                               ArgumentStack.createFrom(null, n));
        }
    }

    @Override
    public Closure applyL(StgContext context, long l) {
        int arity = arity();
        if (arity == 1) {
            barf(this + ": Expected implementation for applyL.");
            return null;
        } else {
            return new PAPSlow(this, arity - 1,
                               ArgumentStack.createFrom(null, l));
        }
    }

    @Override
    public Closure applyF(StgContext context, float f) {
        int arity = arity();
        if (arity == 1) {
            barf(this + ": Expected implementation for applyF.");
            return null;
        } else {
            return new PAPSlow(this, arity - 1,
                               ArgumentStack.createFrom(null, f));
        }
    }

    @Override
    public Closure applyD(StgContext context, double d) {
        int arity = arity();
        if (arity == 1) {
            barf(this + ": Expected implementation for applyD.");
            return null;
        } else {
            return new PAPSlow(this, arity - 1,
                               ArgumentStack.createFrom(null, d));
        }
    }

    @Override
    public Closure applyO(StgContext context, Object o) {
        int arity = arity();
        if (arity == 1) {
            barf(this + ": Expected implementation for applyO.");
            return null;
        } else {
            return new PAPSlow(this, arity - 1,
                               ArgumentStack.createFrom(null, o));
        }
    }

    @Override
    public Closure apply1(StgContext context, Closure p) {
        int arity = arity();
        switch (arity) {
          case 1:
              barf(this + ": Expected implementation for apply1.");
              return null;
          case 2:
              return new PAP1_1(this, p);
          default:
              return new PAPSlow(this, arity - 1,
                                 ArgumentStack.createFromP(null, p));
        }
    }

    @Override
    public Closure apply1V(StgContext context, Closure p) {
        int arity = arity();
        if (arity == 1) {
            boolean old = context.getAndSetTrampoline();
            Closure result = apply1(context, p);
            context.trampoline = old;
            return result.applyV(context);
        } else if (arity == 2) {
            barf(this + ": Expected implementation for apply1V.");
            return null;
        } else {
            return new PAPSlow(this, arity - 2,
                               ArgumentStack.createFromP(null, p));
        }
    }

    @Override
    public Closure apply2(StgContext context, Closure p1, Closure p2) {
        int arity = arity();
        if (arity == 1) {
            boolean old = context.getAndSetTrampoline();
            Closure result = apply1(context, p1);
            context.trampoline = old;
            return result.apply1(context, p2);
        } else if (arity == 2) {
            barf(this + ": Expected implementation for apply2.");
            return null;
        } else {
            return new PAPSlow(this, arity - 2,
                               ArgumentStack.createFromP(null, p1, p2));
        }
    }

    @Override
    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        int arity = arity();
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply1(context, p1);
                context.trampoline = old;
                return result.apply1V(context, p2);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply2(context, p1, p2);
                context.trampoline = old;
                return result.applyV(context);
            case 3:
                barf(this + ": Expected implementation for apply2V.");
                return null;
            default:
                return new PAPSlow(this, arity - 3,
                                   ArgumentStack.createFromP(null, p1, p2));
        }
    }

    @Override
    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        int arity = arity();
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply1(context, p1);
                context.trampoline = old;
                return result.apply2(context, p2, p3);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply2(context, p1, p2);
                context.trampoline = old;
                return result.apply1(context, p3);
            case 3:
                barf(this + ": Expected implementation for apply3.");
                return null;
            default:
                return new PAPSlow(this, arity - 3,
                                   ArgumentStack.createFromP(null, p1, p2, p3));
        }
    }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        int arity = arity();
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply1(context, p1);
                context.trampoline = old;
                return result.apply2V(context, p2, p3);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply2(context, p1, p2);
                context.trampoline = old;
                return result.apply1V(context, p3);
            case 3:
                old = context.getAndSetTrampoline();
                result = apply3(context, p1, p2, p3);
                context.trampoline = old;
                return result.applyV(context);
            case 4:
                barf(this + ": Expected implementation for apply3V.");
                return null;
            default:
                return new PAPSlow(this, arity - 4,
                                   ArgumentStack.createFromP(null, p1, p2, p3));
        }
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        int arity = arity();
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply1(context, p1);
                context.trampoline = old;
                return result.apply3(context, p2, p3, p4);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply2(context, p1, p2);
                context.trampoline = old;
                return result.apply2(context, p3, p4);
            case 3:
                old = context.getAndSetTrampoline();
                result = apply3(context, p1, p2, p3);
                context.trampoline = old;
                return result.apply1(context, p4);
            case 4:
                barf(this + ": Expected implementation for apply4.");
                return null;
            default:
                return new PAPSlow(this, arity - 4,
                                   ArgumentStack.createFromP(null, p1, p2, p3, p4));
        }
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        int arity = arity();
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply1(context, p1);
                context.trampoline = old;
                return result.apply4(context, p2, p3, p4, p5);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply2(context, p1, p2);
                context.trampoline = old;
                return result.apply3(context, p3, p4, p5);
            case 3:
                old = context.getAndSetTrampoline();
                result = apply3(context, p1, p2, p3);
                context.trampoline = old;
                return result.apply2(context, p4, p5);
            case 4:
                old = context.getAndSetTrampoline();
                result = apply4(context, p1, p2, p3, p4);
                context.trampoline = old;
                return result.apply1(context, p5);
            case 5:
                barf(this + ": Expected implementation for apply5.");
                return null;
            default:
                return new PAPSlow(this, arity - 5,
                                   ArgumentStack.createFromP(null, p1, p2, p3, p4, p5));
        }
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        int arity = arity();
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply1(context, p1);
                context.trampoline = old;
                return result.apply5(context, p2, p3, p4, p5, p6);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply2(context, p1, p2);
                context.trampoline = old;
                return result.apply4(context, p3, p4, p5, p6);
            case 3:
                old = context.getAndSetTrampoline();
                result = apply3(context, p1, p2, p3);
                context.trampoline = old;
                return result.apply3(context, p4, p5, p6);
            case 4:
                old = context.getAndSetTrampoline();
                result = apply4(context, p1, p2, p3, p4);
                context.trampoline = old;
                return result.apply2(context, p5, p6);
            case 5:
                old = context.getAndSetTrampoline();
                result = apply5(context, p1, p2, p3, p4, p5);
                context.trampoline = old;
                return result.apply1(context, p6);
            case 6:
                barf(this + ": Expected implementation for apply6.");
                return null;
            default:
                return new PAPSlow(this, arity - 6,
                                   ArgumentStack.createFromP(null, p1, p2, p3, p4, p5, p6));
        }
    }
}
