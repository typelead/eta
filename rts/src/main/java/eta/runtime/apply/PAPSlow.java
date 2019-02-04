package eta.runtime.apply;


import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.ArgumentStack;

import eta.runtime.Runtime;
import static eta.runtime.RuntimeLogging.*;
import static eta.runtime.stg.ArgumentStack.*;
import static eta.runtime.stg.Print.*;

public class PAPSlow extends PAP {
    public ArgumentStack argStack;

    public PAPSlow(Function fun, int arity, ArgumentStack argStack) {
        super(fun, arity);
        this.argStack = argStack;
    }

    public void setStack(ArgumentStack argStack) {
        this.argStack = argStack;
    }

    public Closure apply(StgContext context, ArgumentStack stack) {
        if (Runtime.debugPAPs()) {
            debugPAPs("Arity: " + arity + " ArgLens: " + stack.argLens());
        }
        int funArity = fun.arity();
        switch (stack.typeFlag) {
          case NONE:
              if (funArity == 1) {
                  return fun.applyV(context);
              }
              break;
          case P_FLAG:
              Closure[] closures = stack.closures;
              int pLen = closures.length;
              switch (funArity - pLen) {
                case 0:
                    switch (funArity) {
                      case 1:
                          return fun.apply1(context, closures[0]);
                      case 2:
                          return fun.apply2(context, closures[0], closures[1]);
                      case 3:
                          return fun.apply3(context, closures[0], closures[1],
                                            closures[2]);
                      case 4:
                          return fun.apply4(context, closures[0], closures[1],
                                            closures[2], closures[3]);
                      case 5:
                          return fun.apply5(context, closures[0], closures[1],
                                            closures[2], closures[3], closures[4]);
                      case 6:
                          return fun.apply6(context, closures[0], closures[1],
                                            closures[2], closures[3], closures[4],
                                            closures[5]);
                      default:
                          break;
                    }
                    break;
                case 1:
                    switch (pLen) {
                      case 1:
                          return fun.apply1V(context, closures[0]);
                      case 2:
                          return fun.apply2V(context, closures[0], closures[1]);
                      case 3:
                          return fun.apply3V(context, closures[0], closures[1],
                                            closures[2]);
                      default:
                          break;
                    }
                    break;
                default:
                    break;
              }
              break;
          case O_FLAG:
              if (funArity == 1) {
                  return fun.applyO(context, stack.objects[0]);
              }
              break;
          case I_FLAG:
              if (funArity == 1) {
                  return fun.applyN(context, stack.ints[0]);
              }
              break;
          case L_FLAG:
              if (funArity == 1) {
                  return fun.applyL(context, stack.longs[0]);
              }
              break;
          case F_FLAG:
              if (funArity == 1) {
                  return fun.applyF(context, stack.floats[0]);
              }
              break;
          case D_FLAG:
              if (funArity == 1) {
                  return fun.applyD(context, stack.doubles[0]);
              }
              break;
          default:
              context.merge(stack);
              return fun.enter(context);
        }
        context.merge(stack);
        return fun.enter(context);
    }

    @Override
    public Closure applyV(StgContext context) {
        if (arity == 1) {
            return apply(context, argStack);
        } else {
            return new PAPSlow(fun, arity - 1, argStack);
        }
    }

    @Override
    public Closure applyN(StgContext context, int n) {
        ArgumentStack stack = ArgumentStack.createFrom(argStack, n);
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAPSlow(fun, arity - 1, stack);
        }
    }

    @Override
    public Closure applyL(StgContext context, long l) {
        ArgumentStack stack = ArgumentStack.createFrom(argStack, l);
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAPSlow(fun, arity - 1, stack);
        }
    }

    @Override
    public Closure applyF(StgContext context, float f) {
        ArgumentStack stack = ArgumentStack.createFrom(argStack, f);
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAPSlow(fun, arity - 1, stack);
        }
    }

    @Override
    public Closure applyD(StgContext context, double d) {
        ArgumentStack stack = ArgumentStack.createFrom(argStack, d);
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAPSlow(fun, arity - 1, stack);
        }
    }

    @Override
    public Closure applyO(StgContext context, Object o) {
        ArgumentStack stack = ArgumentStack.createFrom(argStack, o);
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAPSlow(fun, arity - 1, stack);
        }
    }

    @Override
    public Closure apply1(StgContext context, Closure p) {
        ArgumentStack stack = ArgumentStack.createFromP(argStack, p);
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAPSlow(fun, arity - 1, stack);
        }
    }

    @Override
    public Closure apply1V(StgContext context, Closure p) {
        ArgumentStack stack = ArgumentStack.createFromP(argStack, p);
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply(context, stack);
                context.trampoline = old;
                return result.applyV(context);
            case 2:
                return apply(context, stack);
            default:
                return new PAPSlow(fun, arity - 2, stack);
        }
    }

    @Override
    public Closure apply2(StgContext context, Closure p1, Closure p2) {
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1));
                context.trampoline = old;
                return result.apply1(context, p2);
            case 2:
                return apply(context, ArgumentStack.createFromP(argStack, p1, p2));
            default:
                return new PAPSlow(fun, arity - 2,
                                   ArgumentStack.createFromP(argStack, p1, p2));
        }
    }

    @Override
    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1));
                context.trampoline = old;
                return result.apply1V(context, p2);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2));
                context.trampoline = old;
                return result.applyV(context);
            case 3:
                return apply(context, ArgumentStack.createFromP(argStack, p1, p2));
            default:
                return new PAPSlow(fun, arity - 3,
                                   ArgumentStack.createFromP(argStack, p1, p2));
        }
    }

    @Override
    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1));
                context.trampoline = old;
                return result.apply2(context, p2, p3);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2));
                context.trampoline = old;
                return result.apply1(context, p3);
            case 3:
                return apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3));
            default:
                return new PAPSlow(fun, arity - 3,
                                   ArgumentStack.createFromP(argStack, p1, p2, p3));
        }
    }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1));
                context.trampoline = old;
                return result.apply2V(context, p2, p3);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2));
                context.trampoline = old;
                return result.apply1V(context, p3);
            case 3:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3));
                context.trampoline = old;
                return result.applyV(context);
            case 4:
                return apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3));
            default:
                return new PAPSlow(fun, arity - 4,
                                   ArgumentStack.createFromP(argStack, p1, p2, p3));
        }
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1));
                context.trampoline = old;
                return result.apply3(context, p2, p3, p4);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2));
                context.trampoline = old;
                return result.apply2(context, p3, p4);
            case 3:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3));
                context.trampoline = old;
                return result.apply1(context, p4);
            case 4:
                return apply(context,
                             ArgumentStack.createFromP(argStack, p1, p2, p3, p4));
            default:
                return new PAPSlow(fun, arity - 4,
                                   ArgumentStack.createFromP(argStack, p1, p2, p3, p4));
        }
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1));
                context.trampoline = old;
                return result.apply4(context, p2, p3, p4, p5);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2));
                context.trampoline = old;
                return result.apply3(context, p3, p4, p5);
            case 3:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3));
                context.trampoline = old;
                return result.apply2(context, p4, p5);
            case 4:
                old = context.getAndSetTrampoline();
                result =
                    apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3, p4));
                context.trampoline = old;
                return result.apply1(context, p5);
            case 5:
                return apply(context,
                             ArgumentStack.createFromP(argStack, p1, p2, p3, p4, p5));
            default:
                return new PAPSlow(fun, arity - 5,
                                   ArgumentStack.createFromP(argStack, p1, p2, p3, p4, p5));
        }
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        boolean old;
        Closure result;
        switch (arity) {
            case 1:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1));
                context.trampoline = old;
                return result.apply5(context, p2, p3, p4, p5, p6);
            case 2:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2));
                context.trampoline = old;
                return result.apply4(context, p3, p4, p5, p6);
            case 3:
                old = context.getAndSetTrampoline();
                result = apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3));
                context.trampoline = old;
                return result.apply3(context, p4, p5, p6);
            case 4:
                old = context.getAndSetTrampoline();
                result =
                    apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3, p4));
                context.trampoline = old;
                return result.apply2(context, p5, p6);
            case 5:
                old = context.getAndSetTrampoline();
                result =
                    apply(context, ArgumentStack.createFromP(argStack, p1, p2, p3, p4, p5));
                context.trampoline = old;
                return result.apply1(context, p6);
            case 6:
                return apply(context,
                             ArgumentStack.createFromP(argStack, p1, p2, p3, p4, p5, p6));
            default:
                return new PAPSlow(fun, arity - 6,
                                   ArgumentStack.createFromP(argStack, p1, p2, p3, p4, p5, p6));
        }
    }

    @Override
    public void writeArgs(Object pending, PrintState ps) {
        argStack.writeArgs(pending, ps);
    }
}
