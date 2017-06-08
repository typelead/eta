package eta.runtime.exception;

import java.util.ListIterator;

import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

import eta.runtime.apply.ApV;
import eta.runtime.message.MessageThrowTo;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.stg.TSO.TSO_BLOCKEX;
import static eta.runtime.stg.TSO.TSO_INTERRUPTIBLE;
import static eta.runtime.stg.TSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.TSO.WhyBlocked.BlockedOnMsgThrowTo;

public class StgException extends RuntimeException {

    /* TODO: Should this be volatile/atomic? */
    public static boolean noBreakOnException = false;

    public static StgException stgReturnException = new StgReturnException();
    public static StgException threadYieldException = new ThreadYieldException();
    public static StgException stackReloadException = new StackReloadException();

    public static Closure getMaskingState(StgContext context) {
        TSO tso = context.currentTSO;
        context.I(1, ((tso.hasFlag(TSO_BLOCKEX)? 1: 0) +
                      (tso.hasFlag(TSO_INTERRUPTIBLE)? 1: 0)));
        return null;
    }

    public static Closure maskAsyncExceptions(StgContext context, Closure io) {
        TSO tso = context.currentTSO;
        boolean unmask;
        boolean maskUninterruptible;
        if (tso.hasFlag(TSO_BLOCKEX)) {
            if (!tso.hasFlag(TSO_INTERRUPTIBLE)) {
                maskUninterruptible = true;
            }
        } else {
            /* TODO: Check if maskAsyncExceptionsContinuation is expected
               , if so optimize the stack to avoid unmasks multiple times */
            unmask = true;
        }
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        TSO result = io.applyV(context);
        if (maskUninterruptible) {
            barf("Unimplemented maskUninterruptible");
        } else if (unmask) {
            barf("Unimplemented unmask");
        } else {
            return result;
        }
    }

    public static Closure maskUninterruptible(StgContext context, Closure io) {
        TSO tso = context.currentTSO;
        boolean unmask;
        boolean mask;
        if (tso.hasFlag(TSO_BLOCKEX)) {
            if (tso.hasFlag(TSO_INTERRUPTIBLE)) {
                mask = true;
            }
        } else {
            /* TODO: Check if maskUninterruptibleContinuation is expected
               , if so optimize the stack to avoid unmasks multiple times */
            unmask = true;
        }
        tso.addFlags(TSO_BLOCKEX);
        tso.removeFlags(TSO_INTERRUPTIBLE);
        Closure result = io.applyV(context);
        if (mask) {
            barf("Mask");
        } else if (unmask) {
            barf("UnMask");
        } else {
            return result;
        }
    }

    public static Closure unmaskAsyncExceptions(StgContext context, Closure io) {
        Capability cap = context.myCapability;
        TSO tso = context.currentTSO;
        boolean mask;
        boolean maskUninterruptible;
        if (tso.hasFlag(TSO_BLOCKEX)) {
            /* TODO: Check if unmaskContinuation is expected
               , if so optimize the stack to avoid unmasks multiple times */
            if (tso.hasFlag(TSO_INTERRUPTIBLE)) {
                mask = true;
            } else {
                maskUninterruptible = true;
            }
            tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
            if (!tso.blockedExceptions.isEmpty()) {
                /* Preserve io as continuation (first priority) */
                boolean performed = cap.maybePerformBlockedException(tso);
                if (performed) {
                    if (tso.whatNext == ThreadKilled) {
                        Stg.threadFinished(context);
                    } else {
                        barf("Unimplemeneted unmask 1");
                    }
                } else {
                    barf("Unimplemeneted unmask 2");
                }
            }
        }
        Closure result = io.applyV(context);
        if (mask) {
            barf("Unimplemeneted unmask 3");
        } else if (maskUninterruptible) {
            barf("Unimplemeneted unmask 4");
        } else return result;
    }

    public static void killThread(StgContext context, TSO target, Closure exception) {
            TSO tso = context.currentTSO;
            if (target == tso) {
                killMyself(context, target, exception);
            } else {
                Capability cap = context.myCapability;
                MessageThrowTo msg = cap.throwTo(tso, target, exception);
                if (msg == null) {
                    return;
                } else {
                    tso.whyBlocked = BlockedOnMsgThrowTo;
                    tso.blockInfo = msg;
                    // block_throwto.enter(context);
                    barf("killThread#: unimplemented RTS primitive operation.");
                }
            }
    }

    public static StgClosure killMyself(StgContext context, TSO target, Closure exception) {
        Capability cap = context.myCapability;
        TSO tso = context.currentTSO;
        cap.throwToSingleThreaded(target, exception);
        if (tso.whatNext == ThreadKilled) {
            Stg.threadFinished(context);
        } else {
            throw StgException.stackReloadException;
        }
    }

    public static StgClosure catch_(StgContext context, Closure io, Closure handler) {
        TSO tso = context.currentTSO;
        int exceptionsBlocked = tso.showIfFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        UpdateInfo ui = tso.updateInfoStack.peek();
        Closure result;
        try {
            result = io.applyV(context);
        } catch (Exception e) {
            boolean unmask;
            Closure exception;
            boolean async = e instanceof EtaAsyncException;
            if (async) {
                exception = ((EtaAsyncException) e).exception;
            } else if (e instanceof EtaException) {
                exception = ((EtaException) e).exception;
            } else {
                barf("Implement catching Java exceptions.");
            }
            if ((exceptionsBlocked & TSO_BLOCKEX) == 0) {
                unmask = true;
            }
            tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
            if ((exceptionsBlocked & TSO_INTERRUPTIBLE) == 0) {
                tso.removeFlags(TSO_INTERRUPTIBLE);
            } else {
                tso.addFlags(TSO_INTERRUPTIBLE);
            }
            if (async) {
                /* TODO: How should we deal with update frames */
                tso.whatNext = ThreadRun;
            } else {
                /* Deal with update frames above this one */
                tso.updateInfoStack.raiseExceptionAfter(context.myCapability, tso,
                                                        new StgRaise(exception), ui);
            }
            result = handler.applyPV(context, exception);
            if (unmask) {
                barf("Implement UnmaskAsyncExceptionsFrame");
                //result = unmaskAsynExceptionsFrameCode();
                // tso.spPush(new UnmaskAsyncExceptionsFrame());
            }
        }
        return result;
    }

    public static StgClosure raise(StgContext context, Closure exception) {
        /* TODO: Remove the need for this line by using EtaException directly. */
        tso.setStackTrace(Thread.currentThread().getStackTrace());
        throw new EtaException(exception);
        return null;
    }
}
