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

public class Exception {

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
            unmask = true;
        }
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        Closure result = io.applyV(context);
        if (unmask) {
            unmaskAsyncExceptionsRet(context, tso);
        } else if (maskUninterruptible) {
            tso.addFlags(TSO_BLOCKEX);
            tso.removeFlags(TSO_INTERRUPTIBLE);
        }
        return result;
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
            unmask = true;
        }
        tso.addFlags(TSO_BLOCKEX);
        tso.removeFlags(TSO_INTERRUPTIBLE);
        Closure result = io.applyV(context);
        if (unmask) {
            unmaskAsyncExceptionsRet(context, tso);
        } else if (mask) {
            tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        }
        return result;
    }

    public static Closure unmaskAsyncExceptions(StgContext context, Closure io) {
        Capability cap = context.myCapability;
        TSO tso = context.currentTSO;
        boolean mask;
        boolean maskUninterruptible;
        if (tso.hasFlag(TSO_BLOCKEX)) {
            if (tso.hasFlag(TSO_INTERRUPTIBLE)) {
                mask = true;
            } else {
                maskUninterruptible = true;
            }
            tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
            if (!tso.blockedExceptions.isEmpty()) {
                cap.maybePerformBlockedException(tso);
            }
        }
        Closure result = io.applyV(context);
        if (mask) {
            tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        } else if (maskUninterruptible) {
            tso.addFlags(TSO_BLOCKEX);
            tso.removeFlags(TSO_INTERRUPTIBLE);
        }
        return result;
    }

    public static Closure killThread(StgContext context, TSO target, Closure exception) {
        do {
            TSO tso = context.currentTSO;
            if (target == tso) {
                return killMyself(context, target, exception);
            } else {
                Capability cap = context.myCapability;
                MessageThrowTo msg = cap.throwTo(tso, target, exception);
                if (msg == null) {
                    return null;
                } else {
                    tso.whyBlocked = BlockedOnMsgThrowTo;
                    tso.blockInfo = msg;
                    cap.threadPaused(tso);
                    msg.tryUnlock();
                    do {
                        LockSupport.park();
                        if (Thread.interrupted()) {}
                        cap.blockedLoop(true);
                    } while (msg.isValid());
                    return null;
                    /* TODO: Is this the right condition? */
                }
            }
        } while (true);
    }

    public static Closure killMyself(StgContext context, TSO target, Closure exception) {
        Capability cap = context.myCapability;
        cap.throwToSingleThreaded(target, exception);
        return null;
    }

    public static Closure catch_(StgContext context, Closure io, Closure handler) {
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
            /* TODO: It seems that there should be more logic as this
                     discards the masking state before the catch.

                     Note that unmasking is not done for asynchronous exceptions.
                     This may be due to the fact that raiseAsync &
                     maybePeformBlockedExceptions only run after unmasking has
                     been set. Verify. -RM */
            if (!async && (exceptionsBlocked & TSO_BLOCKEX) == 0) {
                unmask = true;
            }
            tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
            if ((exceptionsBlocked & (TSO_BLOCKEX | TSO_INTERRUPTIBLE)) == TSO_BLOCKEX) {
                tso.removeFlags(TSO_INTERRUPTIBLE);
            }
            if (async) {
                /* TODO: How should we deal with update frames in async case?

                         GHC's behavior is to save the stack frame, which is impossible
                         for us. Maybe we should do something based on whether the
                         current tso owns the thunk? -RM */
                tso.whatNext = ThreadRun;
            } else {
                /* NOTE: This will replace all the thunks above this update frame
                         to point to a Raise(exception) thunk. */
                tso.updateInfoStack.raiseExceptionAfter(context.myCapability, tso, new Raise(exception), ui);
            }
            result = handler.applyPV(context, exception);
            if (unmask) {
                unmaskAsyncExceptionsRet(context, tso);
            }
        }
        return result;
    }

    public static Closure raise(StgContext context, Closure exception) {
        /* TODO: Remove the need for this line by using EtaException's stack
                 trace directly. */
        tso.setStackTrace(Thread.currentThread().getStackTrace());
        throw new EtaException(exception);
        return null;
    }

    /** Helper for the exception primops **/

    public static void unmaskAsyncExceptionsRet(StgContext context, TSO tso) {
        tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        if (!tso.blockedExceptions.isEmpty()) {
            context.myCapability.maybePerformBlockedException(tso);
        }
    }
}
