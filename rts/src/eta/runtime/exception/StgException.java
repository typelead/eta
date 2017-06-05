package eta.runtime.exception;

import java.util.ListIterator;

import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;

import eta.runtime.apply.ApV;
import eta.runtime.message.MessageThrowTo;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.stg.StgTSO.TSO_BLOCKEX;
import static eta.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgTSO.WhyBlocked.BlockedOnMsgThrowTo;

public class StgException extends RuntimeException {

    @Override
    public Throwable fillInStackTrace() {
        return null;
    }

    /* TODO: Should this be volatile/atomic? */
    public static boolean noBreakOnException = false;

    public static StgException stgReturnException = new StgReturnException();
    public static StgException threadYieldException = new ThreadYieldException();
    public static StgException stackReloadException = new StackReloadException();

    public static void getMaskingState(StgContext context) {
        StgTSO tso = context.currentTSO;
        context.I(1, ((tso.hasFlag(TSO_BLOCKEX)? 1: 0) +
                      (tso.hasFlag(TSO_INTERRUPTIBLE)? 1: 0)));
    }

    public static void maskAsyncExceptions(StgContext context, StgClosure io) {
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        if (tso.hasFlag(TSO_BLOCKEX)) {
            if (!tso.hasFlag(TSO_INTERRUPTIBLE)) {
                sp.add(new MaskUninterruptibleFrame());
            }
        } else {
            StackFrame top = tso.stack.peek();
            if (top.getClass() == MaskAsyncExceptionsFrame.class) {
                sp.previous();
                sp.remove();
            } else {
                sp.add(new UnmaskAsyncExceptionsFrame());
            }
        }
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        io.applyV(context);
    }

    public static void maskUninterruptible(StgContext context, StgClosure io) {
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        if (tso.hasFlag(TSO_BLOCKEX)) {
            if (tso.hasFlag(TSO_INTERRUPTIBLE)) {
                sp.add(new MaskAsyncExceptionsFrame());
            }
        } else {
            StackFrame top = tso.stack.peek();
            if (top.getClass() == MaskUninterruptibleFrame.class) {
                sp.previous();
                sp.remove();
            } else {
                sp.add(new UnmaskAsyncExceptionsFrame());
            }
        }
        tso.addFlags(TSO_BLOCKEX);
        tso.removeFlags(TSO_INTERRUPTIBLE);
        /* TODO: Ensure that R1 is preserved */
        context.R(1).applyV(context);
    }

    public static void unmaskAsyncExceptions(StgContext context, StgClosure io) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        if (tso.hasFlag(TSO_BLOCKEX)) {
            StackFrame top = tso.stack.peek();
            if (top.getClass() == UnmaskAsyncExceptionsFrame.class) {
                sp.previous();
                sp.remove();
            } else {
                if (tso.hasFlag(TSO_INTERRUPTIBLE)) {
                    sp.add(new MaskAsyncExceptionsFrame());
                } else {
                    sp.add(new MaskUninterruptibleFrame());
                }
            }
            tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
            if (!tso.blockedExceptions.isEmpty()) {
                sp.add(new ApV());
                sp.add(new StgEnter(io));
                boolean performed = cap.maybePerformBlockedException(tso);
                if (performed) {
                    if (tso.whatNext == ThreadKilled) {
                        Stg.threadFinished(context);
                    } else {
                        /* TODO: Verify R1 is conserved on the next
                            stack reload. */
                        throw StgException.stackReloadException;
                    }
                } else {
                    sp.previous();
                    sp.remove();
                    sp.previous();
                    sp.remove();
                }
            }
        }
        io.applyV(context);
    }

    public static void killThread(StgContext context, StgTSO target, StgClosure exception) {
            StgTSO tso = context.currentTSO;
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

    public static void killMyself(StgContext context, StgTSO target, StgClosure exception) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        cap.throwToSingleThreaded(target, exception);
        if (tso.whatNext == ThreadKilled) {
            Stg.threadFinished(context);
        } else {
            throw StgException.stackReloadException;
        }
    }

    public static void catch_(StgContext context, StgClosure io, StgClosure handler) {
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        int exceptionsBlocked = tso.showIfFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        sp.add(new StgCatchFrame(exceptionsBlocked, handler));
        io.applyV(context);
    }

    public static void raise(StgContext context, StgClosure exception) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        boolean retry = false;
        do {
            StackFrame frame = cap.raiseExceptionHelper(tso, exception);
            retry = frame.doRaise(context, cap, tso, exception);
        } while (retry);
    }
}
