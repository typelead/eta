package ghcvm.runtime.exception;

import ghcvm.runtime.stg.StgClosure;

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

    public static StgClosure getMaskingState = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                context.I1 = ((tso.hasFlag(TSO_BLOCKEX)? 1: 0) +
                              (tso.hasFlag(TSO_INTERRUPTIBLE)? 1: 0));
            }
        };

    public static StgClosure maskAsyncExceptions = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                ListIterator<StackFrame> sp = tso.sp;
                if (tso.hasFlag(TSO_BLOCKEX)) {
                    if (!tso.hasFlag(TSO_INTERRUPTIBLE)) {
                        sp.add(new MaskUninterruptableFrame());
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
                /* TODO: Ensure that R1 is preserved */
                Apply.ap_v_fast.enter(context);
            }
        };

    public static StgClosure maskUninterruptible = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                ListIterator<StackFrame> sp = tso.sp;
                if (tso.hasFlag(TSO_BLOCKEX)) {
                    if (tso.hasFlag(TSO_INTERRUPTIBLE)) {
                        sp.add(new MaskAsyncExceptionsFrame());
                    }
                } else {
                    StackFrame top = tso.stack.peek();
                    if (top.getClass() == MaskUninterruptableFrame.class) {
                        sp.previous();
                        sp.remove();
                    } else {
                        sp.add(new UnmaskAsyncExceptionsFrame());
                    }
                }
                tso.addFlags(TSO_BLOCKEX);
                tso.removeFlags(TSO_INTERRUPTIBLE);
                /* TODO: Ensure that R1 is preserved */
                Apply.ap_v_fast.enter(context);
            }
        };

    public static StgClosure unmaskAsyncExceptions = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                StgClosure io = context.R1;
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
                            sp.add(new MaskUninterruptableFrame());
                        }
                    }
                    tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
                    if (!tso.blockedExceptions.isEmpty()) {
                        sp.add(new ApV());
                        sp.add(new StgEnter(io));
                        boolean performed = cap.maybePerformBlockedException(tso);
                        if (performed) {
                            if (tso.whatNext == ThreadKilled) {
                                Stg.threadFinished.enter(context);
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
                Apply.ap_v_fast.enter(context);
            }
        };

    public static StgClosure killThread = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure target = context.R1;
                StgTSO tso = context.currentTSO;
                if (target == tso) {
                    killMyself.enter(context);
                } else {
                    StgClosure exception = context.R2;
                    Capability cap = context.myCapability;
                    MessageThrowTo msg = cap.throwTO(tso, target, exception);
                    if (msg == null) {
                        return;
                    } else {
                        tso.whyBlocked = BlockedOnMsgThrowTo;
                        tso.blockInfo = msg;
                        block_throwto.enter(context);
                    }
                }
            }
        };

    public static StgClosure killMyself = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure target = context.R1;
                StgClosure exception = context.R2;
                Capability cap = context.myCapability;
                cap.throwToSingleThreaded(target, exception);
                StgTSO tso = context.currentTSO;
                if (tso.whatNext == ThreadKilled) {
                    Stg.threadFinished.enter(context);
                } else {
                    throw StgException.stackReloadException;
                }
            }
        };

    public static StgClosure catch_ = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgClosure handler = context.R2;
                StgTSO tso = context.currentTSO;
                ListIterator<StackFrame> sp = tso.sp;
                int exceptionsBlocked = tso.showIfFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
                sp.add(new StgCatchFrame(exceptionsBlocked, handler));
                Apply.ap_v_fast.enter(context);
            }
        };


    public static StgClosure raise = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                StgClosure exception = context.R1;
                do {
                    cap.raiseExceptionHelper(tso, exception);
                    /* TODO: Finish implementation */
                } while (false);
            }
        };

    public static StgClosure raiseIO = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                raise.enter(context);
            }
        };
}
