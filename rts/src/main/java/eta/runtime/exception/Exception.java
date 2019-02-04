package eta.runtime.exception;

import java.io.PrintWriter;
import java.io.StringWriter;

import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.Thunk;
import eta.runtime.thunk.UpdateInfo;

import eta.runtime.Runtime;
import eta.runtime.message.MessageBlackHole;
import eta.runtime.message.MessageThrowTo;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.RuntimeLogging.*;
import static eta.runtime.stg.TSO.*;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.TSO.WhyBlocked.*;

public class Exception {

    public static int getMaskingState(StgContext context) {
        TSO tso = context.currentTSO;
        return (tso.hasFlag(TSO_BLOCKEX)? 1: 0) +
               (tso.hasFlag(TSO_INTERRUPTIBLE)? 1: 0);
    }

    public static Closure maskAsyncExceptions(StgContext context, Closure io) {
        TSO tso = context.currentTSO;
        boolean unmask = false;
        boolean maskUninterruptible = false;
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
        boolean unmask = false;
        boolean mask   = false;
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
        boolean mask = false;
        boolean maskUninterruptible = false;
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

    public static void killThread(StgContext context, TSO target, Closure exception) {
        final TSO tso = context.currentTSO;
        if (target == tso) {
            killMyself(context, target, exception);
        } else {
            final Capability cap = context.myCapability;
            final MessageThrowTo msg = throwTo(cap, tso, target, exception);
            if (msg != null) {
                tso.whyBlocked = BlockedOnMsgThrowTo;
                tso.blockInfo = msg;
                msg.tryUnlock();
                do {
                    cap.blockedLoop();
                } while (msg.isValid());
            }
        }
    }

    public static void killMyself(StgContext context, TSO target, Closure exception) {
        throwToSingleThreaded(target, exception);
    }

    public static Closure catch_(StgContext context, Closure io, Closure handler) {
        final TSO tso = context.currentTSO;
        final int exceptionsBlocked = tso.showIfFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        final UpdateInfo ui = tso.updateInfoStack.peek();
        Closure result;
        try {
            result = io.applyV(context);
        } catch (java.lang.Exception e) {
            context.raise = null;
            boolean unmask = false;
            Closure exception = null;
            boolean async = e instanceof EtaAsyncException;
            if (async) {
                exception = ((EtaAsyncException) e).exception;
            } else if (e instanceof EtaException) {
                exception = ((EtaException) e).exception;
            } else if (e instanceof StgException) {
                throw e;
            } else {
                exception = EtaException.convertJavaException(tso, e);
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
            }
            result = handler.apply1V(context, exception);
            if (unmask) {
                unmaskAsyncExceptionsRet(context, tso);
            }
            tso.resetStack();
        }
        return result;
    }

    public static Closure raise(StgContext context, Closure exception) {
        EtaException e = EtaException.create(context, exception);
        if (Runtime.debugExceptionsVerbose()) {
            debugExceptions(exceptionToString(e));
        }
        throw e;
    }

    /** Helper for the exception primops **/

    public static void unmaskAsyncExceptionsRet(StgContext context, TSO tso) {
        tso.removeFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        if (!tso.blockedExceptions.isEmpty()) {
            context.myCapability.maybePerformBlockedException(tso);
        }
    }

    public static void throwToSingleThreaded(TSO tso, Closure exception) {
        throwToSingleThreaded(tso, exception, false, null);
    }

    public static void throwToSingleThreaded(TSO tso, Closure exception, boolean stopAtAtomically) {
        throwToSingleThreaded(tso, exception, stopAtAtomically, null);
    }

    public static void throwToSingleThreaded(TSO tso, Closure exception, boolean stopAtAtomically, Thunk stopHere) {
        if (tso.whatNext == ThreadComplete || tso.whatNext == ThreadKilled) {
            return;
        }
        tso.removeFromQueues();
        raiseAsync(tso, exception, stopAtAtomically, stopHere);
    }

    public static void suspendComputation(TSO tso, Thunk stopHere) {
        throwToSingleThreaded(tso, null, false, stopHere);
    }

    public static void raiseAsync(TSO tso, Closure exception, boolean stopAtAtomically, Thunk stopHere) {
        assert tso.whatNext != ThreadComplete && tso.whatNext != ThreadKilled;

        if (tso.whyBlocked != NotBlocked) {
            tso.whyBlocked = NotBlocked;
        }
        throw new EtaAsyncException(exception, stopAtAtomically, stopHere);
    }

    public static MessageThrowTo throwTo(Capability cap, TSO source, TSO target, Closure exception) {
        MessageThrowTo msg = new MessageThrowTo(source, target, exception);
        msg.lock();
        boolean success = throwToMsg(cap, msg, false);
        if (success) {
            msg.done();
            return null;
        } else {
            return msg;
        }
    }

    public static boolean throwToMsg(Capability cap, MessageThrowTo msg, boolean wakeupSource) {
        final TSO target = msg.target;
        final boolean debug = Runtime.debugAsyncExceptions();
        do {
            assert target != null;
            if (target.whatNext == ThreadComplete || target.whatNext == ThreadKilled) {
                if (debug) {
                    debugExceptions("throwToMsg: " + msg.source + " to " + target +
                                    " is inactive with " + target.whatNext);
                }
                return true;
            }
            final Capability targetCap = target.cap;
            if (targetCap != cap) {
                if (targetCap == null) {
                    if (debug) {
                        debugExceptions(target + " is idle, killing");
                    }
                    target.whatNext = ThreadKilled;
                    return true;
                } else {
                    if (debug) {
                        debugExceptions("throwToMsg: Throwing asynchronous exception from "
                                      + msg.source + " to " + target);
                    }
                    if (Runtime.debugAsyncExceptionsVerbose()) {
                        debugExceptions(exceptionToString(new java.lang.Exception()));
                    }
                    cap.sendMessage(targetCap, msg);
                    return false;
                }
            } else {
                if (debug) {
                    debugExceptions("throwToMsg: " + target + " " + target.whyBlocked +
                                    " receiving asynchronous exception from " + msg.source);
                }
            }
            switch (target.whyBlocked) {
                case NotBlocked:
                    if (target.hasFlag(TSO_BLOCKEX)) {
                        target.blockedThrowTo(msg);
                        return false;
                    }
                    break;
                case BlockedOnMsgThrowTo:
                    MessageThrowTo msg2 = (MessageThrowTo) target.blockInfo;
                    if (msg2.hashCode() < msg.hashCode()) {
                        msg2.lock();
                    } else {
                        if (!msg2.tryLock()) {
                            cap.sendMessage(targetCap, msg);
                            return false;
                        }
                    }
                    if (!msg2.isValid()) {
                        msg.unlock();
                        cap.tryWakeupThread(target);
                        continue;
                    }
                    if (target.hasFlag(TSO_BLOCKEX) && !target.hasFlag(TSO_INTERRUPTIBLE)) {
                        msg.unlock();
                        target.blockedThrowTo(msg);
                        return false;
                    }
                    msg2.unlock();
                    break;
               case BlockedOnBlackHole:
                   if (target.hasFlag(TSO_BLOCKEX)) {
                       target.blockedThrowTo(msg);
                       return false;
                   }
                   assert target.blockInfo instanceof MessageBlackHole;
                   ((MessageBlackHole) target.blockInfo).invalidate();
                   break;
               case BlockedOnSTM:
                   target.lock();
                   if (target.whyBlocked != BlockedOnSTM) {
                       target.unlock();
                       continue;
                   } else {
                       if (target.hasFlag(TSO_BLOCKEX)
                           && !target.hasFlag(TSO_INTERRUPTIBLE)) {
                           target.blockedThrowTo(msg);
                           target.unlock();
                           return false;
                       } else {
                           target.unlock();
                       }
                       break;
                   }
               case BlockedOnYield:
               case BlockedOnRead:
               case BlockedOnWrite:
               case BlockedOnDelay:
               case BlockedOnMVar:
               case BlockedOnMVarRead:
               case BlockedOnJavaCall:
               case BlockedOnJavaCall_Interruptible:
                   if (target.hasFlag(TSO_BLOCKEX) && !target.hasFlag(TSO_INTERRUPTIBLE)) {
                       target.blockedThrowTo(msg);
                       return false;
                   }
                   break;
               default:
                   barf("Unimplemented throwTo(): " + target.whyBlocked);
            }
            break;
        } while (true);
        if (wakeupSource) {
            final TSO source = msg.source;
            msg.done();
            cap.tryWakeupThread(source);
        }
        raiseAsync(target, msg.exception, false, null);
        return true;
    }

    public static String exceptionToString(final Throwable e) {
        final StringWriter sw = new StringWriter();
        e.printStackTrace(new PrintWriter(sw));
        return sw.toString();
    }

    public static Throwable normalize(Throwable e) {
        Throwable f = e;
        Throwable d = null;
        while (e != null) {
            d = e;
            e = e.getCause();
        }
        if (!(d instanceof StgException)) {
            return d;
        } else  {
            return f;
        }
    }
}
