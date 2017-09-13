package eta.runtime.exception;

import java.util.Arrays;
import java.util.ListIterator;

import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.UpdateInfo;

import eta.runtime.message.MessageBlackHole;
import eta.runtime.message.MessageThrowTo;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.RuntimeLogging.debugScheduler;
import static eta.runtime.stg.TSO.*;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.TSO.WhyBlocked.*;

public class Exception {

    public static Closure getMaskingState(StgContext context) {
        TSO tso = context.currentTSO;
        context.I(1, ((tso.hasFlag(TSO_BLOCKEX)? 1: 0) +
                      (tso.hasFlag(TSO_INTERRUPTIBLE)? 1: 0)));
        return null;
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

    public static Closure killThread(StgContext context, TSO target, Closure exception) {
        do {
            TSO tso = context.currentTSO;
            if (target == tso) {
                return killMyself(context, target, exception);
            } else {
                Capability cap = context.myCapability;
                MessageThrowTo msg = throwTo(cap, tso, target, exception);
                if (msg == null) {
                    return null;
                } else {
                    tso.whyBlocked = BlockedOnMsgThrowTo;
                    tso.blockInfo = msg;
                    msg.tryUnlock();
                    do {
                        cap.blockedLoop();
                    } while (msg.isValid());
                    return null;
                }
            }
        } while (true);
    }

    public static Closure killMyself(StgContext context, TSO target, Closure exception) {
        Capability cap = context.myCapability;
        throwToSingleThreaded(target, exception);
        return null;
    }

    public static Closure catch_(StgContext context, Closure io, Closure handler) {
        TSO tso = context.currentTSO;
        int exceptionsBlocked = tso.showIfFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        UpdateInfo ui = tso.updateInfoStack.peek();
        Closure result;
        try {
            result = io.applyV(context);
        } catch (java.lang.Exception e) {
            boolean unmask = false;
            Closure exception = null;
            boolean async = e instanceof EtaAsyncException;
            if (async) {
                exception = ((EtaAsyncException) e).exception;
            } else if (e instanceof EtaException) {
                exception = ((EtaException) e).exception;
            } else {
                exception = convertJavaException(tso, e);
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
            tso.resetStack();
        }
        return result;
    }

    public static Closure raise(StgContext context, Closure exception) {
        EtaException e = new EtaException(exception);
        StackTraceElement[] original = Thread.currentThread().getStackTrace();
        context.saveStackTrace(e, Arrays.copyOfRange(original, 2, original.length));
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

    public static void throwToSingleThreaded(TSO tso, Closure exception, boolean stopAtAtomically, UpdateInfo stopHere) {
        if (tso.whatNext == ThreadComplete || tso.whatNext == ThreadKilled) {
            return;
        }
        tso.removeFromQueues();
        raiseAsync(tso, exception, stopAtAtomically, stopHere);
    }

    public static void suspendComputation(TSO tso, UpdateInfo stopHere) {
        throwToSingleThreaded(tso, null, false, stopHere);
    }

    public static void raiseAsync(TSO tso, Closure exception, boolean stopAtAtomically, UpdateInfo stopHere) {
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
            msg.unlock();
            return null;
        } else {
            return msg;
        }
    }

    public static boolean throwToMsg(Capability cap, MessageThrowTo msg, boolean wakeupSource) {
        TSO target = msg.target;
        do {
            assert target != null;
            if (target.whatNext == ThreadComplete
                || target.whatNext == ThreadKilled) {
                return true;
            }
            debugScheduler("Throwing asynchronous exception from TSO %d to TSO %d.", msg.source.id, msg.target.id);
            Capability targetCap = target.cap;
            if (target.cap != cap) {
                debugScheduler("Sending a ThrowTo message to Capability[%d].", targetCap.id);
                cap.sendMessage(targetCap, msg);
                return false;
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
                    if (msg2.id < msg.id) {
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
                    break;
               case BlockedOnMVar:
               case BlockedOnMVarRead:
                   /* TODO: Figure out MVar story */
                   barf("Unimplemented MVar");
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
               case BlockedOnRead:
               case BlockedOnWrite:
               case BlockedOnDelay:
                   barf("Unimplemented IO manager");
                   break;
               default:
                   barf("Unimplemented throwTo()");
            }
            break;
        } while (true);
        if (wakeupSource) {
            TSO source = msg.source;
            msg.done();
            cap.tryWakeupThread(source);
        }
        Exception.raiseAsync(target, msg.exception, false, null);
        return true;
    }

    public static Closure convertJavaException(TSO tso, java.lang.Exception e) {
        tso.saveStack(e, e.getStackTrace());
        return Closures.mkSomeException(e);
    }

    public static EtaException toEtaException(TSO tso, java.lang.Exception e) {
        return new EtaException(convertJavaException(tso, e));
    }
}
