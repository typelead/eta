package eta.runtime.concurrent;

import java.io.IOException;
import java.util.Map;
import java.util.Deque;
import java.util.Iterator;
import java.util.concurrent.Future;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.nio.channels.Channel;
import java.nio.channels.Selector;
import java.nio.channels.SelectionKey;
import java.nio.channels.SelectableChannel;
import java.nio.channels.ClosedChannelException;

import eta.runtime.Runtime;
import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.exception.Exception;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.stg.TSO.*;
import static eta.runtime.stg.TSO.WhyBlocked;
import static eta.runtime.stg.TSO.WhatNext;
import static eta.runtime.stg.TSO.WhyBlocked.*;
import static eta.runtime.stg.TSO.WhatNext.*;

public class Concurrent {
    public static final int SPIN_COUNT = 1000;

    /* Global Run Queue */

    public static final Deque<TSO> globalRunQueue = new ConcurrentLinkedDeque<TSO>();
    public static long globalRunQueueModifiedTime = 0;

    public static void pushToGlobalRunQueue(TSO tso) {
        assert tso.cap == null;
        globalRunQueue.offerFirst(tso);
    }

    public static boolean emptyGlobalRunQueue() {
        return globalRunQueue.isEmpty();
    }

    /* MVar Operations */

    public static Closure takeMVar(StgContext context, MVar mvar) {
        Capability cap = context.myCapability;
        Closure val = mvar.tryTake();
        while (val == null) {
            cap.blockedLoop();
            val = mvar.tryTake();
        }
        return val;
    }

    public static Closure readMVar(StgContext context, MVar mvar) {
        return mvar.read();
    }

    public static Closure putMVar(StgContext context, MVar mvar, Closure val) {
        Capability cap = context.myCapability;
        boolean success = mvar.tryPut(val);
        while (!success) {
            cap.blockedLoop();
            success = mvar.tryPut(val);
        }
        return null;
    }

    public static Closure tryReadMVar(StgContext context, MVar mvar) {
        Closure value = mvar.tryRead();
        context.I(1, (value == null)? 0: 1);
        return value;
    }

    public static Closure fork(StgContext context, Closure closure) {
        Capability cap = context.myCapability;
        TSO currentTSO = context.currentTSO;
        TSO tso = Runtime.createIOThread(closure);
        tso.addFlags(currentTSO.andFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE));
        pushToGlobalRunQueue(tso);
        cap.idleLoop(false);
        context.O(1, tso);
        return null;
    }

    /* TODO: The scheduling policy in Eta is for TSOs to get grabbed by Capabilities
             and keep executing them to completion which doesn't happen THAT
             frequently.

             Hence, it makes no sense to use `forkOn` in Eta since threads will
             be bound to a given thread anyways. If you put multiple threads on
             a single Capability, be warned that one of the threads may never run!
     */
    public static Closure forkOn(StgContext context, int cpu, Closure closure) {
        return fork(context, closure);
    }

    public static Closure yield(StgContext context) {
        Capability cap = context.myCapability;
        TSO tso        = context.currentTSO;
        tso.whyBlocked = BlockedOnYield;
        tso.blockInfo  = null;
        cap.blockedLoop();
        return null;
    }

    /* In Eta, all the threads are bound, so this always returns true. */
    public static Closure isCurrentThreadBound(StgContext context) {
        context.I(1, 1);
        return null;
    }

    public static Closure threadStatus(StgContext context, TSO tso) {
        WhatNext whatNext = tso.whatNext;
        int ret;
        WhyBlocked whyBlocked = tso.whyBlocked;
        if (whatNext == ThreadComplete) {
            ret = 16;
        } else {
            if (whatNext == ThreadKilled) {
                ret = 17;
            } else {
                ret = whyBlocked.getVal();
            }
        }
        int cap = tso.cap.id;
        /* NOTE: The Eta RTS doesn't have a concept of TSO locking. It hurts more
                 than helps performance due to the fact that we can't presently
                 save/restore the Java stack. */
        int locked = 0;
        context.I(1, ret);
        context.I(2, cap);
        context.I(3, locked);
        return null;
    }

    /* TODO: Implement this */
    public static Closure traceEvent(StgContext context) {
        return null;
    }

    /* Managing Java Futures */

    public static final Map<Future, TSO> futureMap
        = new ConcurrentHashMap<Future, TSO>();

    public static final AtomicBoolean futureMapLock = new AtomicBoolean();

    public static final class FutureBlockResult {
        public Object    result;
        public Exception exception;
        public FutureBlockResult(Object result, Exception e) {
            this.result    = result;
            this.exception = exception;
        }
    }

    public static void checkForCompletedFutures(Capability cap) {
        /* Only one thread at a time should check the futures. */
        if (futureMapLock.compareAndSet(false, true)) {
            try {
                Iterator<Map.Entry<Future, TSO>> it = futureMap.entrySet().iterator();
                while (it.hasNext()) {
                    Map.Entry<Future, TSO> entry  = it.next();
                    Future                 future = entry.getKey();
                    TSO                    tso    = entry.getValue();
                    if (future.isDone()) {
                        it.remove();
                        Object    result    = null;
                        java.lang.Exception exception = null;
                        do {
                            try {
                                result    = future.get();
                            } catch (CancellationException e) {
                                exception = e;
                            } catch (ExecutionException e) {
                                exception = e;
                            } catch (InterruptedException e) {
                                /* TODO: Is this the right behavior? */
                                continue;
                            }
                            break;
                        } while (true);
                        tso.blockInfo = new FutureResult(result, exception);
                        cap.tryWakeupThread(tso);
                    }
                }
            } finally {
                futureMapLock.set(false);
            }
        }
    }

    public static Closure threadWaitFuture(StgContext context, Future future) {
        Capability cap = context.myCapability;
        TSO tso        = context.currentTSO;
        tso.whyBlocked = BlockedOnFuture;
        tso.blockInfo  = future;
        do {
            if (futureMap.get(future) == null) {
                futureMap.put(future, tso);
            }
            cap.blockedLoop();
        } while (!future.isDone());
        Object exception = null;
        Object result    = null;
        if (tso.blockInfo != null) {
            FutureResult futureResult = (FutureResult) tso.blockInfo;
            exception = futureResult.exception;
            result    = futureResult.result;
            tso.blockInfo = null;
        }
        context.O(1, exception);
        context.O(2, result);
        return null;
    }

    /* Managing Scalable I/O */

    public static Selector globalSelector;

    static {
        try {
            globalSelector = Selector.open();
        } catch (IOException e) {
            globalSelector = null;
        }
    }
    public static AtomicBoolean selectorLock = new AtomicBoolean();

    public static Closure threadWaitIO(StgContext context, Channel channel, int ops) {
        Capability cap = context.myCapability;
        TSO tso        = context.currentTSO;
        if (globalSelector == null) {
            barf("Your platform does not support non-blocking IO.");
        }
        if (!(channel instanceof SelectableChannel)) {
            barf("Non-selectable channel sent to threadWaitIO#.");
        }
        SelectionKey selectKey = null;
        try {
            SelectableChannel selectChannel = (SelectableChannel) channel;
            selectKey = selectChannel.register(globalSelector, ops, tso);
        } catch (ClosedChannelException e) {
            /* TODO: If the channel is closed, the user should know about it.
               Notify them in some form. */
        }
        WhyBlocked blocked;
        switch (ops) {
            case SelectionKey.OP_READ:
                blocked = BlockedOnRead;
                break;
            case SelectionKey.OP_WRITE:
                blocked = BlockedOnRead;
                break;
            default:
                blocked = BlockedOnIO;
                break;
        }
        tso.whyBlocked = blocked;
        tso.blockInfo  = selectKey;
        do {
            cap.blockedLoop();
        } while (selectKey.isValid());
        return null;
    }

    public static Closure waitRead(StgContext context, Object o) {
        return threadWaitIO(context, (Channel) o, SelectionKey.OP_READ);
    }

    public static Closure waitWrite(StgContext context, Object o) {
        return threadWaitIO(context, (Channel) o, SelectionKey.OP_WRITE);
    }

    public static void checkForReadyIO(Capability cap) {
        if (selectorLock.compareAndSet(false, true)) {
            try {
                int selectedKeys = globalSelector.selectNow();
                if (selectedKeys > 0) {
                    Iterator<SelectionKey> it = globalSelector.selectedKeys().iterator();
                    while (it.hasNext()) {
                        SelectionKey key = it.next();
                        if (key.isValid() && ((key.readyOps() & key.interestOps()) != 0)) {
                            TSO tso = (TSO) key.attachment();
                            key.cancel();
                            cap.tryWakeupThread(tso);
                        }
                        it.remove();
                    }
                }
            } catch (IOException e) {
                /* TODO: If the selector is closed, the user should know about it.
                   Do some logging here. */
            } finally {
                selectorLock.set(false);
            }
        }
    }

    public static int forkOS_createThread(int stablePtr) {
        try {
            new OSThread(stablePtr).start();
        } catch (java.lang.Exception e) {
            return 1;
        }
        return 0;
    }
}
