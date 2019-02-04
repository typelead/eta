package eta.runtime.concurrent;

import java.io.IOException;
import java.util.Map;
import java.util.Queue;
import java.util.Iterator;
import java.util.concurrent.Future;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.nio.ByteBuffer;
import java.nio.channels.Channel;
import java.nio.channels.SelectionKey;
import java.nio.channels.SelectableChannel;

import eta.runtime.Runtime;
import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.io.MemoryManager;
import eta.runtime.exception.Exception;
import static eta.runtime.RuntimeLogging.*;
import static eta.runtime.stg.TSO.*;
import static eta.runtime.stg.TSO.WhyBlocked;
import static eta.runtime.stg.TSO.WhatNext;
import static eta.runtime.stg.TSO.WhyBlocked.*;
import static eta.runtime.stg.TSO.WhatNext.*;

public class Concurrent {
    public static final int SPIN_COUNT = 1000;

    /* Global Run Queue */

    public static final Queue<TSO> globalRunQueue = new ConcurrentLinkedQueue<TSO>();

    public static volatile long globalRunQueueModifiedTime = 0;

    public static void pushToGlobalRunQueue(TSO tso) {
        globalRunQueue.offer(tso);
    }

    public static TSO stealFromGlobalRunQueue() {
        return globalRunQueue.poll();
    }

    public static int getGlobalRunQueueSize() {
        return globalRunQueue.size();
    }

    public static boolean emptyGlobalRunQueue() {
        return globalRunQueue.isEmpty();
    }

    /* MVar Operations */

    public static Closure takeMVar(StgContext context, MVar mvar) {
        Capability cap = context.myCapability;
        Closure val = mvar.tryTake();
        if (Runtime.debugMVar()) {
            debugMVar("takeMVar start: " + mvar.hashCode());
        }
        if (val == null) {
            TSO tso = context.currentTSO;
            tso.whyBlocked = BlockedOnMVar;
            tso.blockInfo  = mvar;
            try {
                do {
                    cap.blockedLoop();
                    val = mvar.tryTake();
                } while (val == null);
            } finally {
                tso.whyBlocked = NotBlocked;
                tso.blockInfo  = null;
            }
        }
        if (Runtime.debugMVar()) {
            debugMVar("takeMVar done: " + mvar.hashCode());
        }
        return val;
    }

    public static Closure readMVar(StgContext context, MVar mvar) {
        if (Runtime.debugMVar()) {
            debugMVar("readMVar start: " + mvar.hashCode());
        }
        Capability cap = context.myCapability;
        Closure val = mvar.tryRead();
        if (val == null) {
            TSO tso = context.currentTSO;
            tso.whyBlocked = BlockedOnMVarRead;
            tso.blockInfo  = mvar;
            try {
                do {
                    cap.blockedLoop();
                    val = mvar.tryRead();
                } while (val == null);
            } finally {
                tso.whyBlocked = NotBlocked;
                tso.blockInfo  = null;
            }
        }
        if (Runtime.debugMVar()) {
            debugMVar("readMVar done: " + mvar.hashCode());
        }
        return val;
    }

    public static void putMVar(StgContext context, MVar mvar, Closure val) {
        if (Runtime.debugMVar()) {
            debugMVar("putMVar start: " + mvar.hashCode() + " " + val.hashCode());
        }
        Capability cap = context.myCapability;
        boolean success = mvar.tryPut(val);
        if (!success) {
            TSO tso = context.currentTSO;
            tso.whyBlocked = BlockedOnMVar;
            tso.blockInfo  = mvar;
            try {
                do {
                    cap.blockedLoop();
                    success = mvar.tryPut(val);
                } while (!success);
            } finally {
                tso.blockInfo  = null;
                tso.whyBlocked = NotBlocked;
            }
        }
        if (Runtime.debugMVar()) {
            debugMVar("putMVar done: " + mvar.hashCode());
        }
    }

    public static Closure tryTakeMVar(StgContext context, MVar mvar) {
        Closure value = mvar.tryTake();
        context.I1 = (value == null)? 0 : 1;
        return value;
    }

    public static int tryPutMVar(StgContext context, MVar mvar, Closure val) {
        return mvar.tryPut(val)? 1 : 0;
    }

    public static Closure tryReadMVar(StgContext context, MVar mvar) {
        Closure value = mvar.tryRead();
        context.I1 = (value == null)? 0: 1;
        return value;
    }

    public static TSO fork(StgContext context, Closure closure) {
        Capability cap = context.myCapability;
        TSO currentTSO = context.currentTSO;
        TSO tso = Runtime.createIOThread(closure);
        tso.addFlags(currentTSO.andFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE));
        pushToGlobalRunQueue(tso);
        cap.idleLoop(false);
        return tso;
    }

    /* TODO: The scheduling policy in Eta is for TSOs to get grabbed by Capabilities
             and keep executing them to completion which doesn't happen THAT
             frequently.

             Hence, it makes no sense to use `forkOn` in Eta since threads will
             be bound to a given thread anyways. If you put multiple threads on
             a single Capability, be warned that one of the threads may never run!
     */
    public static TSO forkOn(StgContext context, int cpu, Closure closure) {
        return fork(context, closure);
    }

    public static void yield(StgContext context) {
        Capability cap = context.myCapability;
        TSO tso        = context.currentTSO;
        tso.whyBlocked = BlockedOnYield;
        tso.blockInfo  = null;
        cap.blockedLoop();
    }

    /* In Eta, all the threads are bound, so this always returns true. */
    public static int isCurrentThreadBound(StgContext context) {
        return 1;
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
        int cap = tso.cap.getId();
        /* NOTE: The Eta RTS doesn't have a concept of TSO locking. It hurts more
                 than helps performance due to the fact that we can't presently
                 save/restore the Java stack. */
        int locked = 0;
        context.I1 = ret;
        context.I2 = cap;
        context.I3 = locked;
        return null;
    }

    /* TODO: Implement this */
    public static void traceEvent(StgContext context, long addr) {}

    public static void labelThread(StgContext context, TSO tso, long address) {
        ByteBuffer buffer  = MemoryManager.getBoundedBuffer(address);
        ByteBuffer iterate = buffer.duplicate();
        int n = 0;
        for (n = 0; iterate.get() != 0; n++) {}
        byte[] bytes = new byte[n];
        buffer.get(bytes);
        tso.setName(new String(bytes));
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
        context.O1 = exception;
        context.O2 = result;
        return null;
    }

    /* Managing Scalable I/O */
    public static void threadWaitIO(StgContext context, Channel channel, int ops)
        throws IOException {
        if (!(channel instanceof SelectableChannel)) {
            if (Runtime.debugIO()) {
                debugIO("Non-selectable channel " + channel + " sent to threadWaitIO#.");
            }
            return;
        }
        context.registerIO((SelectableChannel) channel, ops);
    }

    public static void waitRead(StgContext context, Object o) throws IOException {
        threadWaitIO(context, (Channel) o, SelectionKey.OP_READ);
    }

    public static void waitWrite(StgContext context, Object o) throws IOException {
        threadWaitIO(context, (Channel) o, SelectionKey.OP_WRITE);
    }

    public static void waitConnect(StgContext context, Object o) throws IOException {
        threadWaitIO(context, (Channel) o, SelectionKey.OP_CONNECT);
    }

    public static void waitAccept(StgContext context, Object o) throws IOException {
        threadWaitIO(context, (Channel) o, SelectionKey.OP_ACCEPT);
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
