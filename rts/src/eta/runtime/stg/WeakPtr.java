package eta.runtime.stg;

import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.concurrent.Concurrent.SPIN_COUNT;

public final class WeakPtr extends Value {
    /* TODO: Should `value` be volatile? */
    public WeakReference<Closure> key;
    public Closure value;
    public Closure finalizer;
    public Deque<JavaFinalizer> javaFinalizers;
    public AtomicBoolean lock = new AtomicBoolean(false);
    public boolean dead = false;

    public static class JavaFinalizer {
        public boolean hasEnvironment;
        public long fptr;
        public long ptr;
        public long eptr;

        public JavaFinalizer(boolean hasEnvironment, long fptr, long ptr, long eptr) {
            this.hasEnvironment = hasEnvironment;
            this.fptr           = fptr;
            this.ptr            = ptr;
            this.eptr           = eptr;
        }

        public void finalize() {
            Method m = FunPtr.get(fptr);
            if (m != null) {
                try {
                    if (hasEnvironment) {
                        m.invoke(null, eptr, ptr);
                    } else {
                        m.invoke(null, ptr);
                    }
                } catch (IllegalAccessException e) {
                    throw e;
                } catch (IllegalArgumentException e) {
                    throw e;
                } catch (InvocationTargetException e) {
                    throw e;
                }
            }
        }
    }

    public WeakPtr(Closure key, Closure value, Closure finalizer) {
        this.key       = new WeakReference<Closure>(key, weakPtrRefQueue);
        this.value     = value;
        this.finalizer = finalizer;
    }

    public static WeakPtr create(Closure key, Closure value, Closure finalizer) {
        WeakPtr newWeakPtr = new WeakPtr(key, value, finalizer);
        weakPtrRefMap.put(newWeakPtr.key, new WeakReference<WeakPtr>(newWeakPtr));
    }

    @Override
    public Closure enter(StgContext context) {
        barf("WEAK object entered!");
        return null;
    }

    public final void lock() {
        do {} while(!lock.compareAndSet(false, true));
    }

    public final void unlock() {
        lock.set(false);
    }

    public final boolean tryLock() {
        return lock.compareAndSet(false, true);
    }

    public final void die() {
        dead = true;
    }

    public final boolean isDead() {
        return dead;
    }

    public final Closure getValue() {
        return value;
    }

    public final void addJavaFinalizer(JavaFinalizer jfinalizer) {
        if (javaFinalizers == null) {
            lock();
            if (javaFinalizers == null) {
                javaFinalizers = new ConcurrentLinkedDeque<JavaFinalizer>();
            }
            unlock();
        }
        javaFinalizers.offerLast(jfinalizer);
    }

    public final void runJavaFinalizers() {
        if (javaFinalizers != null && !javaFinalizers.isEmpty()) {
            JavaFinalizer jfinalizer;
            while ((jfinalizer = javaFinalizers.pollLast()) != null) {
                jfinalizer.finalize();
            }
            javaFinalizers = null;
        }
    }

    /** Garbage Collection of WeakPtrs */

    private static ReferenceQueue<Closure> weakPtrRefQueue
        = new ReferenceQueue<Closure>();

    private static Map<WeakReference<Closure>, WeakReference<WeakPtr>> weakPtrRefMap
        = new ConcurrentHashMap<WeakReference<Closure>, WeakReference<WeakPtr>>();

    private static AtomicBoolean weakPtrLock = new AtomicBoolean();

    public static void checkForGCWeakPtrs() {
        if (weakPtrLock.compareAndSet(false, true)) {
            try {
                WeakReference<Closure> ref;
                List<Closure> finalizers = new ArrayList<Closure>(10);
                /* The total number of weak references that we processed due to
                   being enqueued by the GC in the reference queue. */
                int collected = 0;
                /* The total number of weak references that we *actually* processed
                   because the finalizers were alive. */
                int actuallyCollected = 0;
                while ((ref = weakPtrRefQueue.poll()) != null) {
                    WeakReference<WeakPtr> weakWeakPtr = weakPtrRefMap.get(ref);
                    if (weakWeakPtr != null) {
                        WeakPtr weakPtr = weakWeakPtr.get();
                        if (weakPtr != null && weakPtr.tryLock() && !weakPtr.isDead()) {
                            try {
                                weakPtr.die();
                                weakPtr.value = null;
                                weakPtr.runJavaFinalizers();
                                Closure finalizer = weakPtr.finalizer;
                                if (finalizer != null) {
                                    weakPtr.finalizer = null;
                                    finalizers.add(finalizer);
                                }
                                actuallyCollected++;
                            } finally {
                                weakPtr.unlock();
                            }
                        }
                        weakPtrRefMap.remove(ref);
                        collected++;
                    }
                }
                if (!finalizers.isEmpty()) {
                    int numFinalizers = finalizers.size();
                    /* TODO: Should we schedule this right away instead? */
                    Concurrent.pushToGlobalRunQueue(
                      Runtime.createIOThread(
                        Closures.applyObject(
                          Closures.apply(Closures.runFinalizerBatch
                                        ,Closures.mkInt(numFinalizers))
                         ,new Array(finalizers.toArray(new Closure[numFinalizers])))));
                }
                if (collected > 0 && Runtime.getGCOnWeakPtrFinalization()) {
                    System.gc();
                }
            } finally {
                weakPtrLock.set(false);
            }
        }
    }

}
