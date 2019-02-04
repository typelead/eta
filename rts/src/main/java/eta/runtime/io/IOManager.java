package eta.runtime.io;

import java.io.IOException;
import java.nio.channels.Selector;
import java.nio.channels.SelectionKey;
import java.nio.channels.SelectableChannel;
import java.nio.channels.CancelledKeyException;
import java.util.concurrent.locks.LockSupport;

import eta.runtime.Runtime;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.util.MPSCReferenceQueue;
import eta.runtime.exception.Exception;
import static eta.runtime.stg.TSO.WhyBlocked;
import static eta.runtime.stg.TSO.WhyBlocked.*;
import static eta.runtime.RuntimeLogging.*;

public class IOManager {
    public Selector selector;
    public final MPSCReferenceQueue<TSO> messages =
        new MPSCReferenceQueue<TSO>();
    public long eventSequence = 0;
    public volatile Capability owner;

    public IOManager(final Capability cap, final Selector selector) {
        this.selector = selector;
        this.owner    = cap;
    }

    public static IOManager create(final Capability cap) throws IOException {
        return new IOManager(cap, Selector.open());
    }

    public final void registerIO(final Capability cap, final TSO tso,
                                 final SelectableChannel channel, final int ops)
        throws IOException {
        tso.whyBlocked = toWhyBlocked(ops);
        try {
            if (owner != cap) {
                tso.blockInfo = channel;
                messages.write(tso);
                while (tso.whyBlocked != NotBlocked) {
                    /* TODO: Verify this condition */
                    LockSupport.park();
                }
            } else {
                eventLoopOneShot(cap, tso, channel, ops);
            }
        } finally {
            tso.whyBlocked = NotBlocked;
        }
    }

    public final void eventLoopOneShot(final Capability cap, final TSO tso,
                                       final SelectableChannel channel, final int ops)
        throws IOException {
        final Object result = safeRegister(channel, ops, tso);
        if (result instanceof Throwable) {
            if (Runtime.debugIO()) {
                debugIO(tso + " threw an exception while attempting to select on " +
                        channel + " with interestOps " + ops + ":\n" +
                        Exception.exceptionToString((Throwable) result));
            }
            return;
        }
        final SelectionKey sk = (SelectionKey) result;
        int numSelected  = 0;
        while ((numSelected = selector.select()) <= 0);
        sk.cancel();
    }

    /* Returns either a Throwable or a SelectionKey */
    private Object safeRegister(final SelectableChannel selectChannel,
                                final int ops, final Object attachment) {
        SelectionKey sk = null;
        boolean exceptionTried = false;
        boolean selectTried = false;
        while (sk == null) {
            try {
                sk = selectChannel.register(selector, ops, attachment);
            } catch (CancelledKeyException e) {
                if (selectTried) {
                    /* When the select() doesn't work for some reason. */
                    return e;
                } else {
                    /* This happens when the selector has invalid selector
                       keys that will only be removed upon the next select(). */
                    try {
                        selector.selectNow();
                        selectTried = true;
                    } catch (IOException io) {
                        /* We ignore these and continue on the first one. */
                        if (exceptionTried) {
                            return io;
                        } else {
                            exceptionTried = true;
                        }
                    }
                }
            } catch (IOException e) {
                /* If the channel is closed or some other anomalie happened, return instantly
                   so that the rest of the code can do appropriate cleanup. */
                return e;
            }
        }
        return sk;
    }

    private static int fromWhyBocked(final WhyBlocked whyBlocked) {
        final int ops;
        switch (whyBlocked) {
            case BlockedOnRead:
                ops = SelectionKey.OP_READ;
                break;
            case BlockedOnWrite:
                ops = SelectionKey.OP_WRITE;
                break;
            case BlockedOnConnect:
                ops = SelectionKey.OP_CONNECT;
                break;
            case BlockedOnAccept:
                ops = SelectionKey.OP_ACCEPT;
                break;
            default:
                throw new IllegalArgumentException("Invalid whyBlocked: " + whyBlocked);
        }
        return ops;
    }


    private static WhyBlocked toWhyBlocked(final int ops) {
        final WhyBlocked blocked;
        switch (ops) {
            case SelectionKey.OP_READ:
                blocked = BlockedOnRead;
                break;
            case SelectionKey.OP_WRITE:
                blocked = BlockedOnWrite;
                break;
            case SelectionKey.OP_CONNECT:
                blocked = BlockedOnConnect;
                break;
            case SelectionKey.OP_ACCEPT:
                blocked = BlockedOnAccept;
                break;
            default:
                blocked = BlockedOnIO;
                break;
        }
        return blocked;
    }
}
