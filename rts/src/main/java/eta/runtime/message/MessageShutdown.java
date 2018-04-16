package eta.runtime.message;

import eta.runtime.stg.Capability;

public class MessageShutdown extends Message {

    public static final MessageShutdown INSTANCE = new MessageShutdown();

    public static final ThreadDeath THREAD_DEATH_EXCEPTION = new ThreadDeath();

    private MessageShutdown() {}

    public static MessageShutdown getInstance() {
        return INSTANCE;
    }

    @Override
    public void execute(Capability cap) {
        throw THREAD_DEATH_EXCEPTION;
    }

    @Override
    public String toString() {
        return "MessageShutdown";
    }
}
