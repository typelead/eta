package ghcvm.runtime.stackframe;

import java.util.Iterator;

public class PushFrameIterator implements Iterator<StackFrame> {

    private StackFrame frame;

    public PushFrameIterator(StackFrame frame) {
        this.frame = frame;
    }

    @Override
    public StackFrame next() {
        return frame;
    }

    @Override
    public boolean hasNext() {
        StackFrame ret = frame;
        frame = null;
        return (ret != null);
    }

    @Override
    public void remove() {}

}
