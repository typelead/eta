package ghcvm.runtime.stackframe;

import java.util.ListIterator;

import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.types.Capability;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import static ghcvm.runtime.stackframe.StackFrame.MarkFrameResult.Default;

public abstract class StackFrame extends StgClosure {
    private int stackIndex;

    @Override
    public final void enter(StgContext context) {
        ListIterator<StackFrame> sp = context.sp;
        // TODO: Test this logic
        stackIndex = sp.previousIndex();
        if (sp.hasNext()) {
            sp.next().enter(context);
        }
        while (true) {
            int index = sp.previousIndex();
            if (stackIndex == index) {
                stackEnter(context);
                sp.remove();
                break;
            } else {
                if (stackIndex < index) {
                    do {
                        sp.previous();
                        index = sp.previousIndex();
                    } while (stackIndex < index);
                    sp.next().enter(context);
                    continue;
                }
                break;
            }
        }
   }

    public abstract void stackEnter(StgContext context);
    public MarkFrameResult mark(Capability cap, StgTSO tso) { return Default; }

    public enum MarkFrameResult {
        Marked, Stop, Default, Update, UpdateEvaluted
    }
}
