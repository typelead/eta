package ghcvm.runtime.stg;

import java.util.ListIterator;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgThunk;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult.Default;

public abstract class StackFrame extends StgClosure {
    private int stackIndex;

    @Override
    public final void enter(StgContext context) {
        ListIterator<StackFrame> sp = context.currentTSO.sp;
        // TODO: Test this logic

        /* WARNING: This logic is VERY delicate. Make sure you
                    run test cases before modifying this. */

        /* Record the index at which this frame is in the stack */
        stackIndex = sp.previousIndex();
        do {
            /* If the stack has more frames, enter
               into them */
            if (sp.hasNext()) {
                sp.next().enter(context);
            }
            int index = sp.previousIndex();
            if (stackIndex == index) {
                /* Execute the entry code for the stack frame */
                stackEnter(context);
                /* Ensure that the sp is shifted back
                    to point to this frame */
                index = sp.previousIndex();
                while (stackIndex < index) {
                    sp.previous();
                    index--;
                }
                /* Pop the frame since we're done with it now */
                /* TODO: Check if a next() or previous() is required here */
                sp.remove();
            } else {
                if (stackIndex < index) {
                    /* If stackIndex < index, new frames have been added
                        hence, we should update the Java method call stack
                        to reflect it. */
                    do {
                        sp.previous();
                        index--;
                    } while (stackIndex < index);
                    continue;
                }
                /* Whens stackIndex > index, do nothing and return
                    to the frame below this one */
            }
        } while (false);
   }

    public abstract void stackEnter(StgContext context);
    public MarkFrameResult mark(Capability cap, StgTSO tso) { return Default; }

    public enum MarkFrameResult {
        Marked, Stop, Default, Update, UpdateEvaluted
    }

    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee) {
        /* Move to the next stack frame */
        tso.sp.previous();
        return true;
    }

    public StgClosure getClosure() { return null; }

    public boolean doFindRetry(Capability cap, StgTSO tso) {
        /* Move to the next stack frame */
        return true;
    }
}
