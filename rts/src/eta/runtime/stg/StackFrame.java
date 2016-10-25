package eta.runtime.stg;

import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.thunk.StgThunk;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Default;
import static eta.runtime.RtsMessages.barf;

public abstract class StackFrame extends StgClosure {
    public int stackIndex;

    @Override
    public final void enter(StgContext context) {
        ListIterator<StackFrame> sp = context.currentTSO.sp;
        /* WARNING: This logic is VERY delicate. Make sure you
                    run test cases before modifying this. */

        /* Record the index at which this frame is in the stack */
        stackIndex = sp.previousIndex();
        int index = stackIndex;

        /* If the stack has more frames, enter
           into them */
        while (sp.hasNext()) {
            sp.next().enter(context);

            index = sp.previousIndex();
            if (stackIndex != index) {
                /* This case occurs when frames have been removed */
                return;
            }
            if (sp.hasPrevious()) {
                /* Check if the frame at this index has been modified */
                if (sp.previous() != this) return;
                sp.next();
            }
        }
        assert stackIndex == index:
               "StackFrame.enter: breaking invariant that stackIndex == index.";

        /* Execute the entry code for the stack frame */
        stackEnter(context);

        /* Ensure that the sp is shifted back to point to this frame */
        index = sp.previousIndex();
        while (stackIndex < index) {
            sp.previous();
            index--;
        }

        /* Grab the frame sp points to */
        StackFrame thisFrame = sp.previous();
        if (thisFrame == this) {
            /* Pop the frame since we're done with it now */
            sp.remove();
        } else if (stackIndex != index){
            /* Shift the pointer to the previous one,
                only if there doesn't exist a stack frame at this index */
            sp.next();
        }
   }

    public abstract void stackEnter(StgContext context);
    public MarkFrameResult mark(Capability cap, StgTSO tso) { return Default; }

    public enum MarkFrameResult {
        Marked, Stop, Default, Update, UpdateEvaluted
    }

    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<StgClosure> topClosure) {
        return true;
    }

    public StgClosure getClosure() { return null; }

    public boolean doFindRetry(Capability cap, StgTSO tso) {
        /* Move to the next stack frame */
        return true;
    }


    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<StgClosure> raiseClosure, StgClosure exception) {
        return true;
    }


    public boolean doRaise(StgContext context, Capability cap, StgTSO tso, StgClosure exception) {
        return false;
    }
}
