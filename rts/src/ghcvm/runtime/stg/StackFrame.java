package ghcvm.runtime.stg;

import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import ghcvm.runtime.thunk.StgThunk;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult.Default;
import static ghcvm.runtime.RtsMessages.barf;

public abstract class StackFrame extends StgClosure {
    private int stackIndex;

    @Override
    public final void enter(StgContext context) {
        ListIterator<StackFrame> sp = context.currentTSO.sp;
        /* WARNING: This logic is VERY delicate. Make sure you
                    run test cases before modifying this. */

        /* Record the index at which this frame is in the stack */
        stackIndex = sp.previousIndex();
        do {
            /* If the stack has more frames, enter
               into them */
            while (sp.hasNext()) {
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
                StackFrame thisFrame = sp.previous();
                if (thisFrame == this) {
                    /* Pop the frame since we're done with it now */
                    sp.remove();
                } else {
                    barf("StackFrame.enter: Wrong frame after enter.");
                }
                /* TODO: Check if a next() or previous() is required here */
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
                } else {
                    barf("StackFrame.enter: stackIndex > index.");
                }
                /* Whens stackIndex > index, do nothing and return
                    to the frame below this one */
            }
            break;
        } while (true);
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
