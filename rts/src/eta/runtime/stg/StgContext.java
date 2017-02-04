package eta.runtime.stg;

import java.util.ListIterator;

import static eta.runtime.stg.StgContext.ReturnCode.ThreadYielding;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;

public class StgContext {
    public ArgumentStack argStack = new ArgumentStack();
    public StgTSO currentTSO;
    public Capability myCapability;
    public ReturnCode ret;

    /* Used for ContinuationFrames */
    public int target;
    public ArgumentStack localsStack;
    public ArgumentStack returnStack;
    public volatile boolean save;

    public void reset(Capability cap, StgTSO t) {
        myCapability = cap;
        currentTSO = t;
        argStack = new ArgumentStack();
        target = 0;
        localsStack = null;
        returnStack = null;
        save = false;
    }

    public void pushFrame(StackFrame frame) {
        currentTSO.spPush(frame);
    }

    public void merge(AbstractArgumentStack argStack) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder.from(argStack)
            .setSimple(false)
            .build();
        // TODO: Need to synchronize this?
        this.argStack = (ArgumentStack) stack;
    }

    public void dump() {
        System.out.println("Context Dump");
        System.out.println("currentTSO: " + currentTSO);
        System.out.println("myCapabilitymyCapability: " + myCapability);
        System.out.println("ret: " + ret);
        argStack.dump();
        currentTSO.dump();
    }

    public StackFrame stackTop() {
        ListIterator<StackFrame> sp = currentTSO.sp;
        StackFrame prevFrame = sp.previous();
        sp.next();
        return prevFrame;
    }

    public int stackTopIndex() {
        return currentTSO.sp.previousIndex();
    }

    /* Returns false, if execution should proceed with the continuation.
       Returns true,  if the continuation should terminate
                       ( either because of an exception
                       , or because of a context switch )
     */
    public boolean checkForStackFrames(int stackIndex, StackFrame frame) {
        ListIterator<StackFrame> sp = currentTSO.sp;
        do {
            /* Do a check for context switching */
            contextSwitchCheck();

            /* NOTE: This code bears a strong resemblance to
                     StackFrame.enter() and so the logic should stay consistent. */
            /* Grab the current index */
            int index = sp.previousIndex();

            /* If frames were added, shift the pointer to 'stackIndex' */
            while (stackIndex < index) {
                sp.previous();
                index--;
            }

            StackFrame thisFrame = sp.previous();
            sp.next();
            if (save) {
                /* Context switch */
                return false;
            } else if (thisFrame == frame) {
                /* If the stack hasn't changed on us */
                if (sp.hasNext()) {
                    /* If frames were added, enter them */
                    sp.next().enter(this);
                } else {
                    /* Otherwise, return to the continuation */
                    return false;
                }
            } else  {
                /* If frames were removed, pop down the call stack */
                return true;
            }

        } while (true);
    }

    public void contextSwitchCheck() {
        if (myCapability.contextSwitch || myCapability.interrupt) {
            ret = ThreadYielding;
            currentTSO.whatNext = ThreadRunGHC;
            Stg.returnToSched.enter(this);
        }
    }

    public StgClosure R(int index) {
        return argStack.R(index);
    }

    public void R(int index, StgClosure closure) {
        argStack.R(index, closure);
    }

    public Object O(int index) {
        return argStack.O(index);
    }

    public void O(int index, Object closure) {
        argStack.O(index, closure);
    }

    public int I(int index) {
        return argStack.I(index);
    }

    public void I(int index, int closure) {
        argStack.I(index, closure);
    }

    public long L(int index) {
        return argStack.L(index);
    }

    public void L(int index, long closure) {
        argStack.L(index, closure);
    }

    public float F(int index) {
        return argStack.F(index);
    }

    public void F(int index, float closure) {
        argStack.F(index, closure);
    }

    public double D(int index) {
        return argStack.D(index);
    }

    public void D(int index, double closure) {
        argStack.D(index, closure);
    }

    public enum ReturnCode {
        HeapOverflow,
        StackOverflow,
        ThreadYielding,
        ThreadBlocked,
        ThreadFinished
    }
}
