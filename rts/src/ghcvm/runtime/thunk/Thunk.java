package ghcvm.runtime.thunk;

import java.util.ListIterator;

import ghcvm.runtime.stg.Stg;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgEnter;

import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static ghcvm.runtime.stg.StgContext.ReturnCode.ThreadBlocked;

public class Thunk {
    public static StgClosure block_blackhole = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                StgClosure closure = context.R1;
                ListIterator<StackFrame> sp = tso.sp;
                sp.add(new StgEnter(closure));
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                Stg.returnToSched.enter(context);
            }
        };
}
