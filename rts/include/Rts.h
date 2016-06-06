#ifndef __GHVM_RTS_H__
#define __GHVM_RTS_H__

#define CLOSURE_PTR        StgClosure
#define REF_CLOSURE_PTR    Ptr<StgClosure>

#define IF_DEBUG(c,s) if (RtsFlags.debug && RtsFlags.DebugFlags.contains(c)) {s;}

#define DEBUG_WEAK   "weak"
#define DEBUG_APPLY  "apply"
#endif
