#ifndef __GHVM_RTS_H__
#define __GHVM_RTS_H__

#define CLOSURE_PTR        StgClosure
#define REF_CLOSURE_PTR    Ptr<StgClosure>

#define SCHED_RUNNING       0
#define SCHED_INTERRUPTING  1
#define SCHED_SHUTTING_DOWN 2

// Exit Codes
#define EXIT_SUCCESS        0
#define EXIT_FAILURE        1
#define EXIT_MISMATCH       63
#define EXIT_SKIP           77
#define EXIT_INTERNAL_ERROR 254
#define EXIT_DEADLOCK       253
#define EXIT_INTERRUPTED    252
#define EXIT_HEAPOVERFLOW   251
#define EXIT_KILLED         250

#define IF_DEBUG(c,s) if (RtsFlags.debug && RtsFlags.DebugFlags.contains(c)) {s;}

#define DEBUG_WEAK   "weak"
#define DEBUG_APPLY  "apply"
#endif
