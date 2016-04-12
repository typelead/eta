#ifndef __GHVM_RTS_H__
#define __GHVM_RTS_H__

#define CLOSURE_PTR        long
#define REF_CLOSURE_PTR    LongPtr

#define SCHED_RUNNING       0
#define SCHED_INTERRUPTING  1
#define SCHED_SHUTTING_DOWN 2

// Inline Functions
#define isBoundTask(task) (task.incall.tso != null)
#define emptyRunQueue(cap) (cap.runQueueHead == null)

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


#define ACTIVITY_YES      0
#define ACTIVITY_MAYBE_NO 1
#define ACTIVITY_INACTIVE 2
#define ACTIVITY_DONE_GC  3

#endif
