#ifndef GHCVM_H
#define GHCVM_H

#define GLOBAL_VAR(name,value,ty)               \
  {-# NOINLINE name #-};                        \
  name :: IORef (ty);                           \
  name = Util.global (value);

#define ASSERT(e)      if debugIsOn && not (e) then (assertPanic __FILE__ __LINE__) else

#endif /* GhcVM.h */
