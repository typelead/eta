#ifndef __ETAVERSION_H__
#define __ETAVERSION_H__

#ifndef __GLASGOW_HASKELL__
# define __GLASGOW_HASKELL__ 710
#endif

#ifndef __GLASGOW_HASKELL__
# define __GLASGOW_HASKELL__ 710
#endif

#define __GLASGOW_HASKELL_PATCHLEVEL1__ 3
#define __GLASGOW_HASKELL_PATCHLEVEL2__ 3

#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) (\
  ((ma)*100+(mi)) <  __GLASGOW_HASKELL__ || \
  ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \
         && (pl1) <  __GLASGOW_HASKELL_PATCHLEVEL1__ || \
  ((ma)*100+(mi)) == __GLASGOW_HASKELL__    \
         && (pl1) == __GLASGOW_HASKELL_PATCHLEVEL1__ \
         && (pl2) <= __GLASGOW_HASKELL_PATCHLEVEL2__ )

#endif /* __ETAVERSION_H__ */
