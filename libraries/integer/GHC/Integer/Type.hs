{-# LANGUAGE BangPatterns, CPP, UnboxedTuples, UnliftedFFITypes, MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- Commentary of Integer library is located on the wiki:
-- http://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/Integer
--
-- It gives an in-depth description of implementation details and
-- decisions.
--
-- TODO: Try to use optimized big/small int primops on IL32P64 archs
-- (mostly Windows/x86_64). Currently, we have to fall back to
-- unoptimized code-paths for most big/small-int primops, due to
-- @mpz_*()@ functions using @long@ types, which is smaller than
-- @mp_limb_t@ on IL32P64. The @mpn_*()@ functions are often safe to
-- use, as they use @mb_limb_t@ instead of @long@.
-- (look out for @#if SIZEOF_HSWORD == SIZEOF_LONG@ occurences)
--

#define INT_MINBOUND (-2147483648#)
#define NEG_INT_MINBOUND (S# 2147483647# `plusInteger` S# 1#)

module GHC.Integer.Type where

import GHC.Magic (runRW#)

import GHC.Prim (
  -- Other types we use, convert from, or convert to
  Int#, Word#, Double#, Float#, ByteArray#, MutableByteArray#, Addr#, State#,
  indexIntArray#,
  -- Conversions between those types
  int2Word#, int2Double#, int2Float#, word2Int#,
  -- Operations on Int# that we use for operations on S#
  quotInt#, remInt#, quotRemInt#, negateInt#,
  (*#), (-#),
  (==#), (/=#), (<=#), (>=#), (<#), (>#),
  mulIntMayOflo#, addIntC#, subIntC#,
  and#, or#, xor#,
  void#,

  -- 64-bit operations
  Int64#, Word64#,
  int64ToWord64#, intToInt64#,
  int64ToInt#, word64ToInt64#,
  geInt64#, leInt64#, leWord64#,

  -- Other
  unsafeCoerce#, jbool2int#,
  jobjectArrayAt#
 )

import GHC.Integer.BigInteger.Prim (
    -- BigInteger-related primitives
    Integer#, IntegerPair#,
    cmpInteger#,
    cmpIntegerInt#,
    plusInteger#, minusInteger#,
    timesInteger#,
    quotRemInteger#, quotInteger#, remInteger#,
    divModInteger#, divInteger#, modInteger#,
    divExactInteger#,
    gcdInteger#, gcdExtInteger#, gcdIntegerInt#, gcdInt#,
    decodeDouble#,
    int2Integer#, integer2Int#, word2Integer#, integer2Word#,
    andInteger#, orInteger#, xorInteger#, complementInteger#,
    testBitInteger#, mul2ExpInteger#, fdivQ2ExpInteger#,
    powInteger#, powModInteger#, powModSecInteger#, recipModInteger#,
    nextPrimeInteger#, testPrimeInteger#,
    -- sizeInBaseInteger#,
    -- importIntegerFromByteArray#, importIntegerFromAddr#,
    -- exportIntegerToMutableByteArray#, exportIntegerToAddr#,
    int64ToInteger#,  integerToInt64#,
    word64ToInteger#, integerToWord64#,
    --ETA-specific
    zeroInteger#,
    equalsInteger#,
    absInteger#,
    bitsInteger#,
    signumInteger#,
    negateInteger#,
    integer2Float#,
    integer2Double#,
    int_encodeFloat#,
    encodeFloat#,
    int_encodeDouble#,
    encodeDouble#
 )

import GHC.Classes
import GHC.Types

default ()
{-
%*********************************************************
%*                                                      *
\subsection{The @Integer@ type}
%*                                                      *
%*********************************************************

Convenient boxed Integer PrimOps.

-}
-- | Arbitrary-precision integers.
data Integer
   = S# Int#            -- ^ \"small\" integers fitting into an 'Int#'
   | J# Integer#        -- ^ \"big\" integers represented as GMP's @mpz_t@ structure.
     --
     -- The 'Int#' field corresponds to @mpz_t@'s @_mp_size@ field,
     -- which encodes the sign and the number of /limbs/ stored in the
     -- 'ByteArray#' field (i.e. @mpz_t@'s @_mp_d@ field). Note: The
     -- 'ByteArray#' may have been over-allocated, and thus larger
     -- than the size denoted by the 'Int#' field.
     --
     -- This representation tries to avoid using the GMP number
     -- representation for small integers that fit into a native
     -- 'Int#'. This allows to reduce (or at least defer) calling into GMP
     -- for operations whose results remain in the 'Int#'-domain.
     --
     -- Note: It does __not__ constitute a violation of invariants to
     -- represent an integer which would fit into an 'Int#' with the
     -- 'J#'-constructor. For instance, the value @0@ has (only) two valid
     -- representations, either @'S#' 0#@ or @'J#' 0 _@.

-- | Construct 'Integer' value from list of 'Int's.
--
-- This function is used by GHC for constructing 'Integer' literals.
mkInteger :: Bool   -- ^ sign of integer ('True' if non-negative)
          -> [Int]  -- ^ absolute value expressed in 31 bit chunks, least significant first

                    -- (ideally these would be machine-word 'Word's rather than 31-bit truncated 'Int's)
          -> Integer
mkInteger nonNegative is = let abs = f is
                           in if nonNegative then abs else negateInteger abs
    where f [] = S# 0#
          f (I# i : is') = S# i `orInteger` shiftLInteger (f is') 31#

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i = S# i

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = if isTrue# (i >=# 0#)
                  then S# i
                  else case word2Integer# w of o# -> J# o#
    where
      !i = word2Int# w

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (S# i) = int2Word# i
integerToWord (J# o#) = integer2Word# o#

{-# NOINLINE integerToWord64 #-}
integerToWord64 :: Integer -> Word64#
integerToWord64 (S# i) = int64ToWord64# (intToInt64# i)
integerToWord64 (J# o#) = integerToWord64# o#

{-# NOINLINE word64ToInteger #-}
word64ToInteger :: Word64# -> Integer
word64ToInteger w = if isTrue# (w `leWord64#` int64ToWord64# (intToInt64# 0x7FFFFFFF#))
                    then S# (int64ToInt# (word64ToInt64# w))
                    else case word64ToInteger# w of o# -> J# o#

{-# NOINLINE integerToInt64 #-}
integerToInt64 :: Integer -> Int64#
integerToInt64 (S# i) = intToInt64# i
integerToInt64 (J# o#) = integerToInt64# o#

{-# NOINLINE int64ToInteger #-}
int64ToInteger :: Int64# -> Integer
int64ToInteger i = if isTrue# (i `leInt64#` intToInt64# 0x7FFFFFFF#) &&
                      isTrue# (i `geInt64#` intToInt64# -0x80000000#)
                   then smallInteger (int64ToInt# i)
                   else case int64ToInteger# i of o# -> J# o#

integerToInt :: Integer -> Int#
{-# NOINLINE integerToInt #-}
integerToInt (S# i)   = i
integerToInt (J# o#) = integer2Int# o#

-- This manually floated out constant is needed as GHC doesn't do it on its own
minIntAsBig :: Integer
minIntAsBig = case int2Integer# INT_MINBOUND of o# -> J# o#

-- | Promote 'S#' to 'J#'
toBig :: Integer -> Integer
toBig (S# i)     = case int2Integer# i of o# -> J# o#
toBig i@(J# _) = i

-- | Demote 'J#' to 'S#' if possible. See also 'smartJ#'.
toSmall :: Integer -> Integer
toSmall i@(S# _) = i
toSmall (J# o#)  = smartJ# o#


-- | Smart 'J#' constructor which tries to construct 'S#' if possible
smartJ# :: Integer# -> Integer
smartJ# i# = if isTrue# (bits <=# 31#) then
               S# (integer2Int# i#)
             else
               J# i#
  where bits = bitsInteger# i#

-- -- |Construct 'Integer' out of a 'MPZ#' as returned by GMP wrapper primops
-- --
-- -- IMPORTANT: The 'ByteArray#' element MUST NOT be accessed unless the
-- -- size-element indicates more than one limb!
-- --
-- -- See notes at definition site of 'MPZ#' in "GHC.Integer.GMP.Prim"
-- -- for more details.
-- mpzToInteger :: MPZ# -> Integer
-- mpzToInteger (# 0#, _, _ #) = S# 0#
-- mpzToInteger (# 1#, _, w# #) | isTrue# (v# >=# 0#) = S# v#
--                              | True = case word2Integer# w# of (# _, d #) -> J# 1# d
--     where
--       v# = word2Int# w#
-- mpzToInteger (# -1#, _, w# #) | isTrue# (v# <=# 0#) = S# v#
--                               | True = case word2Integer# w# of (# _, d #) -> J# -1# d
--     where
--       v# = negateInt# (word2Int# w#)
-- mpzToInteger (# s#, mb#, _ #) = J# s# mb#

-- | Variant of 'mpzToInteger' for pairs of 'Integer's
unboxedIntegerPair :: IntegerPair# -> (# Integer, Integer #)
unboxedIntegerPair bigIntArr# =
  case runRW# (\s -> jobjectArrayAt# bigIntArr# 0# s) of
    (# s, i1 #) -> case jobjectArrayAt# bigIntArr# 1# s of
      (# _, i2 #) -> (# smartJ# i1, smartJ# i2 #)

-- -- |Negate MPZ#
-- mpzNeg :: MPZ# -> MPZ#
-- mpzNeg (# s#, mb#, w# #) = (# negateInt# s#, mb#, w# #)

{- Note [Use S# if possible]
~~~~~~~~~~~~~~~~~~~~~~~~~
It's a big win to use S#, rather than J#, whenever possible.  Not only
does it take less space, but (probably more important) subsequent
operations are more efficient. See Trac #8638.

'smartJ#' is the smart constructor for J# that performs the necessary
tests.  When returning a nested result, we always use smartJ# strictly,
thus
       let !r = smartJ# a b in (# r, somthing_else #)
to avoid creating a thunk that is subsequently evaluated to a J#.
smartJ# itself does a pretty small amount of work, so it's not worth
thunking it.

We call 'smartJ#' in places like quotRemInteger where a big input
might produce a small output.

Just using smartJ# in this way has good results:

        Program           Size    Allocs   Runtime   Elapsed  TotalMem
--------------------------------------------------------------------------------
         gamteb          +0.1%    -19.0%      0.03      0.03     +0.0%
          kahan          +0.2%     -1.2%      0.17      0.17     +0.0%
         mandel          +0.1%     -7.7%      0.05      0.05     +0.0%
          power          +0.1%    -40.8%    -32.5%    -32.5%     +0.0%
         symalg          +0.2%     -0.5%      0.01      0.01     +0.0%
--------------------------------------------------------------------------------
            Min          +0.0%    -40.8%    -32.5%    -32.5%     -5.1%
            Max          +0.2%     +0.1%     +2.0%     +2.0%     +0.0%
 Geometric Mean          +0.1%     -1.0%     -2.5%     -2.5%     -0.1%

%*********************************************************
%*                                                      *
\subsection{Dividing @Integers@}
%*                                                      *
%*********************************************************
-}

-- XXX There's no good reason for us using unboxed tuples for the
-- results, but we don't have Data.Tuple available.

-- Note that we don't check for divide-by-zero here. That needs
-- to be done where it's used.
-- (we don't have error)

-- TODO: Verify divMod/quotRem are correct
{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger (S# INT_MINBOUND) b = quotRemInteger minIntAsBig b
quotRemInteger (S# i) (S# j) = case quotRemInt# i j of
                                   (# q, r #) -> (# S# q, S# r #)
quotRemInteger i1@(J# _) i2@(S# _) = quotRemInteger i1 (toBig i2)
quotRemInteger i1@(S# _) i2@(J# _) = quotRemInteger (toBig i1) i2
quotRemInteger (J# o1#) (J# o2#)
  = unboxedIntegerPair (quotRemInteger# o1# o2#) -- See Note [Use S# if possible]

{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger (S# INT_MINBOUND) b = divModInteger minIntAsBig b
divModInteger (S# i) (S# j) = (# S# d, S# m #)
    where
      -- NB. don't inline these.  (# S# (i `quotInt#` j), ... #) means
      -- (# let q = i `quotInt#` j in S# q, ... #) which builds a
      -- useless thunk.  Placing the bindings here means they'll be
      -- evaluated strictly.
      !d = i `divInt#` j
      !m = i `modInt#` j
divModInteger i1@(J# _) i2@(S# _) = divModInteger i1 (toBig i2)
divModInteger i1@(S# _) i2@(J# _) = divModInteger (toBig i1) i2
divModInteger (J# o1#) (J# o2#) = unboxedIntegerPair (divModInteger# o1# o2#)

{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger (S# INT_MINBOUND) b = remInteger minIntAsBig b
remInteger (S# a) (S# b) = S# (remInt# a b)
{- Special case doesn't work, because a 1-element J# has the range
   -(2^32-1) -- 2^32-1, whereas S# has the range -2^31 -- (2^31-1)
remInteger ia@(S# a) (J# sb b)
  | sb ==# 1#  = S# (remInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (remInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | 0# <# sb   = ia
  | otherwise  = S# (0# -# a)
-}
remInteger ia@(S# _) ib@(J# _) = remInteger (toBig ia) ib
remInteger i1@(J# _) i2@(S# _) = remInteger i1 (toBig i2)
remInteger (J# o1#) (J# o2#)
  = smartJ# (remInteger# o1# o2#)

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger (S# INT_MINBOUND) b = quotInteger minIntAsBig b
quotInteger (S# a) (S# b) = S# (quotInt# a b)
{- Special case disabled, see remInteger above
quotInteger (S# a) (J# sb b)
  | sb ==# 1#  = S# (quotInt# a (word2Int# (integer2Word# sb b)))
  | sb ==# -1# = S# (quotInt# a (0# -# (word2Int# (integer2Word# sb b))))
  | otherwise  = S# 0
-}
quotInteger ia@(S# _) ib@(J# _) = quotInteger (toBig ia) ib
quotInteger i1@(J# _) i2@(S# _) = quotInteger i1 (toBig i2)
quotInteger (J# o1#) (J# o2#)
  = smartJ# (quotInteger# o1# o2#)

{-# NOINLINE modInteger #-}
modInteger :: Integer -> Integer -> Integer
modInteger (S# INT_MINBOUND) b = modInteger minIntAsBig b
modInteger (S# a) (S# b) = S# (modInt# a b)
modInteger ia@(S# _) ib@(J# _) = modInteger (toBig ia) ib
modInteger i1@(J# _) i2@(S# _) = modInteger i1 (toBig i2)
modInteger (J# o1#) (J# o2#)
  = smartJ# (modInteger# o1# o2#)

{-# NOINLINE divInteger #-}
divInteger :: Integer -> Integer -> Integer
divInteger (S# INT_MINBOUND) b = divInteger minIntAsBig b
divInteger (S# a) (S# b) = S# (divInt# a b)
divInteger ia@(S# _) ib@(J# _) = divInteger (toBig ia) ib
divInteger i1@(J# _) i2@(S# _) = divInteger i1 (toBig i2)
divInteger (J# o1#) (J# o2#)
  = smartJ# (divInteger# o1# o2#)

-- | Compute greatest common divisor.
{-# NOINLINE gcdInteger #-}
gcdInteger :: Integer -> Integer -> Integer
-- SUP: Do we really need the first two cases?
gcdInteger (S# INT_MINBOUND) b = gcdInteger minIntAsBig b
gcdInteger a (S# INT_MINBOUND) = gcdInteger a minIntAsBig
gcdInteger (S# a) (S# b) = S# (gcdInt a b)
gcdInteger ia@(S# a)  ib@(J# o#)
 =      if isTrue# (a  ==# 0#) then absInteger ib
   else if isTrue# (jbool2int# (o# `equalsInteger#` (zeroInteger# void#))) then absInteger ia
   else                             S# (gcdIntegerInt# absO absA)
       where !absA = if isTrue# (a <# 0#) then negateInt# a  else a
             !absO = absInteger# o#
gcdInteger ia@(J# _) ib@(S# _) = gcdInteger ib ia
gcdInteger (J# o1#) (J# o2#)   = smartJ# (gcdInteger# o1# o2#)

-- | Extended euclidean algorithm.
--
-- For @/a/@ and @/b/@, compute their greatest common divisor @/g/@
-- and the coefficient @/s/@ satisfying @/a//s/ + /b//t/ = /g/@.
--
-- /Since: 0.5.1.0/
{-# NOINLINE gcdExtInteger #-}
gcdExtInteger :: Integer -> Integer -> (# Integer, Integer #)
gcdExtInteger a@(S# _)   b@(S# _) = gcdExtInteger (toBig a) (toBig b)
gcdExtInteger a@(S# _) b@(J# _)   = gcdExtInteger (toBig a) b
gcdExtInteger a@(J# _) b@(S# _)   = gcdExtInteger a (toBig b)
gcdExtInteger (J# o1#) (J# o2#)   = unboxedIntegerPair (gcdExtInteger# o1# o2#)

-- | Compute least common multiple.
{-# NOINLINE lcmInteger #-}
lcmInteger :: Integer -> Integer -> Integer
lcmInteger a b =      if a `eqInteger` S# 0# then S# 0#
                 else if b `eqInteger` S# 0# then S# 0#
                 else (divExact aa (gcdInteger aa ab)) `timesInteger` ab
  where aa = absInteger a
        ab = absInteger b

-- | Compute greatest common divisor.
gcdInt :: Int# -> Int# -> Int#
gcdInt 0# y  = absInt y
gcdInt x  0# = absInt x
gcdInt x  y  = gcdInt# (absInt x) (absInt y)

absInt :: Int# -> Int#
absInt x = if isTrue# (x <# 0#) then negateInt# x else x

divExact :: Integer -> Integer -> Integer
divExact (S# INT_MINBOUND) b = divExact minIntAsBig b
divExact (S# a) (S# b) = S# (quotInt# a b)
divExact (S# a) (J# o#)
  = S# (quotInt# a (integer2Int# o#))
divExact i1@(J# _) i2@(S# _) = divExact i1 (toBig i2)
divExact (J# o1#) (J# o2#) = smartJ# (divExactInteger# o1# o2#)

{-
%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Eq@, @Ord@}
%*                                                      *
%*********************************************************
-}

-- | /Since: 0.5.1.0/
{-# NOINLINE eqInteger# #-}
eqInteger# :: Integer -> Integer -> Int#
eqInteger# (S# i)   (S# j)   = i ==# j
eqInteger# (S# i)   (J# o#)  = cmpIntegerInt# o# i ==# 0#
eqInteger# (J# o#)  (S# i)   = cmpIntegerInt# o# i ==# 0#
eqInteger# (J# o1#) (J# o2#) = cmpInteger# o1# o2# ==# 0#

-- | /Since: 0.5.1.0/
{-# NOINLINE neqInteger# #-}
neqInteger# :: Integer -> Integer -> Int#
neqInteger# (S# i)   (S# j)   = i /=# j
neqInteger# (S# i)   (J# o#)  = cmpIntegerInt# o# i /=# 0#
neqInteger# (J# o#)  (S# i)   = cmpIntegerInt# o# i /=# 0#
neqInteger# (J# o1#) (J# o2#) = cmpInteger# o1# o2# /=# 0#

{-# INLINE eqInteger  #-}
{-# INLINE neqInteger #-}
eqInteger, neqInteger :: Integer -> Integer -> Bool
eqInteger  a b = isTrue# (a `eqInteger#`  b)
neqInteger a b = isTrue# (a `neqInteger#` b)

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

------------------------------------------------------------------------

-- | /Since: 0.5.1.0/
{-# NOINLINE leInteger# #-}
leInteger# :: Integer -> Integer -> Int#
leInteger# (S# i)   (S# j)   = i <=# j
leInteger# (J# o#)  (S# i)   = cmpIntegerInt# o# i <=# 0#
leInteger# (S# i)   (J# o#)  = cmpIntegerInt# o# i >=# 0#
leInteger# (J# o1#) (J# o2#) = cmpInteger# o1# o2# <=# 0#

-- | /Since: 0.5.1.0/
{-# NOINLINE gtInteger# #-}
gtInteger# :: Integer -> Integer -> Int#
gtInteger# (S# i)   (S# j)   = i ># j
gtInteger# (J# o#)  (S# i)   = cmpIntegerInt# o# i ># 0#
gtInteger# (S# i)   (J# o#)  = cmpIntegerInt# o# i <# 0#
gtInteger# (J# o1#) (J# o2#) = cmpInteger# o1# o2# ># 0#

-- | /Since: 0.5.1.0/
{-# NOINLINE ltInteger# #-}
ltInteger# :: Integer -> Integer -> Int#
ltInteger# (S# i)   (S# j)   = i <# j
ltInteger# (J# o#)  (S# i)   = cmpIntegerInt# o# i <# 0#
ltInteger# (S# i)   (J# o#)  = cmpIntegerInt# o# i ># 0#
ltInteger# (J# o1#) (J# o2#) = cmpInteger# o1# o2# <# 0#

-- | /Since: 0.5.1.0/
{-# NOINLINE geInteger# #-}
geInteger# :: Integer -> Integer -> Int#
geInteger# (S# i)   (S# j)   = i >=# j
geInteger# (J# o#)  (S# i)   = cmpIntegerInt# o# i >=# 0#
geInteger# (S# i)   (J# o#)  = cmpIntegerInt# o# i <=# 0#
geInteger# (J# o1#) (J# o2#) = cmpInteger# o1# o2# >=# 0#

{-# INLINE leInteger #-}
{-# INLINE ltInteger #-}
{-# INLINE geInteger #-}
{-# INLINE gtInteger #-}
leInteger, gtInteger, ltInteger, geInteger :: Integer -> Integer -> Bool
leInteger a b = isTrue# (a `leInteger#` b)
gtInteger a b = isTrue# (a `gtInteger#` b)
ltInteger a b = isTrue# (a `ltInteger#` b)
geInteger a b = isTrue# (a `geInteger#` b)

{-# NOINLINE compareInteger #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger (S# i)  (S# j)
   =      if isTrue# (i ==# j) then EQ
     else if isTrue# (i <=# j) then LT
     else                           GT
compareInteger (J# o#) (S# i)
   = case cmpIntegerInt# o# i of { res# ->
     if isTrue# (res# <# 0#) then LT else
     if isTrue# (res# ># 0#) then GT else EQ
     }
compareInteger (S# i) (J# o#)
   = case cmpIntegerInt# o# i of { res# ->
     if isTrue# (res# ># 0#) then LT else
     if isTrue# (res# <# 0#) then GT else EQ
     }
compareInteger (J# o1#) (J# o2#)
   = case cmpInteger# o1# o2# of { res# ->
     if isTrue# (res# <# 0#) then LT else
     if isTrue# (res# ># 0#) then GT else EQ
     }

instance Ord Integer where
    (<=) = leInteger
    (<)  = ltInteger
    (>)  = gtInteger
    (>=) = geInteger
    compare = compareInteger

{-
%*********************************************************
%*                                                      *
\subsection{The @Integer@ instances for @Num@}
%*                                                      *
%*********************************************************
-}

{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger (S# INT_MINBOUND) = NEG_INT_MINBOUND
absInteger n@(S# i)   = if isTrue# (i >=# 0#) then n else S# (negateInt# i)
absInteger n@(J# o#) = J# (absInteger# o#)

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger (S# i) = if isTrue# (i <# 0#) then S# -1#
                       else if isTrue# (i ==# 0#) then S# 0#
                       else S# 1#
signumInteger (J# o#) = S# (signumInteger# o#)

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger i1@(S# i) i2@(S# j) = case addIntC# i j of
                                    (# r, c #) ->
                                      if isTrue# (c ==# 0#)
                                      then S# r
                                      else plusInteger (toBig i1) (toBig i2)
plusInteger i1@(J# _) (S# 0#)   = i1
plusInteger i1@(J# _) i2@(S# _) = plusInteger i1 (toBig i2)
plusInteger i1@(S# _) i2@(J# _) = plusInteger i2 i1
plusInteger (J# o1#)  (J# o2#)  = smartJ# (plusInteger# o1# o2#)

{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger i1@(S# i) i2@(S# j) = case subIntC# i j of
                                     (# r, c #) ->
                                       if isTrue# (c ==# 0#) then S# r
                                       else minusInteger (toBig i1) (toBig i2)
minusInteger i1@(J# _) (S# 0#)   = i1
minusInteger (S# 0#)   (J# o#)   = J# (negateInteger# o#)
minusInteger i1@(J# _) i2@(S# _) = minusInteger i1 (toBig i2)
minusInteger i1@(S# _) i2@(J# _) = minusInteger (toBig i1) i2
minusInteger (J# o1#)  (J# o2#)  = smartJ# (minusInteger# o1# o2#)

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger i1@(S# i) i2@(S# j) = if isTrue# (mulIntMayOflo# i j ==# 0#)
                                   then S# (i *# j)
                                   else timesInteger (toBig i1) (toBig i2)
timesInteger (S# 0#)   _         = S# 0#
timesInteger (S# -1#)  i2        = negateInteger i2
timesInteger (S# 1#)   i2        = i2
timesInteger i1@(S# _) i2@(J# _) = timesInteger (toBig i1) i2
timesInteger i1@(J# _) i2@(S# _) = timesInteger i2 i1 -- swap args & retry
timesInteger (J# o1#)  (J# o2#)  = smartJ# (timesInteger# o1# o2#)

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger (S# INT_MINBOUND) = NEG_INT_MINBOUND
negateInteger (S# i)            = S# (negateInt# i)
negateInteger (J# o#)           = J# (negateInteger# o#)

{-
%*********************************************************
%*                                                      *
\subsection{The @Integer@ stuff for Double@}
%*                                                      *
%*********************************************************
-}
-- TODO: Work on encoding/decoding of floats

{-# NOINLINE encodeFloatInteger #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger (S# i)  j = int_encodeFloat# i j
encodeFloatInteger (J# o#) e = encodeFloat# o# e

{-# NOINLINE encodeDoubleInteger #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (S# i) j  = int_encodeDouble# i j
encodeDoubleInteger (J# o#) e = encodeDouble# o# e

{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d = case decodeDouble# d of
                          (# exp#, man# #) -> let !man = smartJ# man#
                                              in (# man, exp# #)

-- previous code: doubleFromInteger n = fromInteger n = encodeFloat n 0
-- doesn't work too well, because encodeFloat is defined in
-- terms of ccalls which can never be simplified away.  We
-- want simple literals like (fromInteger 3 :: Float) to turn
-- into (F# 3.0), hence the special case for S# here.

{-# NOINLINE doubleFromInteger #-}
doubleFromInteger :: Integer -> Double#
doubleFromInteger (S# i#) = int2Double# i#
doubleFromInteger (J# o#) = integer2Double# o#

{-# NOINLINE floatFromInteger #-}
floatFromInteger :: Integer -> Float#
floatFromInteger (S# i#) = int2Float# i#
floatFromInteger (J# o#) = integer2Float# o#

{-
%*********************************************************
%*                                                      *
\subsection{The @Integer@ Bit definitions@}
%*                                                      *
%*********************************************************

We explicitly pattern match against J# and S# in order to produce
Core that doesn't have pattern matching errors, as that would
introduce a spurious dependency to base.
-}
{-# NOINLINE andInteger #-}
andInteger :: Integer -> Integer -> Integer
(S# x)   `andInteger` (S# y)   = S# (word2Int# (int2Word# x `and#` int2Word# y))
x@(S# _) `andInteger` y@(J# _) = toBig x `andInteger` y
x@(J# _) `andInteger` y@(S# _) = x `andInteger` toBig y
(J# o1#) `andInteger` (J# o2#) = smartJ# (andInteger# o1# o2#)

{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
(S# x)   `orInteger` (S# y)   = S# (word2Int# (int2Word# x `or#` int2Word# y))
x@(S# _) `orInteger` y@(J# _) = toBig x `orInteger` y
x@(J# _) `orInteger` y@(S# _) = x `orInteger` toBig y
(J# o1#) `orInteger` (J# o2#) = smartJ# (orInteger# o1# o2#)

{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
(S# x)   `xorInteger` (S# y)   = S# (word2Int# (int2Word# x `xor#` int2Word# y))
x@(S# _) `xorInteger` y@(J# _) = toBig x `xorInteger` y
x@(J# _) `xorInteger` y@(S# _) = x `xorInteger` toBig y
(J# o1#) `xorInteger` (J# o2#) = smartJ# (xorInteger# o1# o2#)

{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger (S# x)  = S# (word2Int# (int2Word# x `xor#` int2Word# (0# -# 1#)))
complementInteger (J# o#) = smartJ# (complementInteger# o#)

{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger j@(S# _) i = shiftLInteger (toBig j) i
shiftLInteger (J# o#)  i = smartJ# (mul2ExpInteger# o# i)

{-# NOINLINE shiftRInteger #-}
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger j@(S# _) i = shiftRInteger (toBig j) i
shiftRInteger (J# o#)  i = smartJ# (fdivQ2ExpInteger# o# i)

-- | /Since: 0.5.1.0/
{-# NOINLINE testBitInteger #-}
testBitInteger :: Integer -> Int# -> Bool
testBitInteger j@(S# _) i = testBitInteger (toBig j) i
testBitInteger (J# o#)  i = isTrue# (jbool2int# (testBitInteger# o# i) /=# 0#)

-- | \"@'powInteger' /b/ /e/@\" computes base @/b/@ raised to exponent @/e/@.
--
-- /Since: 0.5.1.0/
{-# NOINLINE powInteger #-}
powInteger :: Integer -> Word# -> Integer
powInteger j@(S# _) e = powInteger (toBig j) e
powInteger (J# o#)  e = smartJ# (powInteger# o# e)

-- | \"@'powModInteger' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- Negative exponents are supported if an inverse modulo @/m/@
-- exists. It's advised to avoid calling this primitive with negative
-- exponents unless it is guaranteed the inverse exists, as failure to
-- do so will likely cause program abortion due to a divide-by-zero
-- fault. See also 'recipModInteger'.
--
-- /Since: 0.5.1.0/
{-# NOINLINE powModInteger #-}
powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger (J# o1#) (J# o2#) (J# o3#) =
    smartJ# (powModInteger# o1# o2# o3#)
powModInteger b e m = powModInteger (toBig b) (toBig e) (toBig m)

-- | \"@'powModSecInteger' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@. It is required that @/e/ > 0@ and
-- @/m/@ is odd.
--
-- This is a \"secure\" variant of 'powModInteger' using the
-- @mpz_powm_sec()@ function which is designed to be resilient to side
-- channel attacks and is therefore intended for cryptographic
-- applications.
--
-- This primitive is only available when the underlying GMP library
-- supports it (GMP >= 5). Otherwise, it internally falls back to
-- @'powModInteger'@, and a warning will be emitted when used.
--
-- /Since: 0.5.1.0/
-- TODO: Implement powModSecInteger properly
{-# NOINLINE powModSecInteger #-}
powModSecInteger :: Integer -> Integer -> Integer -> Integer
powModSecInteger (J# o1#) (J# o2#) (J# o3#) =
    smartJ# (powModSecInteger# o1# o2# o3#)
powModSecInteger b e m = powModSecInteger (toBig b) (toBig e) (toBig m)

-- | \"@'recipModInteger' /x/ /m/@\" computes the inverse of @/x/@ modulo @/m/@. If
-- the inverse exists, the return value @/y/@ will satisfy @0 < /y/ <
-- abs(/m/)@, otherwise the result is @0@.
--
-- Note: The implementation exploits the undocumented property of
-- @mpz_invert()@ to not mangle the result operand (which is initialized
-- to 0) in case of non-existence of the inverse.
--
-- /Since: 0.5.1.0/
{-# NOINLINE recipModInteger #-}
recipModInteger :: Integer -> Integer -> Integer
recipModInteger j@(S# _) m@(S# _) = recipModInteger (toBig j) (toBig m)
recipModInteger j@(S# _) m@(J# _) = recipModInteger (toBig j) m
recipModInteger j@(J# _) m@(S# _) = recipModInteger j (toBig m)
recipModInteger (J# o1#) (J# o2#) = smartJ# (recipModInteger# o1# o2#)

-- | Probalistic Miller-Rabin primality test.
--
-- \"@'testPrimeInteger' /n/ /k/@\" determines whether @/n/@ is prime
-- and returns one of the following results:
--
-- * @2#@ is returned if @/n/@ is definitely prime,
--
-- * @1#@ if @/n/@ is a /probable prime/, or
--
-- * @0#@ if @/n/@ is definitely not a prime.
--
-- The @/k/@ argument controls how many test rounds are performed for
-- determining a /probable prime/. For more details, see
-- <http://gmplib.org/manual/Number-Theoretic-Functions.html#index-mpz_005fprobab_005fprime_005fp-360 GMP documentation for `mpz_probab_prime_p()`>.
--
-- /Since: 0.5.1.0/
-- TODO: Second argument is certainty, not # of rounds
-- TODO: Replace unsafeCoerce# with JBool# -> Int# function
{-# NOINLINE testPrimeInteger #-}
testPrimeInteger :: Integer -> Int# -> Int#
testPrimeInteger j@(S# _) reps = testPrimeInteger (toBig j) reps
testPrimeInteger (J# o#)  reps = jbool2int# (testPrimeInteger# o# reps)

-- | Compute next prime greater than @/n/@ probalistically.
--
-- According to the GMP documentation, the underlying function
-- @mpz_nextprime()@ \"uses a probabilistic algorithm to identify
-- primes. For practical purposes it's adequate, the chance of a
-- composite passing will be extremely small.\"
--
-- /Since: 0.5.1.0/
{-# NOINLINE nextPrimeInteger #-}
nextPrimeInteger :: Integer -> Integer
nextPrimeInteger j@(S# _) = nextPrimeInteger (toBig j)
nextPrimeInteger (J# o#)  = smartJ# (nextPrimeInteger# o#)

-- | Compute number of digits (without sign) in given @/base/@.
--
-- It's recommended to avoid calling 'sizeInBaseInteger' for small
-- integers as this function would currently convert those to big
-- integers in order to call @mpz_sizeinbase()@.
--
-- This function wraps @mpz_sizeinbase()@ which has some
-- implementation pecularities to take into account:
--
-- * \"@'sizeInBaseInteger' 0 /base/ = 1@\" (see also comment in 'exportIntegerToMutableByteArray').
--
-- * This function is only defined if @/base/ >= 2#@ and @/base/ <= 256#@
--   (Note: the documentation claims that only @/base/ <= 62#@ is
--   supported, however the actual implementation supports up to base 256).
--
-- * If @/base/@ is a power of 2, the result will be exact. In other
--   cases (e.g. for @/base/ = 10#@), the result /may/ be 1 digit too large
--   sometimes.
--
-- * \"@'sizeInBaseInteger' /i/ 2#@\" can be used to determine the most
--   significant bit of @/i/@.
--
-- /Since: 0.5.1.0/
-- {-# NOINLINE sizeInBaseInteger #-}
-- sizeInBaseInteger :: Integer -> Int# -> Word#
-- sizeInBaseInteger (J# o#) b  = sizeInBaseInteger# o# b
-- sizeInBaseInteger j@(S# _) b = sizeInBaseInteger (toBig j) b -- TODO

-- -- | Dump 'Integer' (without sign) to mutable byte-array in base-256 representation.
-- --
-- -- The call
-- --
-- -- @'exportIntegerToMutableByteArray' /i/ /mba/ /offset/ /order/@
-- --
-- -- writes
-- --
-- -- * the 'Integer' @/i/@
-- --
-- -- * into the 'MutableByteArray#' @/mba/@ starting at @/offset/@
-- --
-- -- * with most significant byte first if @order@ is @1#@ or least
-- --   significant byte first if @order@ is @-1#@, and
-- --
-- -- * returns number of bytes written.
-- --
-- -- Use \"@'sizeInBaseInteger' /i/ 256#@\" to compute the exact number of
-- -- bytes written in advance for @/i/ /= 0@. In case of @/i/ == 0@,
-- -- 'exportIntegerToMutableByteArray' will write and report zero bytes
-- -- written, whereas 'sizeInBaseInteger' report one byte.
-- --
-- -- It's recommended to avoid calling 'exportIntegerToMutableByteArray' for small
-- -- integers as this function would currently convert those to big
-- -- integers in order to call @mpz_export()@.
-- --
-- -- /Since: 0.5.1.0/
-- {-# NOINLINE exportIntegerToMutableByteArray #-}
-- exportIntegerToMutableByteArray :: Integer -> MutableByteArray# s -> Word# -> Int# -> State# s -> (# State# s, Word# #)
-- exportIntegerToMutableByteArray (J# s d) mba o e = exportIntegerToMutableByteArray# s d mba o e
-- exportIntegerToMutableByteArray j@(S# _) mba o e = exportIntegerToMutableByteArray (toBig j) mba o e -- TODO

-- -- | Dump 'Integer' (without sign) to @/addr/@ in base-256 representation.
-- --
-- -- @'exportIntegerToAddr' /addr/ /o/ /e/@
-- --
-- -- See description of 'exportIntegerToMutableByteArray' for more details.
-- --
-- -- /Since: 0.5.1.0/
-- {-# NOINLINE exportIntegerToAddr #-}
-- exportIntegerToAddr :: Integer -> Addr# -> Int# -> State# s -> (# State# s, Word# #)
-- exportIntegerToAddr (J# s d) addr o e = exportIntegerToAddr# s d addr o e
-- exportIntegerToAddr j@(S# _) addr o e = exportIntegerToAddr (toBig j) addr o e -- TODO

-- -- | Read 'Integer' (without sign) from byte-array in base-256 representation.
-- --
-- -- The call
-- --
-- -- @'importIntegerFromByteArray' /ba/ /offset/ /size/ /order/@
-- --
-- -- reads
-- --
-- -- * @/size/@ bytes from the 'ByteArray#' @/ba/@ starting at @/offset/@
-- --
-- -- * with most significant byte first if @/order/@ is @1#@ or least
-- --   significant byte first if @/order/@ is @-1#@, and
-- --
-- -- * returns a new 'Integer'
-- --
-- -- /Since: 0.5.1.0/
-- {-# NOINLINE importIntegerFromByteArray #-}
-- importIntegerFromByteArray :: ByteArray# -> Word# -> Word# -> Int# -> Integer
-- importIntegerFromByteArray ba o l e = smartJ# (importIntegerFromByteArray# ba o l e)

-- -- | Read 'Integer' (without sign) from memory location at @/addr/@ in
-- -- base-256 representation.
-- --
-- -- @'importIntegerFromAddr' /addr/ /size/ /order/@
-- --
-- -- See description of 'importIntegerFromByteArray' for more details.
-- --
-- -- /Since: 0.5.1.0/
-- {-# NOINLINE importIntegerFromAddr #-}
-- importIntegerFromAddr :: Addr# -> Word# -> Int# -> State# s -> (# State# s, Integer #)
-- importIntegerFromAddr addr l e st = case importIntegerFromAddr# addr l e st of
--                                       (# st', mpz #) -> let !j = smartJ# mpz in (# st', j #)

{-
%*********************************************************
%*                                                      *
\subsection{The @Integer@ hashing@}
%*                                                      *
%*********************************************************

-}
-- This is used by hashUnique

-- | 'hashInteger' returns the same value as 'fromIntegral', although in
-- unboxed form.  It might be a reasonable hash function for 'Integer',
-- given a suitable distribution of 'Integer' values.
--
-- Note: 'hashInteger' is currently just an alias for 'integerToInt'.

hashInteger :: Integer -> Int#
hashInteger = integerToInt
