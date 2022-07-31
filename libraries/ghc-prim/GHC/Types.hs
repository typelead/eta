{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples,
             RoleAnnotations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Types
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC type definitions.
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Types (
        Bool(..), Char(..), Int(..), Word(..),
        Float(..), Double(..),
        Ordering(..), IO(..),
        Java(..), Object(..), JString(..),
        CharSequence(..),
        Comparable(..),
        Object#,
        isTrue#,
        Nat, Symbol,
        SPEC(..),
        Coercible,
    ) where

import GHC.Prim

infixr 5 :

{- *********************************************************************
*                                                                      *
                  Nat and Symbol
*                                                                      *
********************************************************************* -}

-- | (Kind) This is the kind of type-level natural numbers.
data Nat

-- | (Kind) This is the kind of type-level symbols.
-- Declared here because class IP needs it
data Symbol


data [] a = [] | a : [a]

data Bool = False | True

{- | The character type 'Char' is an enumeration whose values represent
Unicode (or equivalently ISO\/IEC 10646) characters (see
<https://www.unicode.org/> for details).  This set extends the ISO 8859-1
(Latin-1) character set (the first 256 characters), which is itself an extension
of the ASCII character set (the first 128 characters).  A character literal in
Haskell has type 'Char'.

To convert a 'Char' to or from the corresponding 'Int' value defined
by Unicode, use 'Prelude.toEnum' and 'Prelude.fromEnum' from the
'Prelude.Enum' class respectively (or equivalently 'ord' and 'chr').
-}
data Char = C# Char#

-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.
data Int = I# Int#

-- |A 'Word' is an unsigned integral type, with the same size as 'Int'.
data Word = W# Word#

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
data Float = F# Float#

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
data Double = D# Double#

data Ordering = LT | EQ | GT

{- |
A value of type @'IO' a@ is a computation which, when performed,
does some I\/O before returning a value of type @a@.

There is really only one way to \"perform\" an I\/O action: bind it to
@Main.main@ in your program.  When your program is run, the I\/O will
be performed.  It isn't possible to perform I\/O from an arbitrary
function, unless that function is itself in the 'IO' monad and called
at some point, directly or indirectly, from @Main.main@.

'IO' is a monad, so 'IO' actions can be combined using either the do-notation
or the '>>' and '>>=' operations from the 'Monad' class.
-}
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
type role IO representational
{-
The above role annotation is redundant but is included because this role
is significant in the normalisation of FFI types. Specifically, if this
role were to become nominal (which would be very strange, indeed!), changes
elsewhere in GHC would be necessary. See [FFI type roles] in TcForeign.
-}

{-
Note [Kind-changing of (~) and Coercible]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(~) and Coercible are tricky to define. To the user, they must appear as
constraints, but we cannot define them as such in Haskell. But we also cannot
just define them only in GHC.Prim (like (->)), because we need a real module
for them, e.g. to compile the constructor's info table.

Furthermore the type of MkCoercible cannot be written in Haskell
(no syntax for ~#R).

So we define them as regular data types in GHC.Types, and do magic in TysWiredIn,
inside GHC, to change the kind and type.
-}


-- | A data constructor used to box up all unlifted equalities
--
-- The type constructor is special in that GHC pretends that it
-- has kind (? -> ? -> Fact) rather than (* -> * -> *)
data (~) a b = Eq# ((~#) a b)


-- | This two-parameter class has instances for types @a@ and @b@ if
--      the compiler can infer that they have the same representation. This class
--      does not have regular instances; instead they are created on-the-fly during
--      type-checking. Trying to manually declare an instance of @Coercible@
--      is an error.
--
--      Nevertheless one can pretend that the following three kinds of instances
--      exist. First, as a trivial base-case:
--
--      @instance a a@
--
--      Furthermore, for every type constructor there is
--      an instance that allows to coerce under the type constructor. For
--      example, let @D@ be a prototypical type constructor (@data@ or
--      @newtype@) with three type arguments, which have roles @nominal@,
--      @representational@ resp. @phantom@. Then there is an instance of
--      the form
--
--      @instance Coercible b b\' => Coercible (D a b c) (D a b\' c\')@
--
--      Note that the @nominal@ type arguments are equal, the
--      @representational@ type arguments can differ, but need to have a
--      @Coercible@ instance themself, and the @phantom@ type arguments can be
--      changed arbitrarily.
--
--      The third kind of instance exists for every @newtype NT = MkNT T@ and
--      comes in two variants, namely
--
--      @instance Coercible a T => Coercible a NT@
--
--      @instance Coercible T b => Coercible NT b@
--
--      This instance is only usable if the constructor @MkNT@ is in scope.
--
--      If, as a library author of a type constructor like @Set a@, you
--      want to prevent a user of your module to write
--      @coerce :: Set T -> Set NT@,
--      you need to set the role of @Set@\'s type parameter to @nominal@,
--      by writing
--
--      @type role Set nominal@
--
--      For more details about this feature, please refer to
--      <https://web.archive.org/web/20170426120538/http://cs.brynmawr.edu/~rae/papers/2014/coercible/coercible.pdf Safe Coercions>
--      by Joachim Breitner, Richard A. Eisenberg, Simon Peyton Jones and Stephanie Weirich.
--
--      @since 4.7.0.0
data Coercible a b = MkCoercible ((~#) a b)
-- It's really ~R# (representational equality), not ~#,
-- but  * we don't yet have syntax for ~R#,
--      * the compiled code is the same either way
--      * TysWiredIn has the truthful types
-- Also see Note [Kind-changing of (~) and Coercible]

-- | Alias for 'tagToEnum#'. Returns True if its parameter is 1# and False
--   if it is 0#.

{-# INLINE isTrue# #-}
isTrue# :: Int# -> Bool   -- See Note [Optimizing isTrue#]
isTrue# x = tagToEnum# x

-- Note [Optimizing isTrue#]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Current definition of isTrue# is a temporary workaround. We would like to
-- have functions isTrue# and isFalse# defined like this:
--
--     isTrue# :: Int# -> Bool
--     isTrue# 1# = True
--     isTrue# _  = False
--
--     isFalse# :: Int# -> Bool
--     isFalse# 0# = True
--     isFalse# _  = False
--
-- These functions would allow us to safely check if a tag can represent True
-- or False. Using isTrue# and isFalse# as defined above will not introduce
-- additional case into the code. When we scrutinize return value of isTrue#
-- or isFalse#, either explicitly in a case expression or implicitly in a guard,
-- the result will always be a single case expression (given that optimizations
-- are turned on). This results from case-of-case transformation. Consider this
-- code (this is both valid Haskell and Core):
--
-- case isTrue# (a ># b) of
--     True  -> e1
--     False -> e2
--
-- Inlining isTrue# gives:
--
-- case (case (a ># b) of { 1# -> True; _ -> False } ) of
--     True  -> e1
--     False -> e2
--
-- Case-of-case transforms that to:
--
-- case (a ># b) of
--   1# -> case True of
--           True  -> e1
--           False -> e2
--   _  -> case False of
--           True  -> e1
--           False -> e2
--
-- Which is then simplified by case-of-known-constructor:
--
-- case (a ># b) of
--   1# -> e1
--   _  -> e2
--
-- While we get good Core here, the code generator will generate very bad Cmm
-- if e1 or e2 do allocation. It will push heap checks into case alternatives
-- which results in about 2.5% increase in code size. Until this is improved we
-- just make isTrue# an alias to tagToEnum#. This is a temporary solution (if
-- you're reading this in 2023 then things went wrong). See #8326.
--

-- | 'SPEC' is used by GHC in the @SpecConstr@ pass in order to inform
-- the compiler when to be particularly aggressive. In particular, it
-- tells GHC to specialize regardless of size or the number of
-- specializations. However, not all loops fall into this category.
--
-- Libraries can specify this by using 'SPEC' data type to inform which
-- loops should be aggressively specialized.
data SPEC = SPEC | SPEC2

-- The Java Monad
newtype Java c a = Java (Object# c -> (# Object# c, a #))
type role Java nominal representational

data {-# CLASS "java.lang.Object" #-} Object  = O#  (Object# Object)
data {-# CLASS "java.lang.String" #-} JString = JS# (Object# JString)
data {-# CLASS "java.lang.CharSequence" #-} CharSequence = CS# (Object# CharSequence)
data {-# CLASS "java.lang.Comparable" #-} Comparable t = CT# (Object# (Comparable t))
