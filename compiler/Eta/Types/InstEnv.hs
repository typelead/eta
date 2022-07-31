{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to TcInstDcls and TcDeriv.
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Eta.Types.InstEnv (
        DFunId, InstMatch, ClsInstLookupResult,
        OverlapFlag(..), OverlapMode(..), setOverlapModeMaybe,
        ClsInst(..), DFunInstType, pprInstance, pprInstanceHdr, pprInstances,
        instanceHead, instanceSig, mkLocalInstance, mkImportedInstance,
        instanceDFunId, tidyClsInstDFun, instanceRoughTcs,
        fuzzyClsInstCmp, pruneMatches,


        InstEnvs(..), VisibleOrphanModules, InstEnv,
        emptyInstEnv, extendInstEnv, deleteFromInstEnv, identicalClsInstHead,
        extendInstEnvList, lookupUniqueInstEnv, lookupInstEnv', lookupInstEnv, instEnvElts,
        memberInstEnv, instIsVisible,
        classInstances, orphNamesOfClsInst, instanceBindFun,
        instanceCantMatch, roughMatchTcs
    ) where

#include "HsVersions.h"

import Eta.BasicTypes.Module
import Eta.Types.Class
import Eta.BasicTypes.Var
import Eta.BasicTypes.VarSet
import Eta.BasicTypes.Name
import Eta.BasicTypes.NameSet
import Eta.TypeCheck.TcType
import Eta.Types.TyCon
import Eta.Types.Unify
import Eta.Utils.Outputable
import Eta.Main.ErrUtils
import Eta.BasicTypes.BasicTypes
import Eta.Utils.UniqFM
import Eta.Utils.Util
import Eta.BasicTypes.Id
import Eta.Utils.FastString
import Eta.Core.CoreSyn
import Data.Data        ( Data, Typeable )
import Data.Maybe       ( isJust, isNothing )
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid
#endif

{-
************************************************************************
*                                                                      *
           ClsInst: the data type for type-class instances
*                                                                      *
************************************************************************
-}

data ClsInst
  = ClsInst {   -- Used for "rough matching"; see Note
                -- Note [ClsInst laziness and the rough-match fields]
                -- INVARIANT: is_tcs = roughMatchTcs is_tys
               is_cls_nm :: Name        -- ^ Class name
             , is_tcs  :: [Maybe Name]  -- ^ Top of type args

               -- | @is_dfun_name = idName . is_dfun@.
               --
               -- We use 'is_dfun_name' for the visibility check,
               -- 'instIsVisible', which needs to know the 'Module' which the
               -- dictionary is defined in. However, we cannot use the 'Module'
               -- attached to 'is_dfun' since doing so would mean we would
               -- potentially pull in an entire interface file unnecessarily.
               -- This was the cause of #12367.
             , is_dfun_name :: Name

                -- Used for "proper matching"; see Note [Proper-match fields]
             , is_tvs  :: [TyVar]       -- Fresh template tyvars for full match
                                        -- See Note [Template tyvars are fresh]
             , is_cls  :: Class         -- The real class
             , is_tys  :: [Type]        -- Full arg types (mentioning is_tvs)
                -- INVARIANT: is_dfun Id has type
                --      forall is_tvs. (...) => is_cls is_tys
                -- (modulo alpha conversion)

             , is_dfun :: DFunId -- See Note [Haddock assumptions]
                    -- See Note [Silent superclass arguments] in TcInstDcls
                    -- for how to map the DFun's type back to the source
                    -- language instance decl

             , is_flag :: OverlapFlag   -- See detailed comments with
                                        -- the decl of BasicTypes.OverlapFlag
             , is_orphan :: IsOrphan
    }
  deriving (Data, Typeable)

-- | A fuzzy comparison function for class instances, intended for sorting
-- instances before displaying them to the user.
fuzzyClsInstCmp :: ClsInst -> ClsInst -> Ordering
fuzzyClsInstCmp x y =
    stableNameCmp (is_cls_nm x) (is_cls_nm y) `mappend`
    mconcat (map cmp (zip (is_tcs x) (is_tcs y)))
  where
    cmp (Nothing, Nothing) = EQ
    cmp (Nothing, Just _) = LT
    cmp (Just _, Nothing) = GT
    cmp (Just x, Just y) = stableNameCmp x y

{-
Note [ClsInst laziness and the rough-match fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we load 'instance A.C B.T' from A.hi, but suppose that the type B.T is
otherwise unused in the program. Then it's stupid to load B.hi, the data type
declaration for B.T -- and perhaps further instance declarations!

We avoid this as follows:

* is_cls_nm, is_tcs, is_dfun_name are all Names. We can poke them to our heart's
  content.

* Proper-match fields. is_dfun, and its related fields is_tvs, is_cls, is_tys
  contain TyVars, Class, Type, Class etc, and so are all lazy thunks. When we
  poke any of these fields we'll typecheck the DFunId declaration, and hence
  pull in interfaces that it refers to. See Note [Proper-match fields].

* Rough-match fields. During instance lookup, we use the is_cls_nm :: Name and
  is_tcs :: [Maybe Name] fields to perform a "rough match", *without* poking
  inside the DFunId. The rough-match fields allow us to say "definitely does not
  match", based only on Names.

  This laziness is very important; see Trac #12367. Try hard to avoid pulling on
  the structured fields unless you really need the instance.

* Another place to watch is InstEnv.instIsVisible, which needs the module to
  which the ClsInst belongs. We can get this from is_dfun_name.

* In is_tcs,
    Nothing  means that this type arg is a type variable

    (Just n) means that this type arg is a
                TyConApp with a type constructor of n.
                This is always a real tycon, never a synonym!
                (Two different synonyms might match, but two
                different real tycons can't.)
                NB: newtypes are not transparent, though!

Note [Template tyvars are fresh]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The is_tvs field of a ClsInst has *completely fresh* tyvars.
That is, they are
  * distinct from any other ClsInst
  * distinct from any tyvars free in predicates that may
    be looked up in the class instance environment
Reason for freshness: we use unification when checking for overlap
etc, and that requires the tyvars to be distinct.

The invariant is checked by the ASSERT in lookupInstEnv'.

Note [Proper-match fields]
~~~~~~~~~~~~~~~~~~~~~~~~~
The is_tvs, is_cls, is_tys fields are simply cached values, pulled
out (lazily) from the dfun id. They are cached here simply so
that we don't need to decompose the DFunId each time we want
to match it.  The hope is that the rough-match fields mean
that we often never poke the proper-match fields.

However, note that:
 * is_tvs must be a superset of the free vars of is_tys

 * is_tvs, is_tys may be alpha-renamed compared to the ones in
   the dfun Id

Note [Haddock assumptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
For normal user-written instances, Haddock relies on

 * the SrcSpan of
 * the Name of
 * the is_dfun of
 * an Instance

being equal to

  * the SrcSpan of
  * the instance head type of
  * the InstDecl used to construct the Instance.
-}

instanceDFunId :: ClsInst -> DFunId
instanceDFunId = is_dfun

tidyClsInstDFun :: (DFunId -> DFunId) -> ClsInst -> ClsInst
tidyClsInstDFun tidy_dfun ispec
  = ispec { is_dfun = tidy_dfun (is_dfun ispec) }

instanceRoughTcs :: ClsInst -> [Maybe Name]
instanceRoughTcs = is_tcs

instance NamedThing ClsInst where
   getName ispec = getName (is_dfun ispec)

instance Outputable ClsInst where
   ppr = pprInstance

pprInstance :: ClsInst -> SDoc
-- Prints the ClsInst as an instance declaration
pprInstance ispec
  = hang (pprInstanceHdr ispec)
       2 (vcat [ ptext (sLit "--") <+> pprDefinedAt (getName ispec)
               , ifPprDebug (ppr (is_dfun ispec)) ])

-- * pprInstanceHdr is used in VStudio to populate the ClassView tree
pprInstanceHdr :: ClsInst -> SDoc
-- Prints the ClsInst as an instance declaration
pprInstanceHdr (ClsInst { is_flag = flag, is_dfun = dfun })
  = getPprStyle $ \ sty ->
    let dfun_ty = idType dfun
        (tvs, theta, res_ty) = tcSplitSigmaTy dfun_ty
        theta_to_print = drop (dfunNSilent dfun) theta
          -- See Note [Silent superclass arguments] in TcInstDcls
        ty_to_print | debugStyle sty = dfun_ty
                    | otherwise      = mkSigmaTy tvs theta_to_print res_ty
    in ptext (sLit "instance") <+> ppr flag <+> pprSigmaType ty_to_print

pprInstances :: [ClsInst] -> SDoc
pprInstances ispecs = vcat (map pprInstance ispecs)

instanceHead :: ClsInst -> ([TyVar], Class, [Type])
-- Returns the head, using the fresh tyavs from the ClsInst
instanceHead (ClsInst { is_tvs = tvs, is_tys = tys, is_dfun = dfun })
   = (tvs, cls, tys)
   where
     (_, _, cls, _) = tcSplitDFunTy (idType dfun)

instanceSig :: ClsInst -> ([TyVar], [Type], Class, [Type])
-- Decomposes the DFunId
instanceSig ispec = tcSplitDFunTy (idType (is_dfun ispec))

mkLocalInstance :: DFunId -> OverlapFlag
                -> [TyVar] -> Class -> [Type]
                -> ClsInst
-- Used for local instances, where we can safely pull on the DFunId
mkLocalInstance dfun oflag tvs cls tys
  = ClsInst { is_flag = oflag, is_dfun = dfun
            , is_tvs = tvs
            , is_dfun_name = dfun_name
            , is_cls = cls, is_cls_nm = cls_name
            , is_tys = tys, is_tcs = roughMatchTcs tys
            , is_orphan = orph
            }
  where
    cls_name = className cls
    dfun_name = idName dfun
    this_mod = ASSERT( isExternalName dfun_name ) nameModule dfun_name
    is_local name = nameIsLocalOrFrom this_mod name

        -- Compute orphanhood.  See Note [Orphans] in InstEnv
    (cls_tvs, fds) = classTvsFds cls
    arg_names = [filterNameSet is_local (orphNamesOfType ty) | ty <- tys]

    -- See Note [When exactly is an instance decl an orphan?]
    orph | is_local cls_name = NotOrphan (nameOccName cls_name)
         | all notOrphan mb_ns  = ASSERT( not (null mb_ns) ) head mb_ns
         | otherwise         = IsOrphan

    notOrphan NotOrphan{} = True
    notOrphan _ = False

    mb_ns :: [IsOrphan]    -- One for each fundep; a locally-defined name
                           -- that is not in the "determined" arguments
    mb_ns | null fds   = [choose_one arg_names]
          | otherwise  = map do_one fds
    do_one (_ltvs, rtvs) = choose_one [ns | (tv,ns) <- cls_tvs `zip` arg_names
                                          , not (tv `elem` rtvs)]

    choose_one :: [NameSet] -> IsOrphan
    choose_one nss = case local_names of
                       []      -> IsOrphan
                       (_ : _) -> NotOrphan anchor
       where
       local_names = nameSetElems (unionNameSets nss)
       anchor = minimum $ map nameOccName local_names

mkImportedInstance :: Name         -- ^ the name of the class
                   -> [Maybe Name] -- ^ the types which the class was applied to
                   -> Name         -- ^ the 'Name' of the dictionary binding
                   -> DFunId       -- ^ the 'Id' of the dictionary.
                   -> OverlapFlag  -- ^ may this instance overlap?
                   -> IsOrphan     -- ^ is this instance an orphan?
                   -> ClsInst
-- Used for imported instances, where we get the rough-match stuff
-- from the interface file
-- The bound tyvars of the dfun are guaranteed fresh, because
-- the dfun has been typechecked out of the same interface file
mkImportedInstance cls_nm mb_tcs dfun_name dfun oflag orphan
  = ClsInst { is_flag = oflag, is_dfun = dfun
            , is_tvs = tvs, is_tys = tys
            , is_dfun_name = dfun_name
            , is_cls_nm = cls_nm, is_cls = cls, is_tcs = mb_tcs
            , is_orphan = orphan }
  where
    (tvs, _, cls, tys) = tcSplitDFunTy (idType dfun)

roughMatchTcs :: [Type] -> [Maybe Name]
roughMatchTcs tys = map rough tys
  where
    rough ty = case tcSplitTyConApp_maybe ty of
                  Just (tc,_) -> Just (tyConName tc)
                  Nothing     -> Nothing

instanceCantMatch :: [Maybe Name] -> [Maybe Name] -> Bool
-- (instanceCantMatch tcs1 tcs2) returns True if tcs1 cannot
-- possibly be instantiated to actual, nor vice versa;
-- False is non-committal
instanceCantMatch (Just t : ts) (Just a : as) = t/=a || instanceCantMatch ts as
instanceCantMatch _             _             =  False  -- Safe

{-

************************************************************************
*                                                                      *
                InstEnv, ClsInstEnv
*                                                                      *
************************************************************************

A @ClsInstEnv@ all the instances of that class.  The @Id@ inside a
ClsInstEnv mapping is the dfun for that instance.

If class C maps to a list containing the item ([a,b], [t1,t2,t3], dfun), then

        forall a b, C t1 t2 t3  can be constructed by dfun

or, to put it another way, we have

        instance (...) => C t1 t2 t3,  witnessed by dfun
-}

---------------------------------------------------
type InstEnv = UniqFM ClsInstEnv        -- Maps Class to instances for that class

-- | 'InstEnvs' represents the combination of the global type class instance
-- environment, the local type class instance environment, and the set of
-- transitively reachable orphan modules (according to what modules have been
-- directly imported) used to test orphan instance visibility.
data InstEnvs = InstEnvs {
        ie_global  :: InstEnv,               -- External-package instances
        ie_local   :: InstEnv,               -- Home-package instances
        ie_visible :: VisibleOrphanModules   -- Set of all orphan modules transitively
                                             -- reachable from the module being compiled
                                             -- See Note [Instance lookup and orphan instances]
    }

-- | Set of visible orphan modules, according to what modules have been directly
-- imported.  This is based off of the dep_orphs field, which records
-- transitively reachable orphan modules (modules that define orphan instances).
type VisibleOrphanModules = ModuleSet

newtype ClsInstEnv
  = ClsIE [ClsInst]    -- The instances for a particular class, in any order

instance Outputable ClsInstEnv where
  ppr (ClsIE is) = pprInstances is

-- INVARIANTS:
--  * The is_tvs are distinct in each ClsInst
--      of a ClsInstEnv (so we can safely unify them)

-- Thus, the @ClassInstEnv@ for @Eq@ might contain the following entry:
--      [a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
-- The "a" in the pattern must be one of the forall'd variables in
-- the dfun type.

emptyInstEnv :: InstEnv
emptyInstEnv = emptyUFM

instEnvElts :: InstEnv -> [ClsInst]
instEnvElts ie = [elt | ClsIE elts <- eltsUFM ie, elt <- elts]

-- | Test if an instance is visible, by checking that its origin module
-- is in 'VisibleOrphanModules'.
-- See Note [Instance lookup and orphan instances]
instIsVisible :: VisibleOrphanModules -> ClsInst -> Bool
instIsVisible vis_mods ispec
  -- NB: Instances from the interactive package always are visible. We can't
  -- add interactive modules to the set since we keep creating new ones
  -- as a GHCi session progresses.
  | isInteractiveModule mod     = True
  | IsOrphan <- is_orphan ispec = mod `elemModuleSet` vis_mods
  | otherwise                   = True
  where
    mod = nameModule $ is_dfun_name ispec

classInstances :: InstEnvs -> Class -> [ClsInst]
classInstances (InstEnvs { ie_global = pkg_ie, ie_local = home_ie, ie_visible = vis_mods }) cls
  = get home_ie ++ get pkg_ie
  where
    get env = case lookupUFM env cls of
                Just (ClsIE insts) -> filter (instIsVisible vis_mods) insts
                Nothing            -> []

-- | Collects the names of concrete types and type constructors that make
-- up the head of a class instance. For instance, given `class Foo a b`:
--
-- `instance Foo (Either (Maybe Int) a) Bool` would yield
--      [Either, Maybe, Int, Bool]
--
-- Used in the implementation of ":info" in GHCi.
orphNamesOfClsInst :: ClsInst -> NameSet
orphNamesOfClsInst = orphNamesOfDFunHead . idType . instanceDFunId

-- | Checks for an exact match of ClsInst in the instance environment.
-- We use this when we do signature checking in TcRnDriver
-- TODO: This will report that Show [Foo] is a member of an
-- instance environment containing Show a => Show [a], even if
-- Show Foo is not in the environment.  Could try to make this
-- a bit more precise.

memberInstEnv :: InstEnv -> ClsInst -> Bool
memberInstEnv inst_env ins_item@(ClsInst { is_cls_nm = cls_nm } ) =
    maybe False (\(ClsIE items) -> any (identicalClsInstHead ins_item) items)
          (lookupUFM inst_env cls_nm)

extendInstEnvList :: InstEnv -> [ClsInst] -> InstEnv
extendInstEnvList inst_env ispecs = foldl extendInstEnv inst_env ispecs

extendInstEnv :: InstEnv -> ClsInst -> InstEnv
extendInstEnv inst_env ins_item@(ClsInst { is_cls_nm = cls_nm })
  = addToUFM_C add inst_env cls_nm (ClsIE [ins_item])
  where
    add (ClsIE cur_insts) _ = ClsIE (ins_item : cur_insts)

deleteFromInstEnv :: InstEnv -> ClsInst -> InstEnv
deleteFromInstEnv inst_env ins_item@(ClsInst { is_cls_nm = cls_nm })
  = adjustUFM adjust inst_env cls_nm
  where
    adjust (ClsIE items) = ClsIE (filterOut (identicalClsInstHead ins_item) items)

identicalClsInstHead :: ClsInst -> ClsInst -> Bool
-- ^ True when when the instance heads are the same
-- e.g.  both are   Eq [(a,b)]
-- Used for overriding in GHCi
-- Obviously should be insenstive to alpha-renaming
identicalClsInstHead (ClsInst { is_cls_nm = cls_nm1, is_tcs = rough1, is_tvs = tvs1, is_tys = tys1 })
                     (ClsInst { is_cls_nm = cls_nm2, is_tcs = rough2, is_tvs = tvs2, is_tys = tys2 })
  =  cls_nm1 == cls_nm2
  && not (instanceCantMatch rough1 rough2)  -- Fast check for no match, uses the "rough match" fields
  && isJust (tcMatchTys (mkVarSet tvs1) tys1 tys2)
  && isJust (tcMatchTys (mkVarSet tvs2) tys2 tys1)

{-
************************************************************************
*                                                                      *
        Looking up an instance
*                                                                      *
************************************************************************

@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since
the env is kept ordered, the first match must be the only one.  The
thing we are looking up can have an arbitrary "flexi" part.

Note [Instance lookup and orphan instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are compiling a module M, and we have a zillion packages
loaded, and we are looking up an instance for C (T W).  If we find a
match in module 'X' from package 'p', should be "in scope"; that is,

  is p:X in the transitive closure of modules imported from M?

The difficulty is that the "zillion packages" might include ones loaded
through earlier invocations of the GHC API, or earlier module loads in GHCi.
They might not be in the dependencies of M itself; and if not, the instances
in them should not be visible.  Trac #2182, #8427.

There are two cases:
  * If the instance is *not an orphan*, then module X defines C, T, or W.
    And in order for those types to be involved in typechecking M, it
    must be that X is in the transitive closure of M's imports.  So we
    can use the instance.

  * If the instance *is an orphan*, the above reasoning does not apply.
    So we keep track of the set of orphan modules transitively below M;
    this is the ie_visible field of InstEnvs, of type VisibleOrphanModules.

    If module p:X is in this set, then we can use the instance, otherwise
    we can't.

Note [Rules for instance lookup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These functions implement the carefully-written rules in the user
manual section on "overlapping instances". At risk of duplication,
here are the rules.  If the rules change, change this text and the
user manual simultaneously.  The link may be this:
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-class-extensions.html#instance-overlap

The willingness to be overlapped or incoherent is a property of the
instance declaration itself, controlled as follows:

 * An instance is "incoherent"
   if it has an INCOHERENT pragma, or
   if it appears in a module compiled with -XIncoherentInstances.

 * An instance is "overlappable"
   if it has an OVERLAPPABLE or OVERLAPS pragma, or
   if it appears in a module compiled with -XOverlappingInstances, or
   if the instance is incoherent.

 * An instance is "overlapping"
   if it has an OVERLAPPING or OVERLAPS pragma, or
   if it appears in a module compiled with -XOverlappingInstances, or
   if the instance is incoherent.
     compiled with -XOverlappingInstances.

Now suppose that, in some client module, we are searching for an instance
of the target constraint (C ty1 .. tyn). The search works like this.

 * Find all instances I that match the target constraint; that is, the
   target constraint is a substitution instance of I. These instance
   declarations are the candidates.

 * Find all non-candidate instances that unify with the target
   constraint. Such non-candidates instances might match when the
   target constraint is further instantiated. If all of them are
   incoherent, proceed; if not, the search fails.

 * Eliminate any candidate IX for which both of the following hold:
   * There is another candidate IY that is strictly more specific;
     that is, IY is a substitution instance of IX but not vice versa.

   * Either IX is overlappable or IY is overlapping.

 * If only one candidate remains, pick it. Otherwise if all remaining
   candidates are incoherent, pick an arbitrary candidate. Otherwise fail.

Note [Overlapping instances]   (NB: these notes are quite old)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Overlap is permitted, but only in such a way that one can make
a unique choice when looking up.  That is, overlap is only permitted if
one template matches the other, or vice versa.  So this is ok:

  [a]  [Int]

but this is not

  (Int,a)  (b,Int)

If overlap is permitted, the list is kept most specific first, so that
the first lookup is the right choice.


For now we just use association lists.

\subsection{Avoiding a problem with overlapping}

Consider this little program:

\begin{pseudocode}
     class C a        where c :: a
     class C a => D a where d :: a

     instance C Int where c = 17
     instance D Int where d = 13

     instance C a => C [a] where c = [c]
     instance ({- C [a], -} D a) => D [a] where d = c

     instance C [Int] where c = [37]

     main = print (d :: [Int])
\end{pseudocode}

What do you think `main' prints  (assuming we have overlapping instances, and
all that turned on)?  Well, the instance for `D' at type `[a]' is defined to
be `c' at the same type, and we've got an instance of `C' at `[Int]', so the
answer is `[37]', right? (the generic `C [a]' instance shouldn't apply because
the `C [Int]' instance is more specific).

Ghc-4.04 gives `[37]', while ghc-4.06 gives `[17]', so 4.06 is wrong.  That
was easy ;-)  Let's just consult hugs for good measure.  Wait - if I use old
hugs (pre-September99), I get `[17]', and stranger yet, if I use hugs98, it
doesn't even compile!  What's going on!?

What hugs complains about is the `D [a]' instance decl.

\begin{pseudocode}
     ERROR "mj.hs" (line 10): Cannot build superclass instance
     *** Instance            : D [a]
     *** Context supplied    : D a
     *** Required superclass : C [a]
\end{pseudocode}

You might wonder what hugs is complaining about.  It's saying that you
need to add `C [a]' to the context of the `D [a]' instance (as appears
in comments).  But there's that `C [a]' instance decl one line above
that says that I can reduce the need for a `C [a]' instance to the
need for a `C a' instance, and in this case, I already have the
necessary `C a' instance (since we have `D a' explicitly in the
context, and `C' is a superclass of `D').

Unfortunately, the above reasoning indicates a premature commitment to the
generic `C [a]' instance.  I.e., it prematurely rules out the more specific
instance `C [Int]'.  This is the mistake that ghc-4.06 makes.  The fix is to
add the context that hugs suggests (uncomment the `C [a]'), effectively
deferring the decision about which instance to use.

Now, interestingly enough, 4.04 has this same bug, but it's covered up
in this case by a little known `optimization' that was disabled in
4.06.  Ghc-4.04 silently inserts any missing superclass context into
an instance declaration.  In this case, it silently inserts the `C
[a]', and everything happens to work out.

(See `basicTypes/MkId:mkDictFunId' for the code in question.  Search for
`Mark Jones', although Mark claims no credit for the `optimization' in
question, and would rather it stopped being called the `Mark Jones
optimization' ;-)

So, what's the fix?  I think hugs has it right.  Here's why.  Let's try
something else out with ghc-4.04.  Let's add the following line:

    d' :: D a => [a]
    d' = c

Everyone raise their hand who thinks that `d :: [Int]' should give a
different answer from `d' :: [Int]'.  Well, in ghc-4.04, it does.  The
`optimization' only applies to instance decls, not to regular
bindings, giving inconsistent behavior.

Old hugs had this same bug.  Here's how we fixed it: like GHC, the
list of instances for a given class is ordered, so that more specific
instances come before more generic ones.  For example, the instance
list for C might contain:
    ..., C Int, ..., C a, ...
When we go to look for a `C Int' instance we'll get that one first.
But what if we go looking for a `C b' (`b' is unconstrained)?  We'll
pass the `C Int' instance, and keep going.  But if `b' is
unconstrained, then we don't know yet if the more specific instance
will eventually apply.  GHC keeps going, and matches on the generic `C
a'.  The fix is to, at each step, check to see if there's a reverse
match, and if so, abort the search.  This prevents hugs from
prematurely chosing a generic instance when a more specific one
exists.

--Jeff
v
BUT NOTE [Nov 2001]: we must actually *unify* not reverse-match in
this test.  Suppose the instance envt had
    ..., forall a b. C a a b, ..., forall a b c. C a b c, ...
(still most specific first)
Now suppose we are looking for (C x y Int), where x and y are unconstrained.
        C x y Int  doesn't match the template {a,b} C a a b
but neither does
        C a a b  match the template {x,y} C x y Int
But still x and y might subsequently be unified so they *do* match.

Simple story: unify, don't match.
-}

type DFunInstType = Maybe Type
        -- Just ty   => Instantiate with this type
        -- Nothing   => Instantiate with any type of this tyvar's kind
        -- See Note [DFunInstType: instantiating types]

type InstMatch = (ClsInst, [DFunInstType])

type ClsInstLookupResult
     = ( [InstMatch]     -- Successful matches
       , [ClsInst]       -- These don't match but do unify
       , Bool)           -- True if error condition caused by
                         -- SafeHaskell condition.

{-
Note [DFunInstType: instantiating types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A successful match is a ClsInst, together with the types at which
        the dfun_id in the ClsInst should be instantiated
The instantiating types are (Either TyVar Type)s because the dfun
might have some tyvars that *only* appear in arguments
        dfun :: forall a b. C a b, Ord b => D [a]
When we match this against D [ty], we return the instantiating types
        [Just ty, Nothing]
where the 'Nothing' indicates that 'b' can be freely instantiated.
(The caller instantiates it to a flexi type variable, which will
 presumably later become fixed via functional dependencies.)
-}

-- |Look up an instance in the given instance environment. The given class application must match exactly
-- one instance and the match may not contain any flexi type variables.  If the lookup is unsuccessful,
-- yield 'Left errorMessage'.
--
lookupUniqueInstEnv :: InstEnvs
                    -> Class -> [Type]
                    -> Either MsgDoc (ClsInst, [Type])
lookupUniqueInstEnv instEnv cls tys
  = case lookupInstEnv instEnv cls tys of
      ([(inst, inst_tys)], _, _)
             | noFlexiVar -> Right (inst, inst_tys')
             | otherwise  -> Left $ ptext (sLit "flexible type variable:") <+>
                                    (ppr $ mkTyConApp (classTyCon cls) tys)
             where
               inst_tys'  = [ty | Just ty <- inst_tys]
               noFlexiVar = all isJust inst_tys
      _other -> Left $ ptext (sLit "instance not found") <+> (ppr $ mkTyConApp (classTyCon cls) tys)

lookupInstEnv' :: InstEnv          -- InstEnv to look in
               -> VisibleOrphanModules   -- But filter against this
               -> Class -> [Type]  -- What we are looking for
               -> ([InstMatch],    -- Successful matches
                   [ClsInst])     -- These don't match but do unify
-- The second component of the result pair happens when we look up
--      Foo [a]
-- in an InstEnv that has entries for
--      Foo [Int]
--      Foo [b]
-- Then which we choose would depend on the way in which 'a'
-- is instantiated.  So we report that Foo [b] is a match (mapping b->a)
-- but Foo [Int] is a unifier.  This gives the caller a better chance of
-- giving a suitable error message

lookupInstEnv' ie vis_mods cls tys
  = lookup ie
  where
    rough_tcs  = roughMatchTcs tys
    all_tvs    = all isNothing rough_tcs
    --------------
    lookup env = case lookupUFM env cls of
                   Nothing -> ([],[])   -- No instances for this class
                   Just (ClsIE insts) -> find [] [] insts

    --------------
    find ms us [] = (ms, us)
    find ms us (item@(ClsInst { is_tcs = mb_tcs, is_tvs = tpl_tvs
                              , is_tys = tpl_tys, is_flag = oflag }) : rest)
      | not (instIsVisible vis_mods item)
      = find ms us rest  -- See Note [Instance lookup and orphan instances]

        -- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find ms us rest

      | Just subst <- tcMatchTys tpl_tv_set tpl_tys tys
      = find ((item, map (lookup_tv subst) tpl_tvs) : ms) us rest

        -- Does not match, so next check whether the things unify
        -- See Note [Overlapping instances] and Note [Incoherent instances]
      | Incoherent _ <- overlapMode oflag
      = find ms us rest

      | otherwise
      = ASSERT2( tyVarsOfTypes tys `disjointVarSet` tpl_tv_set,
                 (ppr cls <+> ppr tys <+> ppr all_tvs) $$
                 (ppr tpl_tvs <+> ppr tpl_tys)
                )
                -- Unification will break badly if the variables overlap
                -- They shouldn't because we allocate separate uniques for them
                -- See Note [Template tyvars are fresh]
        case tcUnifyTys instanceBindFun tpl_tys tys of
            Just _   -> find ms (item:us) rest
            Nothing  -> find ms us        rest
      where
        tpl_tv_set = mkVarSet tpl_tvs

    ----------------
    lookup_tv :: TvSubst -> TyVar -> DFunInstType
        -- See Note [DFunInstType: instantiating types]
    lookup_tv subst tv = case lookupTyVar subst tv of
                                Just ty -> Just ty
                                Nothing -> Nothing

---------------
-- This is the common way to call this function.
lookupInstEnv :: InstEnvs     -- External and home package inst-env
              -> Class -> [Type]   -- What we are looking for
              -> ClsInstLookupResult
-- ^ See Note [Rules for instance lookup]
lookupInstEnv (InstEnvs { ie_global = pkg_ie, ie_local = home_ie, ie_visible = vis_mods }) cls tys
  = (final_matches, final_unifs, safe_fail)
  where
    (home_matches, home_unifs) = lookupInstEnv' home_ie vis_mods cls tys
    (pkg_matches,  pkg_unifs)  = lookupInstEnv' pkg_ie  vis_mods cls tys
    all_matches = home_matches ++ pkg_matches
    all_unifs   = home_unifs   ++ pkg_unifs
    (final_matches, safe_fail) = pruneMatches fst all_matches

    -- If the selected match is incoherent, discard all unifiers
    final_unifs = case final_matches of
                    (m:_) | is_incoherent (fst m) -> []
                    _ -> all_unifs

pruneMatches :: (a -> ClsInst) -> [a] -> ([a], Bool)
pruneMatches f all_matches =
  case pruned_matches of
    [match] -> check_safe match (f match) all_matches
    _       -> (pruned_matches, False)
  where
    pruned_matches = foldr (insert_overlapping f) [] all_matches
        -- Even if the unifs is non-empty (an error situation)
        -- we still prune the matches, so that the error message isn't
        -- misleading (complaining of multiple matches when some should be
        -- overlapped away)

    -- NOTE [Safe Haskell isSafeOverlap]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- We restrict code compiled in 'Safe' mode from overriding code
    -- compiled in any other mode. The rationale is that code compiled
    -- in 'Safe' mode is code that is untrusted by the ghc user. So
    -- we shouldn't let that code change the behaviour of code the
    -- user didn't compile in 'Safe' mode since that's the code they
    -- trust. So 'Safe' instances can only overlap instances from the
    -- same module. A same instance origin policy for safe compiled
    -- instances.
    check_safe match inst others
        = case isSafeOverlap (is_flag inst) of
                -- most specific isn't from a Safe module so OK
                False -> ([match], False)
                -- otherwise we make sure it only overlaps instances from
                -- the same module
                True -> (go [] others, True)
        where
            go bad [] = match:bad
            go bad (i:unchecked) =
                if inSameMod x
                    then go bad unchecked
                    else go (i:bad) unchecked
              where x = f i

            inSameMod b =
                let na = getName $ getName inst
                    la = isInternalName na
                    nb = getName $ getName b
                    lb = isInternalName nb
                in (la && lb) || (nameModule na == nameModule nb)

---------------
is_incoherent :: ClsInst -> Bool
is_incoherent inst =
  case overlapMode (is_flag inst) of
    Incoherent _ -> True
    _            -> False

---------------
insert_overlapping :: (a -> ClsInst) -> a -> [a] -> [a]
-- ^ Add a new solution, knocking out strictly less specific ones
-- See Note [Rules for instance lookup]
insert_overlapping _f new_item [] = [new_item]
insert_overlapping f new_item (old_item : old_items)
  | new_beats_old        -- New strictly overrides old
  , not old_beats_new
  , new_item `can_override` old_item
  = insert_overlapping f new_item old_items

  | old_beats_new        -- Old strictly overrides new
  , not new_beats_old
  , old_item `can_override` new_item
  = old_item : old_items

  -- Discard incoherent instances; see Note [Incoherent instances]
  | is_incoherent (f old_item)       -- Old is incoherent; discard it
  = insert_overlapping f new_item old_items
  | is_incoherent (f new_item)       -- New is incoherent; discard it
  = old_item : old_items

  -- Equal or incomparable, and neither is incoherent; keep both
  | otherwise
  = old_item : insert_overlapping f new_item old_items
  where

    new_beats_old = new_item `more_specific_than` old_item
    old_beats_new = old_item `more_specific_than` new_item

    -- `instB` can be instantiated to match `instA`
    -- or the two are equal
    matchA `more_specific_than` matchB
      = isJust (tcMatchTys (mkVarSet (is_tvs instB))
               (is_tys instB) (is_tys (f matchA)))
      where instB = f matchB

    matchA `can_override` matchB
       =  hasOverlappingFlag  (overlapMode (is_flag (f matchA)))
       || hasOverlappableFlag (overlapMode (is_flag (f matchB)))
       -- Overlap permitted if either the more specific instance
       -- is marked as overlapping, or the more general one is
       -- marked as overlappable.
       -- Latest change described in: Trac #9242.
       -- Previous change: Trac #3877, Dec 10.

{-
Note [Incoherent instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
For some classes, the choice of a particular instance does not matter, any one
is good. E.g. consider

        class D a b where { opD :: a -> b -> String }
        instance D Int b where ...
        instance D a Int where ...

        g (x::Int) = opD x x  -- Wanted: D Int Int

For such classes this should work (without having to add an "instance D Int
Int", and using -XOverlappingInstances, which would then work). This is what
-XIncoherentInstances is for: Telling GHC "I don't care which instance you use;
if you can use one, use it."

Should this logic only work when *all* candidates have the incoherent flag, or
even when all but one have it? The right choice is the latter, which can be
justified by comparing the behaviour with how -XIncoherentInstances worked when
it was only about the unify-check (note [Overlapping instances]):

Example:
        class C a b c where foo :: (a,b,c)
        instance C [a] b Int
        instance [incoherent] [Int] b c
        instance [incoherent] C a Int c
Thanks to the incoherent flags,
        [Wanted]  C [a] b Int
works: Only instance one matches, the others just unify, but are marked
incoherent.

So I can write
        (foo :: ([a],b,Int)) :: ([Int], Int, Int).
but if that works then I really want to be able to write
        foo :: ([Int], Int, Int)
as well. Now all three instances from above match. None is more specific than
another, so none is ruled out by the normal overlapping rules. One of them is
not incoherent, but we still want this to compile. Hence the
"all-but-one-logic".

The implementation is in insert_overlapping, where we remove matching
incoherent instances as long as there are are others.



************************************************************************
*                                                                      *
        Binding decisions
*                                                                      *
************************************************************************
-}

instanceBindFun :: TyVar -> BindFlag
instanceBindFun tv | isTcTyVar tv && isOverlappableTyVar tv = Skolem
                   | otherwise                              = BindMe
   -- Note [Binding when looking up instances]

{-
Note [Binding when looking up instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When looking up in the instance environment, or family-instance environment,
we are careful about multiple matches, as described above in
Note [Overlapping instances]

The key_tys can contain skolem constants, and we can guarantee that those
are never going to be instantiated to anything, so we should not involve
them in the unification test.  Example:
        class Foo a where { op :: a -> Int }
        instance Foo a => Foo [a]       -- NB overlap
        instance Foo [Int]              -- NB overlap
        data T = forall a. Foo a => MkT a
        f :: T -> Int
        f (MkT x) = op [x,x]
The op [x,x] means we need (Foo [a]).  Without the filterVarSet we'd
complain, saying that the choice of instance depended on the instantiation
of 'a'; but of course it isn't *going* to be instantiated.

We do this only for isOverlappableTyVar skolems.  For example we reject
        g :: forall a => [a] -> Int
        g x = op x
on the grounds that the correct instance depends on the instantiation of 'a'
-}
