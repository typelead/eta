{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Handles @deriving@ clauses on @data@ declarations.
-}
{-# LANGUAGE CPP #-}

module Eta.TypeCheck.TcDeriv ( tcDeriving ) where

import Eta.HsSyn.HsSyn
import Eta.Main.DynFlags
import qualified Eta.LanguageExtensions as LangExt
import Eta.TypeCheck.TcRnMonad
import Eta.TypeCheck.FamInst
import Eta.TypeCheck.TcErrors( reportAllUnsolved )
import Eta.TypeCheck.TcValidity( validDerivPred )
import Eta.TypeCheck.TcEnv
import Eta.TypeCheck.TcTyClsDecls( tcFamTyPats, famTyConShape, tcAddDataFamInstCtxt, kcDataDefn )
import Eta.TypeCheck.TcClassDcl( tcAddDeclCtxt )      -- Small helper
import Eta.TypeCheck.TcGenDeriv                       -- Deriv stuff
import Eta.TypeCheck.TcGenGenerics
import Eta.Types.InstEnv
import Eta.TypeCheck.Inst
import Eta.Types.FamInstEnv
import Eta.TypeCheck.TcHsType
import Eta.TypeCheck.TcMType
import Eta.TypeCheck.TcSimplify
import Eta.Iface.LoadIface( loadInterfaceForName )
import Eta.BasicTypes.Module( getModule )
import Eta.Rename.RnNames( extendGlobalRdrEnvRn )
import Eta.Rename.RnBinds
import Eta.Rename.RnEnv
import Eta.Rename.RnSource   ( addTcgDUs )
import Eta.Main.HscTypes
import Eta.BasicTypes.Avail

import Eta.Types.Unify( tcUnifyTy )
import Eta.Types.Class
import Eta.Types.Type
import Eta.Main.ErrUtils
import Eta.BasicTypes.DataCon
import Eta.Utils.Maybes
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.Name
import Eta.BasicTypes.NameSet
import Eta.Types.TyCon
import Eta.TypeCheck.TcType
import qualified Eta.TypeCheck.TcType as TcType
-- import Eta.BasicTypes.Var
import qualified Eta.BasicTypes.Var as Var
import Eta.BasicTypes.VarSet
import Eta.Prelude.PrelNames
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Util
import Eta.Utils.Outputable
import qualified Eta.Utils.Outputable as Outputable
import Eta.Utils.FastString
import Eta.Utils.Bag
import Eta.Utils.Pair

import Control.Monad
import Data.List

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
                Overview
*                                                                      *
************************************************************************

Overall plan
~~~~~~~~~~~~
1.  Convert the decls (i.e. data/newtype deriving clauses,
    plus standalone deriving) to [EarlyDerivSpec]

2.  Infer the missing contexts for the InferTheta's

3.  Add the derived bindings, generating InstInfos
-}

-- DerivSpec is purely  local to this module
data DerivSpec theta = DS { ds_loc     :: SrcSpan
                          , ds_name    :: Name           -- DFun name
                          , ds_tvs     :: [TyVar]
                          , ds_theta   :: theta
                          , ds_cls     :: Class
                          , ds_tys     :: [Type]
                          , ds_tc      :: TyCon
                          , ds_tc_args :: [Type]
                          , ds_overlap :: Maybe OverlapMode
                          , ds_newtype :: Bool }
        -- This spec implies a dfun declaration of the form
        --       df :: forall tvs. theta => C tys
        -- The Name is the name for the DFun we'll build
        -- The tyvars bind all the variables in the theta
        -- For type families, the tycon in
        --       in ds_tys is the *family* tycon
        --       in ds_tc, ds_tc_args is the *representation* tycon
        -- For non-family tycons, both are the same

        -- the theta is either the given and final theta, in standalone deriving,
        -- or the not-yet-simplified list of constraints together with their origin

        -- ds_newtype = True  <=> Generalised Newtype Deriving (GND)
        --              False <=> Vanilla deriving

{-
Example:

     newtype instance T [a] = MkT (Tree a) deriving( C s )
==>
     axiom T [a] = :RTList a
     axiom :RTList a = Tree a

     DS { ds_tvs = [a,s], ds_cls = C, ds_tys = [s, T [a]]
        , ds_tc = :RTList, ds_tc_args = [a]
        , ds_newtype = True }
-}

type DerivContext = Maybe ThetaType
   -- Nothing    <=> Vanilla deriving; infer the context of the instance decl
   -- Just theta <=> Standalone deriving: context supplied by programmer

data PredOrigin = PredOrigin PredType CtOrigin
type ThetaOrigin = [PredOrigin]

mkPredOrigin :: CtOrigin -> PredType -> PredOrigin
mkPredOrigin origin pred = PredOrigin pred origin

mkThetaOrigin :: CtOrigin -> ThetaType -> ThetaOrigin
mkThetaOrigin origin = map (mkPredOrigin origin)

data EarlyDerivSpec = InferTheta (DerivSpec ThetaOrigin)
                    | GivenTheta (DerivSpec ThetaType)
        -- InferTheta ds => the context for the instance should be inferred
        --      In this case ds_theta is the list of all the constraints
        --      needed, such as (Eq [a], Eq a), together with a suitable CtLoc
        --      to get good error messages.
        --      The inference process is to reduce this to a simpler form (e.g.
        --      Eq a)
        --
        -- GivenTheta ds => the exact context for the instance is supplied
        --                  by the programmer; it is ds_theta

forgetTheta :: EarlyDerivSpec -> DerivSpec ()
forgetTheta (InferTheta spec) = spec { ds_theta = () }
forgetTheta (GivenTheta spec) = spec { ds_theta = () }

earlyDSLoc :: EarlyDerivSpec -> SrcSpan
earlyDSLoc (InferTheta spec) = ds_loc spec
earlyDSLoc (GivenTheta spec) = ds_loc spec

splitEarlyDerivSpec :: [EarlyDerivSpec] -> ([DerivSpec ThetaOrigin], [DerivSpec ThetaType])
splitEarlyDerivSpec [] = ([],[])
splitEarlyDerivSpec (InferTheta spec : specs) =
    case splitEarlyDerivSpec specs of (is, gs) -> (spec : is, gs)
splitEarlyDerivSpec (GivenTheta spec : specs) =
    case splitEarlyDerivSpec specs of (is, gs) -> (is, spec : gs)

pprDerivSpec :: Outputable theta => DerivSpec theta -> SDoc
pprDerivSpec (DS { ds_loc = l, ds_name = n, ds_tvs = tvs,
                   ds_cls = c, ds_tys = tys, ds_theta = rhs })
  = parens (hsep [ppr l, ppr n, ppr tvs, ppr c, ppr tys]
            <+> equals <+> ppr rhs)

instance Outputable theta => Outputable (DerivSpec theta) where
  ppr = pprDerivSpec

instance Outputable EarlyDerivSpec where
  ppr (InferTheta spec) = ppr spec <+> ptext (sLit "(Infer)")
  ppr (GivenTheta spec) = ppr spec <+> ptext (sLit "(Given)")

instance Outputable PredOrigin where
  ppr (PredOrigin ty _) = ppr ty -- The origin is not so interesting when debugging

{-
Inferring missing contexts
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

        data T a b = C1 (Foo a) (Bar b)
                   | C2 Int (T b a)
                   | C3 (T a a)
                   deriving (Eq)

[NOTE: See end of these comments for what to do with
        data (C a, D b) => T a b = ...
]

We want to come up with an instance declaration of the form

        instance (Ping a, Pong b, ...) => Eq (T a b) where
                x == y = ...

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely @Ping@, @Pong@ and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

        Eq (T a b) = (Ping a, Pong b, ...)

Now we can get a (recursive) equation from the @data@ decl:

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

Foo and Bar may have explicit instances for @Eq@, in which case we can
just substitute for them.  Alternatively, either or both may have
their @Eq@ instances given by @deriving@ clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

        Eq (T a b) = {}         -- The empty set

Next iteration:
        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

        After simplification:
                   = Eq a u Ping b u {} u {} u {}
                   = Eq a u Ping b

Next iteration:

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

        After simplification:
                   = Eq a u Ping b
                   u (Eq b u Ping a)
                   u (Eq a u Ping a)

                   = Eq a u Ping b u Eq b u Ping a

The next iteration gives the same result, so this is the fixpoint.  We
need to make a canonical form of the RHS to ensure convergence.  We do
this by simplifying the RHS to a form in which

        - the classes constrain only tyvars
        - the list is sorted by tyvar (major key) and then class (minor key)
        - no duplicates, of course

So, here are the synonyms for the ``equation'' structures:


Note [Data decl contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

        data (RealFloat a) => Complex a = !a :+ !a deriving( Read )

We will need an instance decl like:

        instance (Read a, RealFloat a) => Read (Complex a) where
          ...

The RealFloat in the context is because the read method for Complex is bound
to construct a Complex, and doing that requires that the argument type is
in RealFloat.

But this ain't true for Show, Eq, Ord, etc, since they don't construct
a Complex; they only take them apart.

Our approach: identify the offending classes, and add the data type
context to the instance decl.  The "offending classes" are

        Read, Enum?

FURTHER NOTE ADDED March 2002.  In fact, Haskell98 now requires that
pattern matching against a constructor from a data type with a context
gives rise to the constraints for that context -- or at least the thinned
version.  So now all classes are "offending".

Note [Newtype deriving]
~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
    class C a b
    instance C [a] Char
    newtype T = T Char deriving( C [a] )

Notice the free 'a' in the deriving.  We have to fill this out to
    newtype T = T Char deriving( forall a. C [a] )

And then translate it to:
    instance C [a] Char => C [a] T where ...


Note [Newtype deriving superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(See also Trac #1220 for an interesting exchange on newtype
deriving and superclasses.)

The 'tys' here come from the partial application in the deriving
clause. The last arg is the new instance type.

We must pass the superclasses; the newtype might be an instance
of them in a different way than the representation type
E.g.            newtype Foo a = Foo a deriving( Show, Num, Eq )
Then the Show instance is not done via Coercible; it shows
        Foo 3 as "Foo 3"
The Num instance is derived via Coercible, but the Show superclass
dictionary must the Show instance for Foo, *not* the Show dictionary
gotten from the Num dictionary. So we must build a whole new dictionary
not just use the Num one.  The instance we want is something like:
     instance (Num a, Show (Foo a), Eq (Foo a)) => Num (Foo a) where
        (+) = ((+)@a)
        ...etc...
There may be a coercion needed which we get from the tycon for the newtype
when the dict is constructed in TcInstDcls.tcInstDecl2


Note [Unused constructors and deriving clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #3221.  Consider
   data T = T1 | T2 deriving( Show )
Are T1 and T2 unused?  Well, no: the deriving clause expands to mention
both of them.  So we gather defs/uses from deriving just like anything else.

************************************************************************
*                                                                      *
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
*                                                                      *
************************************************************************
-}

tcDeriving  :: [LTyClDecl Name]  -- All type constructors
            -> [LInstDecl Name]  -- All instance declarations
            -> [LDerivDecl Name] -- All stand-alone deriving declarations
            -> TcM (TcGblEnv, Bag (InstInfo Name), HsValBinds Name)
tcDeriving tycl_decls inst_decls deriv_decls
  = recoverM (do { g <- getGblEnv
                 ; return (g, emptyBag, emptyValBindsOut)}) $
    do  {       -- Fish the "deriving"-related information out of the TcEnv
                -- And make the necessary "equations".
          is_boot <- tcIsHsBootOrSig
        ; traceTc "tcDeriving" (ppr is_boot)

        ; early_specs <- makeDerivSpecs is_boot tycl_decls inst_decls deriv_decls
        ; traceTc "tcDeriving 1" (ppr early_specs)

        -- for each type, determine the auxiliary declarations that are common
        -- to multiple derivations involving that type (e.g. Generic and
        -- Generic1 should use the same TcGenGenerics.MetaTyCons)
        ; (commonAuxs, auxDerivStuff) <- commonAuxiliaries $ map forgetTheta early_specs

        ; let (infer_specs, given_specs) = splitEarlyDerivSpec early_specs
        ; insts1 <- mapM (genInst commonAuxs) given_specs

        -- the stand-alone derived instances (@insts1@) are used when inferring
        -- the contexts for "deriving" clauses' instances (@infer_specs@)
        ; final_specs <- extendLocalInstEnv (map (iSpec . fstOf3) insts1) $
                         inferInstanceContexts infer_specs

        ; insts2 <- mapM (genInst commonAuxs) final_specs

        ; let (inst_infos, deriv_stuff, maybe_fvs) = unzip3 (insts1 ++ insts2)
        ; loc <- getSrcSpanM
        ; let (binds, newTyCons, famInsts, extraInstances) =
                genAuxBinds loc (unionManyBags (auxDerivStuff : deriv_stuff))

        ; dflags <- getDynFlags

        ; (inst_info, rn_binds, rn_dus) <-
            renameDeriv is_boot (inst_infos ++ (bagToList extraInstances)) binds

        ; unless (isEmptyBag inst_info) $
             liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances"
                        (ddump_deriving inst_info rn_binds newTyCons famInsts))

        ; let all_tycons = map ATyCon (bagToList newTyCons)
        ; gbl_env <- tcExtendGlobalEnv all_tycons $
                     tcExtendGlobalEnvImplicit (concatMap implicitTyThings all_tycons) $
                     tcExtendLocalFamInstEnv (bagToList famInsts) $
                     tcExtendLocalInstEnv (map iSpec (bagToList inst_info)) getGblEnv
        ; let all_dus = rn_dus `plusDU` usesOnly (mkFVs $ catMaybes maybe_fvs)
        ; return (addTcgDUs gbl_env all_dus, inst_info, rn_binds) }
  where
    ddump_deriving :: Bag (InstInfo Name) -> HsValBinds Name
                   -> Bag TyCon               -- ^ Empty data constructors
                   -> Bag FamInst             -- ^ Rep type family instances
                   -> SDoc
    ddump_deriving inst_infos extra_binds repMetaTys repFamInsts
      =    hang (ptext (sLit "Derived instances:"))
              2 (vcat (map (\i -> pprInstInfoDetails i $$ text "") (bagToList inst_infos))
                 $$ ppr extra_binds)
        $$ hangP "Generic representation:" (
              hangP "Generated datatypes for meta-information:"
               (vcat (map ppr (bagToList repMetaTys)))
           $$ hangP "Representation types:"
                (vcat (map pprRepTy (bagToList repFamInsts))))

    hangP s x = text "" $$ hang (ptext (sLit s)) 2 x

{-
genTypeableTyConReps :: DynFlags ->
                        [LTyClDecl Name] ->
                        [LInstDecl Name] ->
                        TcM (Bag (LHsBind RdrName, LSig RdrName))
genTypeableTyConReps dflags decls insts =
  do tcs1 <- mapM tyConsFromDecl decls
     tcs2 <- mapM tyConsFromInst insts
     return $ listToBag [ genTypeableTyConRep dflags loc tc
                                          | (loc,tc) <- concat (tcs1 ++ tcs2) ]
  where

  tyConFromDataCon (L l n) = do dc <- tcLookupDataCon n
                                return (do tc <- promoteDataCon_maybe dc
                                           return (l,tc))

  -- Promoted data constructors from a data declaration, or
  -- a data-family instance.
  tyConsFromDataRHS = fmap catMaybes
                    . mapM tyConFromDataCon
                    . concatMap (con_names . unLoc)
                    . dd_cons

  -- Tycons from a data-family declaration; not promotable.
  tyConFromDataFamDecl FamilyDecl { fdLName = L loc name } =
    do tc <- tcLookupTyCon name
       return (loc,tc)


  -- tycons from a type-level declaration
  tyConsFromDecl (L _ d)

    -- data or newtype declaration: promoted tycon, tycon, promoted ctrs.
    | isDataDecl d =
      do let L loc name = tcdLName d
         tc           <- tcLookupTyCon name
         promotedCtrs <- tyConsFromDataRHS (tcdDataDefn d)
         let tyCons = (loc,tc) : promotedCtrs

         return (case promotableTyCon_maybe tc of
                   Nothing -> tyCons
                   Just kc -> (loc,kc) : tyCons)

    -- data family: just the type constructor;  these are not promotable.
    | isDataFamilyDecl d =
      do res <- tyConFromDataFamDecl (tcdFam d)
         return [res]

    -- class: the type constructors of associated data families
    | isClassDecl d =
      let isData FamilyDecl { fdInfo = DataFamily } = True
          isData _ = False

      in mapM tyConFromDataFamDecl (filter isData (map unLoc (tcdATs d)))

    | otherwise = return []


  tyConsFromInst (L _ d) =
    case d of
      ClsInstD ci      -> fmap concat
                        $ mapM (tyConsFromDataRHS . dfid_defn . unLoc)
                        $ cid_datafam_insts ci
      DataFamInstD dfi -> tyConsFromDataRHS (dfid_defn dfi)
      TyFamInstD {}    -> return []
-}

-- Prints the representable type family instance
pprRepTy :: FamInst -> SDoc
pprRepTy fi@(FamInst { fi_tys = lhs })
  = ptext (sLit "type") <+> ppr (mkTyConApp (famInstTyCon fi) lhs) <+>
      equals <+> ppr rhs
  where rhs = famInstRHS fi

-- As of 24 April 2012, this only shares MetaTyCons between derivations of
-- Generic and Generic1; thus the types and logic are quite simple.
type CommonAuxiliary = MetaTyCons
type CommonAuxiliaries = [(TyCon, CommonAuxiliary)] -- NSF what is a more efficient map type?

commonAuxiliaries :: [DerivSpec ()] -> TcM (CommonAuxiliaries, BagDerivStuff)
commonAuxiliaries = foldM snoc ([], emptyBag) where
  snoc acc@(cas, stuff) (DS {ds_name = nm, ds_cls = cls, ds_tc = rep_tycon})
    | getUnique cls `elem` [genClassKey, gen1ClassKey] =
      extendComAux $ genGenericMetaTyCons rep_tycon (nameModule nm)
    | otherwise = return acc
   where extendComAux m -- don't run m if its already in the accumulator
           | any ((rep_tycon ==) . fst) cas = return acc
           | otherwise = do (ca, new_stuff) <- m
                            return $ ((rep_tycon, ca) : cas, stuff `unionBags` new_stuff)

renameDeriv :: Bool
            -> [InstInfo RdrName]
            -> Bag (LHsBind RdrName, LSig RdrName)
            -> TcM (Bag (InstInfo Name), HsValBinds Name, DefUses)
renameDeriv is_boot inst_infos bagBinds
  | is_boot     -- If we are compiling a hs-boot file, don't generate any derived bindings
                -- The inst-info bindings will all be empty, but it's easier to
                -- just use rn_inst_info to change the type appropriately
  = do  { (rn_inst_infos, fvs) <- mapAndUnzipM rn_inst_info inst_infos
        ; return ( listToBag rn_inst_infos
                 , emptyValBindsOut, usesOnly (plusFVs fvs)) }

  | otherwise
  = discardWarnings $         -- Discard warnings about unused bindings etc
    setXOptM LangExt.EmptyCase $  -- Derived decls (for empty types) can have
                              --    case x of {}
    setXOptM LangExt.ScopedTypeVariables $  -- Derived decls (for newtype-deriving) can
    setXOptM LangExt.KindSignatures $       -- used ScopedTypeVariables & KindSignatures
    do  {
        -- Bring the extra deriving stuff into scope
        -- before renaming the instances themselves
        ; (aux_binds, aux_sigs) <- mapAndUnzipBagM return bagBinds
        ; let aux_val_binds = ValBindsIn aux_binds (bagToList aux_sigs)
        ; rn_aux_lhs <- rnTopBindsLHS emptyFsEnv aux_val_binds
        ; let bndrs = collectHsValBinders rn_aux_lhs
        ; envs <- extendGlobalRdrEnvRn (map Avail bndrs) emptyFsEnv ;
        ; setEnvs envs $
    do  { (rn_aux, dus_aux) <- rnValBindsRHS (TopSigCtxt (mkNameSet bndrs)) rn_aux_lhs
        ; (rn_inst_infos, fvs_insts) <- mapAndUnzipM rn_inst_info inst_infos
        ; return (listToBag rn_inst_infos, rn_aux,
                  dus_aux `plusDU` usesOnly (plusFVs fvs_insts)) } }

  where
    rn_inst_info :: InstInfo RdrName -> TcM (InstInfo Name, FreeVars)
    rn_inst_info
      inst_info@(InstInfo { iSpec = inst
                          , iBinds = InstBindings
                            { ib_binds = binds
                            , ib_tyvars = tyvars
                            , ib_pragmas = sigs
                            , ib_extensions = exts -- Only for type-checking
                            , ib_derived = sa } })
        =  ASSERT( null sigs )
           bindLocalNamesFV tyvars $
           do { (rn_binds, fvs) <- rnMethodBinds (is_cls_nm inst) (\_ -> []) binds
              ; let binds' = InstBindings { ib_binds = rn_binds
                                          , ib_tyvars = tyvars
                                          , ib_pragmas = []
                                          , ib_extensions = exts
                                          , ib_derived = sa }
              ; return (inst_info { iBinds = binds' }, fvs) }

{-
Note [Newtype deriving and unused constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (see Trac #1954):

  module Bug(P) where
  newtype P a = MkP (IO a) deriving Monad

If you compile with -fwarn-unused-binds you do not expect the warning
"Defined but not used: data constructor MkP". Yet the newtype deriving
code does not explicitly mention MkP, but it should behave as if you
had written
  instance Monad P where
     return x = MkP (return x)
     ...etc...

So we want to signal a user of the data constructor 'MkP'.
This is the reason behind the (Maybe Name) part of the return type
of genInst.

************************************************************************
*                                                                      *
                From HsSyn to DerivSpec
*                                                                      *
************************************************************************

@makeDerivSpecs@ fishes around to find the info about needed derived instances.
-}

makeDerivSpecs :: Bool
               -> [LTyClDecl Name]
               -> [LInstDecl Name]
               -> [LDerivDecl Name]
               -> TcM [EarlyDerivSpec]
makeDerivSpecs is_boot tycl_decls inst_decls deriv_decls
  = do  { eqns1 <- concatMapM (recoverM (return []) . deriveTyDecl)     tycl_decls
        ; eqns2 <- concatMapM (recoverM (return []) . deriveInstDecl)   inst_decls
        ; eqns3 <- concatMapM (recoverM (return []) . deriveStandalone) deriv_decls
        ; let eqns = eqns1 ++ eqns2 ++ eqns3

        ; if is_boot then   -- No 'deriving' at all in hs-boot files
              do { unless (null eqns) (add_deriv_err (head eqns))
                 ; return [] }
          else return eqns }
  where
    add_deriv_err eqn
       = setSrcSpan (earlyDSLoc eqn) $
         addErr (hang (ptext (sLit "Deriving not permitted in hs-boot file"))
                    2 (ptext (sLit "Use an instance declaration instead")))

------------------------------------------------------------------
deriveTyDecl :: LTyClDecl Name -> TcM [EarlyDerivSpec]
deriveTyDecl (L _ decl@(DataDecl { tcdLName = L _ tc_name
                                 , tcdDataDefn = HsDataDefn { dd_derivs = preds } }))
  = tcAddDeclCtxt decl $
    do { tc <- tcLookupTyCon tc_name
       ; let tvs  = tyConTyVars tc
             tys  = mkTyVarTys tvs

       ; case preds of
          Just (L _ preds') -> concatMapM (deriveTyData tvs tc tys) preds'
          Nothing           -> return [] }

deriveTyDecl _ = return []

------------------------------------------------------------------
deriveInstDecl :: LInstDecl Name -> TcM [EarlyDerivSpec]
deriveInstDecl (L _ (TyFamInstD {})) = return []
deriveInstDecl (L _ (DataFamInstD { dfid_inst = fam_inst }))
  = deriveFamInst fam_inst
deriveInstDecl (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fam_insts } }))
  = concatMapM (deriveFamInst . unLoc) fam_insts

------------------------------------------------------------------
deriveFamInst :: DataFamInstDecl Name -> TcM [EarlyDerivSpec]
deriveFamInst decl@(DataFamInstDecl
                       { dfid_tycon = L _ tc_name, dfid_pats = pats
                       , dfid_defn
                         = defn@(HsDataDefn { dd_derivs = Just (L _ preds) }) })
  = tcAddDataFamInstCtxt decl $
    do { fam_tc <- tcLookupTyCon tc_name
       ; tcFamTyPats (famTyConShape fam_tc) pats (kcDataDefn defn) $
             -- kcDataDefn defn: see Note [Finding the LHS patterns]
         \ tvs' pats' _ ->
           concatMapM (deriveTyData tvs' fam_tc pats') preds }

deriveFamInst _ = return []

{-
Note [Finding the LHS patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When kind polymorphism is in play, we need to be careful.  Here is
Trac #9359:
  data Cmp a where
    Sup :: Cmp a
    V   :: a -> Cmp a

  data family   CmpInterval (a :: Cmp k) (b :: Cmp k) :: *
  data instance CmpInterval (V c) Sup = Starting c deriving( Show )

So CmpInterval is kind-polymorphic, but the data instance is not
   CmpInterval :: forall k. Cmp k -> Cmp k -> *
   data instance CmpInterval * (V (c::*)) Sup = Starting c deriving( Show )

Hence, when deriving the type patterns in deriveFamInst, we must kind
check the RHS (the data constructor 'Starting c') as well as the LHS,
so that we correctly see the instantiation to *.
-}

------------------------------------------------------------------
deriveStandalone :: LDerivDecl Name -> TcM [EarlyDerivSpec]
-- Standalone deriving declarations
--  e.g.   deriving instance Show a => Show (T a)
-- Rather like tcLocalInstDecl
deriveStandalone (L loc (DerivDecl deriv_ty overlap_mode))
  = setSrcSpan loc                   $
    addErrCtxt (standaloneCtxt deriv_ty)  $
    do { traceTc "Standalone deriving decl for" (ppr deriv_ty)
       ; (tvs, theta, cls, inst_tys) <- tcHsInstHead TcType.InstDeclCtxt deriv_ty
       ; traceTc "Standalone deriving;" $ vcat
              [ text "tvs:" <+> ppr tvs
              , text "theta:" <+> ppr theta
              , text "cls:" <+> ppr cls
              , text "tys:" <+> ppr inst_tys ]
                -- C.f. TcInstDcls.tcLocalInstDecl1
       ; checkTc (not (null inst_tys)) derivingNullaryErr

       ; let cls_tys = take (length inst_tys - 1) inst_tys
             inst_ty = last inst_tys
       ; traceTc "Standalone deriving:" $ vcat
              [ text "class:" <+> ppr cls
              , text "class types:" <+> ppr cls_tys
              , text "type:" <+> ppr inst_ty ]

       ; case tcSplitTyConApp_maybe inst_ty of
           Just (tc, tc_args)
              | className cls == typeableClassName
              -> do warn <- woptM Opt_WarnDerivingTypeable
                    when warn
                       $ addWarnTc (Reason Opt_WarnDerivingTypeable)
                       $ text "Standalone deriving `Typeable` has no effect."
                    return []

              | isAlgTyCon tc  -- All other classes
              -> do { spec <- mkEqnHelp (fmap unLoc overlap_mode)
                                        tvs cls cls_tys tc tc_args (Just theta)
                    ; return [spec] }

           _  -> -- Complain about functions, primitive types, etc,
                 failWithTc $ derivingThingErr False cls cls_tys inst_ty $
                 ptext (sLit "The last argument of the instance must be a data or newtype application")
        }


------------------------------------------------------------------
deriveTyData :: [TyVar] -> TyCon -> [Type]   -- LHS of data or data instance
                                             --   Can be a data instance, hence [Type] args
             -> LHsType Name                 -- The deriving predicate
             -> TcM [EarlyDerivSpec]
-- The deriving clause of a data or newtype declaration
-- I.e. not standalone deriving
deriveTyData tvs tc tc_args (L loc deriv_pred)
  = setSrcSpan loc     $        -- Use the location of the 'deriving' item
    do  { (deriv_tvs, cls, cls_tys, cls_arg_kind)
                <- tcExtendTyVarEnv tvs $
                   tcHsDeriv deriv_pred
                -- Deriving preds may (now) mention
                -- the type variables for the type constructor, hence tcExtendTyVarenv
                -- The "deriv_pred" is a LHsType to take account of the fact that for
                -- newtype deriving we allow deriving (forall a. C [a]).

                -- Typeable is special, because Typeable :: forall k. k -> Constraint
                -- so the argument kind 'k' is not decomposable by splitKindFunTys
                -- as is the case for all other derivable type classes
        ; if className cls == typeableClassName
          then do warn <- woptM Opt_WarnDerivingTypeable
                  when warn
                     $ addWarnTc (Reason Opt_WarnDerivingTypeable)
                     $ text "Deriving `Typeable` has no effect."
                  return []
          else

     do {  -- Given data T a b c = ... deriving( C d ),
           -- we want to drop type variables from T so that (C d (T a)) is well-kinded
          let (arg_kinds, _)  = splitKindFunTys cls_arg_kind
              n_args_to_drop  = length arg_kinds
              n_args_to_keep  = tyConArity tc - n_args_to_drop
              args_to_drop    = drop n_args_to_keep tc_args
              tc_args_to_keep = take n_args_to_keep tc_args
              inst_ty_kind    = typeKind (mkTyConApp tc tc_args_to_keep)
              dropped_tvs     = tyVarsOfTypes args_to_drop

              -- Match up the kinds, and apply the resulting kind substitution
              -- to the types.  See Note [Unify kinds in deriving]
              -- We are assuming the tycon tyvars and the class tyvars are distinct
              mb_match        = tcUnifyTy inst_ty_kind cls_arg_kind
              Just kind_subst = mb_match
              (univ_kvs, univ_tvs) = partition isKindVar $ varSetElems $
                                     mkVarSet deriv_tvs `unionVarSet`
                                     tyVarsOfTypes tc_args_to_keep
              univ_kvs'           = filter (`notElemTvSubst` kind_subst) univ_kvs
              (subst', univ_tvs') = mapAccumL substTyVarBndr kind_subst univ_tvs
              final_tc_args       = substTys subst' tc_args_to_keep
              final_cls_tys       = substTys subst' cls_tys

        ; traceTc "derivTyData1" (vcat [ pprTvBndrs tvs, ppr tc, ppr tc_args, ppr deriv_pred
                                       , pprTvBndrs (tyVarsOfTypesList tc_args)
                                       , ppr n_args_to_keep, ppr n_args_to_drop
                                       , ppr inst_ty_kind, ppr cls_arg_kind, ppr mb_match
                                       , ppr final_tc_args, ppr final_cls_tys ])

        -- Check that the result really is well-kinded
        ; checkTc (n_args_to_keep >= 0 && isJust mb_match)
                  (derivingKindErr tc cls cls_tys cls_arg_kind)

        ; traceTc "derivTyData2" (vcat [ ppr univ_tvs ])

        ; checkTc (allDistinctTyVars args_to_drop &&              -- (a) and (b)
                   not (any (`elemVarSet` dropped_tvs) univ_tvs)) -- (c)
                  (derivingEtaErr cls final_cls_tys (mkTyConApp tc final_tc_args))
                -- Check that
                --  (a) The args to drop are all type variables; eg reject:
                --              data instance T a Int = .... deriving( Monad )
                --  (b) The args to drop are all *distinct* type variables; eg reject:
                --              class C (a :: * -> * -> *) where ...
                --              data instance T a a = ... deriving( C )
                --  (c) The type class args, or remaining tycon args,
                --      do not mention any of the dropped type variables
                --              newtype T a s = ... deriving( ST s )
                --              newtype K a a = ... deriving( Monad )

        ; spec <- mkEqnHelp Nothing (univ_kvs' ++ univ_tvs')
                            cls final_cls_tys tc final_tc_args Nothing
        ; return [spec] } }


{-
Note [Unify kinds in deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (Trac #8534)
    data T a b = MkT a deriving( Functor )
    -- where Functor :: (*->*) -> Constraint

So T :: forall k. * -> k -> *.   We want to get
    instance Functor (T * (a:*)) where ...
Notice the '*' argument to T.

Moreover, as well as instantiating T's kind arguments, we may need to instantiate
C's kind args.  Consider (Trac #8865):
  newtype T a b = MkT (Either a b) deriving( Category )
where
  Category :: forall k. (k -> k -> *) -> Constraint
We need to generate the instance
  instance Category * (Either a) where ...
Notice the '*' argument to Category.

So we need to
 * drop arguments from (T a b) to match the number of
   arrows in the (last argument of the) class;
 * and then *unify* kind of the remaining type against the
   expected kind, to figure out how to instantiate C's and T's
   kind arguments.

In the two examples,
 * we unify   kind-of( T k (a:k) ) ~ kind-of( Functor )
         i.e.      (k -> *) ~ (* -> *)   to find k:=*.
         yielding  k:=*

 * we unify   kind-of( Either ) ~ kind-of( Category )
         i.e.      (* -> * -> *)  ~ (k -> k -> k)
         yielding  k:=*

Now we get a kind substitution.  We then need to:

  1. Remove the substituted-out kind variables from the quantified kind vars

  2. Apply the substitution to the kinds of quantified *type* vars
     (and extend the substitution to reflect this change)

  3. Apply that extended substitution to the non-dropped args (types and
     kinds) of the type and class

Forgetting step (2) caused Trac #8893:
  data V a = V [a] deriving Functor
  data P (x::k->*) (a:k) = P (x a) deriving Functor
  data C (x::k->*) (a:k) = C (V (P x a)) deriving Functor

When deriving Functor for P, we unify k to *, but we then want
an instance   $df :: forall (x:*->*). Functor x => Functor (P * (x:*->*))
and similarly for C.  Notice the modified kind of x, both at binding
and occurrence sites.
-}

mkEqnHelp :: Maybe OverlapMode
          -> [TyVar]
          -> Class -> [Type]
          -> TyCon -> [Type]
          -> DerivContext       -- Just    => context supplied (standalone deriving)
                                -- Nothing => context inferred (deriving on data decl)
          -> TcRn EarlyDerivSpec
-- Make the EarlyDerivSpec for an instance
--      forall tvs. theta => cls (tys ++ [ty])
-- where the 'theta' is optional (that's the Maybe part)
-- Assumes that this declaration is well-kinded

mkEqnHelp overlap_mode tvs cls cls_tys tycon tc_args mtheta
  = do {      -- Find the instance of a data family
              -- Note [Looking up family instances for deriving]
         fam_envs <- tcGetFamInstEnvs
       ; let (rep_tc, rep_tc_args, _co) = tcLookupDataFamInst fam_envs tycon tc_args

              -- If it's still a data family, the lookup failed; i.e no instance exists
       ; when (isDataFamilyTyCon rep_tc)
              (bale_out (ptext (sLit "No family instance for") <+> quotes (pprTypeApp tycon tc_args)))

       -- For standalone deriving (mtheta /= Nothing),
       -- check that all the data constructors are in scope.
       ; rdr_env <- getGlobalRdrEnv
       ; let data_con_names = map dataConName (tyConDataCons rep_tc)
             hidden_data_cons = not (isWiredInName (tyConName rep_tc)) &&
                                (isAbstractTyCon rep_tc ||
                                 any not_in_scope data_con_names)
             not_in_scope dc  = null (lookupGRE_Name rdr_env dc)

             -- Make a Qual RdrName that will do for each DataCon
             -- so we can report it as used (Trac #7969)
             data_con_rdrs = [ mkRdrQual (is_as (is_decl imp_spec)) occ
                             | dc_name <- data_con_names
                             , let occ  = nameOccName dc_name
                                   gres = lookupGRE_Name rdr_env dc_name
                             , not (null gres)
                             , Imported (imp_spec:_) <- [gre_prov (head gres)] ]

       ; addUsedRdrNames data_con_rdrs
       ; unless (isNothing mtheta || not hidden_data_cons)
                (bale_out (derivingHiddenErr tycon))

       ; dflags <- getDynFlags
       ; if isDataTyCon rep_tc then
            mkDataTypeEqn dflags overlap_mode tvs cls cls_tys
                          tycon tc_args rep_tc rep_tc_args mtheta
         else
            mkNewTypeEqn dflags overlap_mode tvs cls cls_tys
                         tycon tc_args rep_tc rep_tc_args mtheta }
  where
     bale_out msg = failWithTc (derivingThingErr False cls cls_tys (mkTyConApp tycon tc_args) msg)

{-
Note [Looking up family instances for deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcLookupFamInstExact is an auxiliary lookup wrapper which requires
that looked-up family instances exist.  If called with a vanilla
tycon, the old type application is simply returned.

If we have
  data instance F () = ... deriving Eq
  data instance F () = ... deriving Eq
then tcLookupFamInstExact will be confused by the two matches;
but that can't happen because tcInstDecls1 doesn't call tcDeriving
if there are any overlaps.

There are two other things that might go wrong with the lookup.
First, we might see a standalone deriving clause
   deriving Eq (F ())
when there is no data instance F () in scope.

Note that it's OK to have
  data instance F [a] = ...
  deriving Eq (F [(a,b)])
where the match is not exact; the same holds for ordinary data types
with standalone deriving declarations.

Note [Deriving, type families, and partial applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When there are no type families, it's quite easy:

    newtype S a = MkS [a]
    -- :CoS :: S  ~ []  -- Eta-reduced

    instance Eq [a] => Eq (S a)         -- by coercion sym (Eq (:CoS a)) : Eq [a] ~ Eq (S a)
    instance Monad [] => Monad S        -- by coercion sym (Monad :CoS)  : Monad [] ~ Monad S

When type families are involved it's trickier:

    data family T a b
    newtype instance T Int a = MkT [a] deriving( Eq, Monad )
    -- :RT is the representation type for (T Int a)
    --  :Co:RT    :: :RT ~ []          -- Eta-reduced!
    --  :CoF:RT a :: T Int a ~ :RT a   -- Also eta-reduced!

    instance Eq [a] => Eq (T Int a)     -- easy by coercion
       -- d1 :: Eq [a]
       -- d2 :: Eq (T Int a) = d1 |> Eq (sym (:Co:RT a ; :coF:RT a))

    instance Monad [] => Monad (T Int)  -- only if we can eta reduce???
       -- d1 :: Monad []
       -- d2 :: Monad (T Int) = d1 |> Monad (sym (:Co:RT ; :coF:RT))

Note the need for the eta-reduced rule axioms.  After all, we can
write it out
    instance Monad [] => Monad (T Int)  -- only if we can eta reduce???
      return x = MkT [x]
      ... etc ...

See Note [Eta reduction for data family axioms] in TcInstDcls.


************************************************************************
*                                                                      *
                Deriving data types
*                                                                      *
************************************************************************
-}

mkDataTypeEqn :: DynFlags
              -> Maybe OverlapMode
              -> [Var]                  -- Universally quantified type variables in the instance
              -> Class                  -- Class for which we need to derive an instance
              -> [Type]                 -- Other parameters to the class except the last
              -> TyCon                  -- Type constructor for which the instance is requested
                                        --    (last parameter to the type class)
              -> [Type]                 -- Parameters to the type constructor
              -> TyCon                  -- rep of the above (for type families)
              -> [Type]                 -- rep of the above
              -> DerivContext        -- Context of the instance, for standalone deriving
              -> TcRn EarlyDerivSpec    -- Return 'Nothing' if error

mkDataTypeEqn dflags overlap_mode tvs cls cls_tys
              tycon tc_args rep_tc rep_tc_args mtheta
  = case checkSideConditions dflags mtheta cls cls_tys rep_tc rep_tc_args of
        -- NB: pass the *representation* tycon to checkSideConditions
        NonDerivableClass   msg -> bale_out (nonStdErr cls $$ msg)
        DerivableClassError msg -> bale_out msg
        CanDerive               -> go_for_it
        DerivableViaInstance    -> go_for_it
  where
    go_for_it    = mk_data_eqn overlap_mode tvs cls tycon tc_args rep_tc rep_tc_args mtheta
    bale_out msg = failWithTc (derivingThingErr False cls cls_tys (mkTyConApp tycon tc_args) msg)

mk_data_eqn :: Maybe OverlapMode -> [TyVar] -> Class
            -> TyCon -> [TcType] -> TyCon -> [TcType] -> DerivContext
            -> TcM EarlyDerivSpec
mk_data_eqn overlap_mode tvs cls tycon tc_args rep_tc rep_tc_args mtheta
  = do loc                  <- getSrcSpanM
       dfun_name            <- new_dfun_name cls tycon
       case mtheta of
        Nothing -> do --Infer context
            inferred_constraints <- inferConstraints cls inst_tys rep_tc rep_tc_args
            return $ InferTheta $ DS
                   { ds_loc = loc
                   , ds_name = dfun_name, ds_tvs = tvs
                   , ds_cls = cls, ds_tys = inst_tys
                   , ds_tc = rep_tc, ds_tc_args = rep_tc_args
                   , ds_theta = inferred_constraints
                   , ds_overlap = overlap_mode
                   , ds_newtype = False }
        Just theta -> do -- Specified context
            return $ GivenTheta $ DS
                   { ds_loc = loc
                   , ds_name = dfun_name, ds_tvs = tvs
                   , ds_cls = cls, ds_tys = inst_tys
                   , ds_tc = rep_tc, ds_tc_args = rep_tc_args
                   , ds_theta = theta
                   , ds_overlap = overlap_mode
                   , ds_newtype = False }
  where
    inst_tys = [mkTyConApp tycon tc_args]

----------------------

inferConstraints :: Class -> [TcType]
                 -> TyCon -> [TcType]
                 -> TcM ThetaOrigin
-- Generate a sufficiently large set of constraints that typechecking the
-- generated method definitions should succeed.   This set will be simplified
-- before being used in the instance declaration
inferConstraints cls inst_tys rep_tc rep_tc_args
  | cls `hasKey` genClassKey    -- Generic constraints are easy
  = return []

  | cls `hasKey` gen1ClassKey   -- Gen1 needs Functor
  = ASSERT(length rep_tc_tvs > 0)   -- See Note [Getting base classes]
    do { functorClass <- tcLookupClass functorClassName
       ; return (con_arg_constraints functorClass (get_gen1_constrained_tys last_tv)) }

  | otherwise  -- The others are a bit more complicated
  = ASSERT2( equalLength rep_tc_tvs all_rep_tc_args, ppr cls <+> ppr rep_tc )
    do { traceTc "inferConstraints" (vcat [ppr cls <+> ppr inst_tys, ppr arg_constraints])
       ; return (stupid_constraints ++ extra_constraints
                 ++ sc_constraints
                 ++ arg_constraints) }
  where
    arg_constraints = con_arg_constraints cls get_std_constrained_tys

       -- Constraints arising from the arguments of each constructor
    con_arg_constraints cls' get_constrained_tys
      = [ mkPredOrigin (DerivOriginDC data_con arg_n) (mkClassPred cls' [inner_ty])
        | data_con <- tyConDataCons rep_tc
        , (arg_n, arg_ty) <- ASSERT( isVanillaDataCon data_con )
                             zip [1..] $ -- ASSERT is precondition of dataConInstOrigArgTys
                             dataConInstOrigArgTys data_con all_rep_tc_args
        , not (isUnLiftedType arg_ty)
        , inner_ty <- get_constrained_tys arg_ty ]

                -- No constraints for unlifted types
                -- See Note [Deriving and unboxed types]

                -- For functor-like classes, two things are different
                -- (a) We recurse over argument types to generate constraints
                --     See Functor examples in TcGenDeriv
                -- (b) The rep_tc_args will be one short
    is_functor_like =    getUnique cls `elem` functorLikeClassKeys
                      || onlyOneAndTypeConstr inst_tys
    onlyOneAndTypeConstr [inst_ty] =
      typeKind inst_ty `tcEqKind` mkArrowKind liftedTypeKind liftedTypeKind
    onlyOneAndTypeConstr _         = False

    get_std_constrained_tys :: Type -> [Type]
    get_std_constrained_tys ty
        | is_functor_like = deepSubtypesContaining last_tv ty
        | otherwise       = [ty]

    rep_tc_tvs = tyConTyVars rep_tc
    last_tv = last rep_tc_tvs
    all_rep_tc_args | cls `hasKey` gen1ClassKey || is_functor_like
                      = rep_tc_args ++ [mkTyVarTy last_tv]
                    | otherwise       = rep_tc_args

        -- Constraints arising from superclasses
        -- See Note [Superclasses of derived instance]
    sc_constraints = mkThetaOrigin DerivOrigin $
        substTheta (zipOpenTvSubst (classTyVars cls) inst_tys) (classSCTheta cls)

        -- Stupid constraints
    stupid_constraints = mkThetaOrigin DerivOrigin $
        substTheta subst (tyConStupidTheta rep_tc)
    subst = zipTopTvSubst rep_tc_tvs all_rep_tc_args

        -- Extra Data constraints
        -- The Data class (only) requires that for
        --    instance (...) => Data (T t1 t2)
        -- IF   t1:*, t2:*
        -- THEN (Data t1, Data t2) are among the (...) constraints
        -- Reason: when the IF holds, we generate a method
        --             dataCast2 f = gcast2 f
        --         and we need the Data constraints to typecheck the method
    extra_constraints
      | cls `hasKey` dataClassKey
      , all (isLiftedTypeKind . typeKind) rep_tc_args
      = [mkPredOrigin DerivOrigin (mkClassPred cls [ty]) | ty <- rep_tc_args]
      | otherwise
      = []

{-
Note [Getting base classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functor and Typeable are defined in package 'base', and that is not available
when compiling 'ghc-prim'.  So we must be careful that 'deriving' for stuff in
ghc-prim does not use Functor or Typeable implicitly via these lookups.

Note [Deriving and unboxed types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have some special hacks to support things like
   data T = MkT Int# deriving ( Show )

Specifically, we use TcGenDeriv.box_if_necy to box the Int# into an Int
(which we know how to show). It's a bit ad hoc.

Note [Deriving any class]
~~~~~~~~~~~~~~~~~~~~~~~~~
Classic uses of a deriving clause, or a standalone-deriving declaration, are
for:
  * a built-in class like Eq or Show, for which GHC knows how to generate
    the instance code
  * a newtype, via the mechanism enabled by GeneralizedNewtypeDeriving

The DeriveAnyClass extension adds a third way to derive instances, based on
empty instance declarations.

The canonical use case is in combination with GHC.Generics and default method
signatures. These allow us to have instance declarations be empty, but still
useful, e.g.

  data T a = ...blah..blah... deriving( Generic )
  instance C a => C (T a)  -- No 'where' clause

where C is some "random" user-defined class.

This boilerplate code can be replaced by the more compact

  data T a = ...blah..blah... deriving( Generic, C )

if DeriveAnyClass is enabled.

This is not restricted to Generics; any class can be derived, simply giving
rise to an empty instance.

Unfortunately, it is not clear how to determine the context (in case of
standard deriving; in standalone deriving, the user provides the context).
GHC uses the same heuristic for figuring out the class context that it uses for
Eq in the case of *-kinded classes, and for Functor in the case of
* -> *-kinded classes. That may not be optimal or even wrong. But in such
cases, standalone deriving can still be used.
-}

------------------------------------------------------------------
-- Check side conditions that dis-allow derivability for particular classes
-- This is *apart* from the newtype-deriving mechanism
--
-- Here we get the representation tycon in case of family instances as it has
-- the data constructors - but we need to be careful to fall back to the
-- family tycon (with indexes) in error messages.

data DerivStatus = CanDerive
                 | DerivableClassError SDoc  -- Standard class, but can't do it
                 | DerivableViaInstance      -- See Note [Deriving any class]
                 | NonDerivableClass SDoc    -- Non-standard class

checkSideConditions :: DynFlags -> DerivContext -> Class -> [TcType]
                    -> TyCon -> [Type] -- tycon and its parameters
                    -> DerivStatus
checkSideConditions dflags mtheta cls cls_tys rep_tc rep_tc_args
  | Just cond <- sideConditions mtheta cls
  = case (cond (dflags, rep_tc, rep_tc_args)) of
        NotValid err -> DerivableClassError err  -- Class-specific error
        IsValid  | null cls_tys -> CanDerive     -- All derivable classes are unary, so
                                                 -- cls_tys (the type args other than last)
                                                 -- should be null
                 | otherwise    -> DerivableClassError (classArgsErr cls cls_tys)  -- e.g. deriving( Eq s )
  | otherwise = maybe DerivableViaInstance NonDerivableClass
                      (canDeriveAnyClass dflags rep_tc cls)

classArgsErr :: Class -> [Type] -> SDoc
classArgsErr cls cls_tys = quotes (ppr (mkClassPred cls cls_tys)) <+> ptext (sLit "is not a class")

nonStdErr :: Class -> SDoc
nonStdErr cls = quotes (ppr cls) <+> ptext (sLit "is not a derivable class")

sideConditions :: DerivContext -> Class -> Maybe Condition
sideConditions mtheta cls
  | cls_key == eqClassKey          = Just (cond_std `andCond` cond_args cls)
  | cls_key == ordClassKey         = Just (cond_std `andCond` cond_args cls)
  | cls_key == showClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == readClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == enumClassKey        = Just (cond_std `andCond` cond_isEnumeration)
  | cls_key == ixClassKey          = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == boundedClassKey     = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == dataClassKey        = Just (checkFlag LangExt.DeriveDataTypeable `andCond`
                                           cond_std `andCond`
                                           cond_args cls)
  | cls_key == functorClassKey     = Just (checkFlag LangExt.DeriveFunctor `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK True)
  | cls_key == foldableClassKey    = Just (checkFlag LangExt.DeriveFoldable `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK False) -- Functor/Fold/Trav works ok for rank-n types
  | cls_key == traversableClassKey = Just (checkFlag LangExt.DeriveTraversable `andCond`
                                           cond_vanilla `andCond`
                                           cond_functorOK False)
  | cls_key == genClassKey         = Just (checkFlag LangExt.DeriveGeneric `andCond`
                                           cond_vanilla `andCond`
                                           cond_RepresentableOk)
  | cls_key == gen1ClassKey        = Just (checkFlag LangExt.DeriveGeneric `andCond`
                                           cond_vanilla `andCond`
                                           cond_Representable1Ok)
  | cls_key == classClassKey       = Just cond_class
  | otherwise                      = Nothing
  where
    cls_key = getUnique cls
    cond_std     = cond_stdOK mtheta False  -- Vanilla data constructors, at least one,
                                            --    and monotype arguments
    cond_vanilla = cond_stdOK mtheta True   -- Vanilla data constructors but
                                            --   allow no data cons or polytype arguments

type Condition = (DynFlags, TyCon, [Type]) -> Validity
        -- first Bool is whether or not we are allowed to derive Data and Typeable
        -- second Bool is whether or not we are allowed to derive Functor
        -- TyCon is the *representation* tycon if the data type is an indexed one
        -- [Type] are the type arguments to the (representation) TyCon
        -- Nothing => OK

orCond :: Condition -> Condition -> Condition
orCond c1 c2 tc
  = case (c1 tc, c2 tc) of
     (IsValid,    _)          -> IsValid    -- c1 succeeds
     (_,          IsValid)    -> IsValid    -- c21 succeeds
     (NotValid x, NotValid y) -> NotValid (x $$ ptext (sLit "  or") $$ y)
                                            -- Both fail

andCond :: Condition -> Condition -> Condition
andCond c1 c2 tc = c1 tc `andValid` c2 tc

cond_stdOK :: DerivContext -- Says whether this is standalone deriving or not;
                           --     if standalone, we just say "yes, go for it"
           -> Bool         -- True <=> permissive: allow higher rank
                           --          args and no data constructors
           -> Condition
cond_stdOK (Just _) _ _
  = IsValid     -- Don't check these conservative conditions for
                -- standalone deriving; just generate the code
                -- and let the typechecker handle the result
cond_stdOK Nothing permissive (_, rep_tc, _)
  | null data_cons
  , not permissive      = NotValid (no_cons_why rep_tc $$ suggestion)
  | not (null con_whys) = NotValid (vcat con_whys $$ suggestion)
  | otherwise           = IsValid
  where
    suggestion = ptext (sLit "Possible fix: use a standalone deriving declaration instead")
    data_cons  = tyConDataCons rep_tc
    con_whys   = getInvalids (map check_con data_cons)

    check_con :: DataCon -> Validity
    check_con con
      | not (isVanillaDataCon con)
      = NotValid (badCon con (ptext (sLit "has existentials or constraints in its type")))
      | not (permissive || all isTauTy (dataConOrigArgTys con))
      = NotValid (badCon con (ptext (sLit "has a higher-rank type")))
      | otherwise
      = IsValid

no_cons_why :: TyCon -> SDoc
no_cons_why rep_tc = quotes (pprSourceTyCon rep_tc) <+>
                     ptext (sLit "must have at least one data constructor")

cond_RepresentableOk :: Condition
cond_RepresentableOk (_, tc, tc_args) = canDoGenerics tc tc_args

cond_Representable1Ok :: Condition
cond_Representable1Ok (_, tc, tc_args) = canDoGenerics1 tc tc_args

cond_enumOrProduct :: Class -> Condition
cond_enumOrProduct cls = cond_isEnumeration `orCond`
                         (cond_isProduct `andCond` cond_args cls)

cond_args :: Class -> Condition
-- For some classes (eg Eq, Ord) we allow unlifted arg types
-- by generating specialised code.  For others (eg Data) we don't.
cond_args cls (_, tc, _)
  = case bad_args of
      []     -> IsValid
      (ty:_) -> NotValid (hang (ptext (sLit "Don't know how to derive") <+> quotes (ppr cls))
                             2 (ptext (sLit "for type") <+> quotes (ppr ty)))
  where
    bad_args = [ arg_ty | con <- tyConDataCons tc
                        , arg_ty <- dataConOrigArgTys con
                        , isUnLiftedType arg_ty
                        , not (ok_ty arg_ty) ]

    cls_key = classKey cls
    ok_ty arg_ty
     | cls_key == eqClassKey   = check_in arg_ty ordOpTbl || isObjectType arg_ty
     | cls_key == ordClassKey  = check_in arg_ty ordOpTbl
     | cls_key == showClassKey = check_in arg_ty boxConTbl || isObjectType arg_ty
     | otherwise               = False    -- Read, Ix etc

    check_in :: Type -> [(Type,a)] -> Bool
    check_in arg_ty tbl = any (eqType arg_ty . fst) tbl


cond_isEnumeration :: Condition
cond_isEnumeration (_, rep_tc, _)
  | isEnumerationTyCon rep_tc = IsValid
  | otherwise                 = NotValid why
  where
    why = sep [ quotes (pprSourceTyCon rep_tc) <+>
                  ptext (sLit "must be an enumeration type")
              , ptext (sLit "(an enumeration consists of one or more nullary, non-GADT constructors)") ]
                  -- See Note [Enumeration types] in TyCon

cond_isProduct :: Condition
cond_isProduct (_, rep_tc, _)
  | isProductTyCon rep_tc = IsValid
  | otherwise             = NotValid why
  where
    why = quotes (pprSourceTyCon rep_tc) <+>
          ptext (sLit "must have precisely one constructor")

functorLikeClassKeys :: [Unique]
functorLikeClassKeys = [functorClassKey, foldableClassKey, traversableClassKey]

cond_functorOK :: Bool -> Condition
-- OK for Functor/Foldable/Traversable class
-- Currently: (a) at least one argument
--            (b) don't use argument contravariantly
--            (c) don't use argument in the wrong place, e.g. data T a = T (X a a)
--            (d) optionally: don't use function types
--            (e) no "stupid context" on data type
cond_functorOK allowFunctions (_, rep_tc, _)
  | null tc_tvs
  = NotValid (ptext (sLit "Data type") <+> quotes (ppr rep_tc)
              <+> ptext (sLit "must have some type parameters"))

  | not (null bad_stupid_theta)
  = NotValid (ptext (sLit "Data type") <+> quotes (ppr rep_tc)
              <+> ptext (sLit "must not have a class context") <+> pprTheta bad_stupid_theta)

  | otherwise
  = allValid (map check_con data_cons)
  where
    tc_tvs            = tyConTyVars rep_tc
    Just (_, last_tv) = snocView tc_tvs
    bad_stupid_theta  = filter is_bad (tyConStupidTheta rep_tc)
    is_bad pred       = last_tv `elemVarSet` tyVarsOfType pred

    data_cons = tyConDataCons rep_tc
    check_con con = allValid (check_universal con : foldDataConArgs (ft_check con) con)

    check_universal :: DataCon -> Validity
    check_universal con
      | Just tv <- getTyVar_maybe (last (tyConAppArgs (dataConOrigResTy con)))
      , tv `elem` dataConUnivTyVars con
      , not (tv `elemVarSet` tyVarsOfTypes (dataConTheta con))
      = IsValid   -- See Note [Check that the type variable is truly universal]
      | otherwise
      = NotValid (badCon con existential)

    ft_check :: DataCon -> FFoldType Validity
    ft_check con = FT { ft_triv = IsValid, ft_var = IsValid
                      , ft_co_var = NotValid (badCon con covariant)
                      , ft_fun = \x y -> if allowFunctions then x `andValid` y
                                                           else NotValid (badCon con functions)
                      , ft_tup = \_ xs  -> allValid xs
                      , ft_ty_app = \_ x   -> x
                      , ft_bad_app = NotValid (badCon con wrong_arg)
                      , ft_forall = \_ x   -> x }

    existential = ptext (sLit "must be truly polymorphic in the last argument of the data type")
    covariant   = ptext (sLit "must not use the type variable in a function argument")
    functions   = ptext (sLit "must not contain function types")
    wrong_arg   = ptext (sLit "must use the type variable only as the last argument of a data type")

checkFlag :: LangExt.Extension -> Condition
checkFlag flag (dflags, _, _)
  | xopt flag dflags = IsValid
  | otherwise        = NotValid why
  where
    why = ptext (sLit "You need ") <> text flag_str
          <+> ptext (sLit "to derive an instance for this class")
    flag_str = case [ flagSpecName f | f <- xFlags , flagSpecFlag f == flag ] of
                 [s]   -> s
                 other -> pprPanic "checkFlag" (ppr other)

std_class_via_coercible :: Class -> Bool
-- These standard classes can be derived for a newtype
-- using the coercible trick *even if no -XGeneralizedNewtypeDeriving
-- because giving so gives the same results as generating the boilerplate
std_class_via_coercible clas
  = classKey clas `elem` [eqClassKey, ordClassKey, ixClassKey, boundedClassKey]
        -- Not Read/Show because they respect the type
        -- Not Enum, because newtypes are never in Enum


non_coercible_class :: Class -> Bool
-- *Never* derive Read, Show, Typeable, Data, Generic, Generic1 by Coercible,
-- even with -XGeneralizedNewtypeDeriving
-- Also, avoid Traversable, as the Coercible-derived instance and the "normal"-derived
-- instance behave differently if there's a non-lawful Applicative out there.
-- Besides, with roles, Coercible-deriving Traversable is ill-roled.
non_coercible_class cls
  = classKey cls `elem` ([ readClassKey, showClassKey, dataClassKey
                         , genClassKey, gen1ClassKey, typeableClassKey
                         , traversableClassKey ])

new_dfun_name :: Class -> TyCon -> TcM Name
new_dfun_name clas tycon        -- Just a simple wrapper
  = do { loc <- getSrcSpanM     -- The location of the instance decl, not of the tycon
        ; newDFunName clas [mkTyConApp tycon []] loc }
        -- The type passed to newDFunName is only used to generate
        -- a suitable string; hence the empty type arg list

badCon :: DataCon -> SDoc -> SDoc
badCon con msg = ptext (sLit "Constructor") <+> quotes (ppr con) <+> msg

{-
Note [Check that the type variable is truly universal]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Functor, Foldable, Traversable, we must check that the *last argument*
of the type constructor is used truly universally quantified.  Example

   data T a b where
     T1 :: a -> b -> T a b      -- Fine! Vanilla H-98
     T2 :: b -> c -> T a b      -- Fine! Existential c, but we can still map over 'b'
     T3 :: b -> T Int b         -- Fine! Constraint 'a', but 'b' is still polymorphic
     T4 :: Ord b => b -> T a b  -- No!  'b' is constrained
     T5 :: b -> T b b           -- No!  'b' is constrained
     T6 :: T a (b,b)            -- No!  'b' is constrained

Notice that only the first of these constructors is vanilla H-98. We only
need to take care about the last argument (b in this case).  See Trac #8678.
Eg. for T1-T3 we can write

     fmap f (T1 a b) = T1 a (f b)
     fmap f (T2 b c) = T2 (f b) c
     fmap f (T3 x)   = T3 (f x)


Note [Superclasses of derived instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, a derived instance decl needs the superclasses of the derived
class too.  So if we have
        data T a = ...deriving( Ord )
then the initial context for Ord (T a) should include Eq (T a).  Often this is
redundant; we'll also generate an Ord constraint for each constructor argument,
and that will probably generate enough constraints to make the Eq (T a) constraint
be satisfied too.  But not always; consider:

 data S a = S
 instance Eq (S a)
 instance Ord (S a)

 data T a = MkT (S a) deriving( Ord )
 instance Num a => Eq (T a)

The derived instance for (Ord (T a)) must have a (Num a) constraint!
Similarly consider:
        data T a = MkT deriving( Data, Typeable )
Here there *is* no argument field, but we must nevertheless generate
a context for the Data instances:
        instance Typable a => Data (T a) where ...


************************************************************************
*                                                                      *
                Deriving newtypes
*                                                                      *
************************************************************************
-}

mkNewTypeEqn :: DynFlags -> Maybe OverlapMode -> [Var] -> Class
             -> [Type] -> TyCon -> [Type] -> TyCon -> [Type]
             -> DerivContext
             -> TcRn EarlyDerivSpec
mkNewTypeEqn dflags overlap_mode tvs
             cls cls_tys tycon tc_args rep_tycon rep_tc_args mtheta
-- Want: instance (...) => cls (cls_tys ++ [tycon tc_args]) where ...
  | ASSERT( length cls_tys + 1 == classArity cls )
    might_derive_via_coercible && ((newtype_deriving && not deriveAnyClass)
                                  || std_class_via_coercible cls)
  = do traceTc "newtype deriving:" (ppr tycon <+> ppr rep_tys <+> ppr all_preds)
       dfun_name <- new_dfun_name cls tycon
       loc <- getSrcSpanM
       case mtheta of
        Just theta -> return $ GivenTheta $ DS
            { ds_loc = loc
            , ds_name = dfun_name, ds_tvs = varSetElemsKvsFirst dfun_tvs
            , ds_cls = cls, ds_tys = inst_tys
            , ds_tc = rep_tycon, ds_tc_args = rep_tc_args
            , ds_theta = theta
            , ds_overlap = overlap_mode
            , ds_newtype = True }
        Nothing -> return $ InferTheta $ DS
            { ds_loc = loc
            , ds_name = dfun_name, ds_tvs = varSetElemsKvsFirst dfun_tvs
            , ds_cls = cls, ds_tys = inst_tys
            , ds_tc = rep_tycon, ds_tc_args = rep_tc_args
            , ds_theta = all_preds
            , ds_overlap = overlap_mode
            , ds_newtype = True }
  | otherwise
  = case checkSideConditions dflags mtheta cls cls_tys rep_tycon rep_tc_args of
      -- Error with standard class
      DerivableClassError msg
        | might_derive_via_coercible -> bale_out (msg $$ suggest_nd)
        | otherwise                  -> bale_out msg
      -- Must use newtype deriving or DeriveAnyClass
      NonDerivableClass _msg
        -- Too hard, even with newtype deriving
        | newtype_deriving           -> bale_out cant_derive_err
        -- Try newtype deriving!
        | might_derive_via_coercible -> bale_out (non_std $$ suggest_nd)
        | otherwise                  -> bale_out non_std
      -- CanDerive/DerivableViaInstance
      _ -> do when (newtype_deriving && deriveAnyClass) $
                addWarnTc NoReason (sep [ ptext (sLit "Both DeriveAnyClass and GeneralizedNewtypeDeriving are enabled")
                               , ptext (sLit "Defaulting to the DeriveAnyClass strategy for instantiating") <+> ppr cls ])
              go_for_it
  where
        newtype_deriving  = xopt LangExt.GeneralizedNewtypeDeriving dflags
        deriveAnyClass    = xopt LangExt.DeriveAnyClass             dflags
        go_for_it         = mk_data_eqn overlap_mode tvs cls tycon tc_args
                              rep_tycon rep_tc_args mtheta
        bale_out    = bale_out' newtype_deriving
        bale_out' b = failWithTc . derivingThingErr b cls cls_tys inst_ty

        non_std    = nonStdErr cls
        suggest_nd = ptext (sLit "Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension")

        -- Here is the plan for newtype derivings.  We see
        --        newtype T a1...an = MkT (t ak+1...an) deriving (.., C s1 .. sm, ...)
        -- where t is a type,
        --       ak+1...an is a suffix of a1..an, and are all tyars
        --       ak+1...an do not occur free in t, nor in the s1..sm
        --       (C s1 ... sm) is a  *partial applications* of class C
        --                      with the last parameter missing
        --       (T a1 .. ak) matches the kind of C's last argument
        --              (and hence so does t)
        -- The latter kind-check has been done by deriveTyData already,
        -- and tc_args are already trimmed
        --
        -- We generate the instance
        --       instance forall ({a1..ak} u fvs(s1..sm)).
        --                C s1 .. sm t => C s1 .. sm (T a1...ak)
        -- where T a1...ap is the partial application of
        --       the LHS of the correct kind and p >= k
        --
        --      NB: the variables below are:
        --              tc_tvs = [a1, ..., an]
        --              tyvars_to_keep = [a1, ..., ak]
        --              rep_ty = t ak .. an
        --              deriv_tvs = fvs(s1..sm) \ tc_tvs
        --              tys = [s1, ..., sm]
        --              rep_fn' = t
        --
        -- Running example: newtype T s a = MkT (ST s a) deriving( Monad )
        -- We generate the instance
        --      instance Monad (ST s) => Monad (T s) where

        nt_eta_arity = length (fst (newTyConEtadRhs rep_tycon))
                -- For newtype T a b = MkT (S a a b), the TyCon machinery already
                -- eta-reduces the representation type, so we know that
                --      T a ~ S a a
                -- That's convenient here, because we may have to apply
                -- it to fewer than its original complement of arguments

        -- Note [Newtype representation]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Need newTyConRhs (*not* a recursive representation finder)
        -- to get the representation type. For example
        --      newtype B = MkB Int
        --      newtype A = MkA B deriving( Num )
        -- We want the Num instance of B, *not* the Num instance of Int,
        -- when making the Num instance of A!
        rep_inst_ty = newTyConInstRhs rep_tycon rep_tc_args
        rep_tys     = cls_tys ++ [rep_inst_ty]
        rep_pred    = mkClassPred cls rep_tys
        rep_pred_o  = mkPredOrigin DerivOrigin rep_pred
                -- rep_pred is the representation dictionary, from where
                -- we are gong to get all the methods for the newtype
                -- dictionary


        -- Next we figure out what superclass dictionaries to use
        -- See Note [Newtype deriving superclasses] above

        cls_tyvars = classTyVars cls
        dfun_tvs = tyVarsOfTypes inst_tys
        inst_ty = mkTyConApp tycon tc_args
        inst_tys = cls_tys ++ [inst_ty]
        sc_theta =
            mkThetaOrigin DerivOrigin $
            substTheta (zipOpenTvSubst cls_tyvars inst_tys) (classSCTheta cls)


        -- Next we collect Coercible constraints between
        -- the Class method types, instantiated with the representation and the
        -- newtype type; precisely the constraints required for the
        -- calls to coercible that we are going to generate.
        coercible_constraints =
            [ let (Pair t1 t2) = mkCoerceClassMethEqn cls (varSetElemsKvsFirst dfun_tvs) inst_tys rep_inst_ty meth
              in mkPredOrigin (DerivOriginCoerce meth t1 t2) (mkCoerciblePred t1 t2)
            | meth <- classMethods cls ]

                -- If there are no tyvars, there's no need
                -- to abstract over the dictionaries we need
                -- Example:     newtype T = MkT Int deriving( C )
                -- We get the derived instance
                --              instance C T
                -- rather than
                --              instance C Int => C T
        all_preds = rep_pred_o : coercible_constraints ++ sc_theta -- NB: rep_pred comes first

        -------------------------------------------------------------------
        --  Figuring out whether we can only do this newtype-deriving thing

        -- See Note [Determining whether newtype-deriving is appropriate]
        might_derive_via_coercible
           =  not (non_coercible_class cls)
           && eta_ok
           && ats_ok
--         && not (isRecursiveTyCon tycon)      -- Note [Recursive newtypes]

        -- Check that eta reduction is OK
        eta_ok = nt_eta_arity <= length rep_tc_args
                -- The newtype can be eta-reduced to match the number
                --     of type argument actually supplied
                --        newtype T a b = MkT (S [a] b) deriving( Monad )
                --     Here the 'b' must be the same in the rep type (S [a] b)
                --     And the [a] must not mention 'b'.  That's all handled
                --     by nt_eta_rity.

        ats_ok = null (classATs cls)
               -- No associated types for the class, because we don't
               -- currently generate type 'instance' decls; and cannot do
               -- so for 'data' instance decls

        cant_derive_err
           = vcat [ ppUnless eta_ok eta_msg
                  , ppUnless ats_ok ats_msg ]
        eta_msg   = ptext (sLit "cannot eta-reduce the representation type enough")
        ats_msg   = ptext (sLit "the class has associated types")

{-
Note [Recursive newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Newtype deriving works fine, even if the newtype is recursive.
e.g.    newtype S1 = S1 [T1 ()]
        newtype T1 a = T1 (StateT S1 IO a ) deriving( Monad )
Remember, too, that type families are currently (conservatively) given
a recursive flag, so this also allows newtype deriving to work
for type families.

We used to exclude recursive types, because we had a rather simple
minded way of generating the instance decl:
   newtype A = MkA [A]
   instance Eq [A] => Eq A      -- Makes typechecker loop!
But now we require a simple context, so it's ok.

Note [Determining whether newtype-deriving is appropriate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
  newtype NT = MkNT Foo
    deriving C
we have to decide how to perform the deriving. Do we do newtype deriving,
or do we do normal deriving? In general, we prefer to do newtype deriving
wherever possible. So, we try newtype deriving unless there's a glaring
reason not to.

Note that newtype deriving might fail, even after we commit to it. This
is because the derived instance uses `coerce`, which must satisfy its
`Coercible` constraint. This is different than other deriving scenarios,
where we're sure that the resulting instance will type-check.

************************************************************************
*                                                                      *
\subsection[TcDeriv-fixpoint]{Finding the fixed point of \tr{deriving} equations}
*                                                                      *
************************************************************************

A ``solution'' (to one of the equations) is a list of (k,TyVarTy tv)
terms, which is the final correct RHS for the corresponding original
equation.
\begin{itemize}
\item
Each (k,TyVarTy tv) in a solution constrains only a type
variable, tv.

\item
The (k,TyVarTy tv) pairs in a solution are canonically
ordered by sorting on type variable, tv, (major key) and then class, k,
(minor key)
\end{itemize}
-}

inferInstanceContexts :: [DerivSpec ThetaOrigin] -> TcM [DerivSpec ThetaType]

inferInstanceContexts [] = return []

inferInstanceContexts infer_specs
  = do  { traceTc "inferInstanceContexts" $ vcat (map pprDerivSpec infer_specs)
        ; iterate_deriv 1 initial_solutions }
  where
    ------------------------------------------------------------------
        -- The initial solutions for the equations claim that each
        -- instance has an empty context; this solution is certainly
        -- in canonical form.
    initial_solutions :: [ThetaType]
    initial_solutions = [ [] | _ <- infer_specs ]

    ------------------------------------------------------------------
        -- iterate_deriv calculates the next batch of solutions,
        -- compares it with the current one; finishes if they are the
        -- same, otherwise recurses with the new solutions.
        -- It fails if any iteration fails
    iterate_deriv :: Int -> [ThetaType] -> TcM [DerivSpec ThetaType]
    iterate_deriv n current_solns
      | n > 20  -- Looks as if we are in an infinite loop
                -- This can happen if we have -XUndecidableInstances
                -- (See TcSimplify.tcSimplifyDeriv.)
      = pprPanic "solveDerivEqns: probable loop"
                 (vcat (map pprDerivSpec infer_specs) $$ ppr current_solns)
      | otherwise
      = do {      -- Extend the inst info from the explicit instance decls
                  -- with the current set of solutions, and simplify each RHS
             inst_specs <- zipWithM newDerivClsInst current_solns infer_specs
           ; new_solns <- checkNoErrs $
                          extendLocalInstEnv inst_specs $
                          mapM gen_soln infer_specs

           ; if (current_solns `eqSolution` new_solns) then
                return [ spec { ds_theta = soln }
                       | (spec, soln) <- zip infer_specs current_solns ]
             else
                iterate_deriv (n+1) new_solns }

    eqSolution = eqListBy (eqListBy eqType)

    ------------------------------------------------------------------
    gen_soln :: DerivSpec ThetaOrigin -> TcM ThetaType
    gen_soln (DS { ds_loc = loc, ds_tvs = tyvars
                 , ds_cls = clas, ds_tys = inst_tys, ds_theta = deriv_rhs })
      = setSrcSpan loc  $
        addErrCtxt (derivInstCtxt the_pred) $
        do { theta <- simplifyDeriv the_pred tyvars deriv_rhs
                -- checkValidInstance tyvars theta clas inst_tys
                -- Not necessary; see Note [Exotic derived instance contexts]

           ; traceTc "TcDeriv" (ppr deriv_rhs $$ ppr theta)
                -- Claim: the result instance declaration is guaranteed valid
                -- Hence no need to call:
                --   checkValidInstance tyvars theta clas inst_tys
           ; return (sortBy cmpType theta) }    -- Canonicalise before returning the solution
      where
        the_pred = mkClassPred clas inst_tys

------------------------------------------------------------------
newDerivClsInst :: ThetaType -> DerivSpec theta -> TcM ClsInst
newDerivClsInst theta (DS { ds_name = dfun_name, ds_overlap = overlap_mode
                          , ds_tvs = tvs, ds_cls = clas, ds_tys = tys })
  = newClsInst overlap_mode dfun_name tvs theta clas tys

extendLocalInstEnv :: [ClsInst] -> TcM a -> TcM a
-- Add new locally-defined instances; don't bother to check
-- for functional dependency errors -- that'll happen in TcInstDcls
extendLocalInstEnv dfuns thing_inside
 = do { env <- getGblEnv
      ; let  inst_env' = extendInstEnvList (tcg_inst_env env) dfuns
             env'      = env { tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

{-
***********************************************************************************
*                                                                                 *
*            Simplify derived constraints
*                                                                                 *
***********************************************************************************
-}

simplifyDeriv :: PredType
              -> [TyVar]
              -> ThetaOrigin      -- Wanted
              -> TcM ThetaType  -- Needed
-- Given  instance (wanted) => C inst_ty
-- Simplify 'wanted' as much as possibles
-- Fail if not possible
simplifyDeriv pred tvs theta
  = do { (skol_subst, tvs_skols) <- tcInstSkolTyVars tvs -- Skolemize
                -- The constraint solving machinery
                -- expects *TcTyVars* not TyVars.
                -- We use *non-overlappable* (vanilla) skolems
                -- See Note [Overlap and deriving]

       ; let subst_skol = zipTopTvSubst tvs_skols $ map mkTyVarTy tvs
             skol_set   = mkVarSet tvs_skols
             doc = ptext (sLit "deriving") <+> parens (ppr pred)

       ; wanted <- mapM (\(PredOrigin t o) -> newSimpleWanted o (substTy skol_subst t)) theta

       ; traceTc "simplifyDeriv" $
         vcat [ pprTvBndrs tvs $$ ppr theta $$ ppr wanted, doc ]
       ; (residual_wanted, _ev_binds1)
             <- solveWantedsTcM (mkSimpleWC wanted)
                -- Post: residual_wanted are already zonked

       ; let (good, bad) = partitionBagWith get_good (wc_simple residual_wanted)
                         -- See Note [Exotic derived instance contexts]
             get_good :: Ct -> Either PredType Ct
             get_good ct | validDerivPred skol_set p
                         , isWantedCt ct  = Left p
                         -- NB: residual_wanted may contain unsolved
                         -- Derived and we stick them into the bad set
                         -- so that reportUnsolved may decide what to do with them
                         | otherwise = Right ct
                         where p = ctPred ct

       -- If we are deferring type errors, simply ignore any insoluble
       -- constraints.  They'll come up again when we typecheck the
       -- generated instance declaration
       ; defer <- goptM Opt_DeferTypeErrors
       ; unless defer (reportAllUnsolved (residual_wanted { wc_simple = bad }))

       ; let min_theta = mkMinimalBySCs (bagToList good)
       ; return (substTheta subst_skol min_theta) }

{-
Note [Overlap and deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider some overlapping instances:
  data Show a => Show [a] where ..
  data Show [Char] where ...

Now a data type with deriving:
  data T a = MkT [a] deriving( Show )

We want to get the derived instance
  instance Show [a] => Show (T a) where...
and NOT
  instance Show a => Show (T a) where...
so that the (Show (T Char)) instance does the Right Thing

It's very like the situation when we're inferring the type
of a function
   f x = show [x]
and we want to infer
   f :: Show [a] => a -> String

BOTTOM LINE: use vanilla, non-overlappable skolems when inferring
             the context for the derived instance.
             Hence tcInstSkolTyVars not tcInstSuperSkolTyVars

Note [Exotic derived instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a 'derived' instance declaration, we *infer* the context.  It's a
bit unclear what rules we should apply for this; the Haskell report is
silent.  Obviously, constraints like (Eq a) are fine, but what about
        data T f a = MkT (f a) deriving( Eq )
where we'd get an Eq (f a) constraint.  That's probably fine too.

One could go further: consider
        data T a b c = MkT (Foo a b c) deriving( Eq )
        instance (C Int a, Eq b, Eq c) => Eq (Foo a b c)

Notice that this instance (just) satisfies the Paterson termination
conditions.  Then we *could* derive an instance decl like this:

        instance (C Int a, Eq b, Eq c) => Eq (T a b c)
even though there is no instance for (C Int a), because there just
*might* be an instance for, say, (C Int Bool) at a site where we
need the equality instance for T's.

However, this seems pretty exotic, and it's quite tricky to allow
this, and yet give sensible error messages in the (much more common)
case where we really want that instance decl for C.

So for now we simply require that the derived instance context
should have only type-variable constraints.

Here is another example:
        data Fix f = In (f (Fix f)) deriving( Eq )
Here, if we are prepared to allow -XUndecidableInstances we
could derive the instance
        instance Eq (f (Fix f)) => Eq (Fix f)
but this is so delicate that I don't think it should happen inside
'deriving'. If you want this, write it yourself!

NB: if you want to lift this condition, make sure you still meet the
termination conditions!  If not, the deriving mechanism generates
larger and larger constraints.  Example:
  data Succ a = S a
  data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

Note the lack of a Show instance for Succ.  First we'll generate
  instance (Show (Succ a), Show a) => Show (Seq a)
and then
  instance (Show (Succ (Succ a)), Show (Succ a), Show a) => Show (Seq a)
and so on.  Instead we want to complain of no instance for (Show (Succ a)).

The bottom line
~~~~~~~~~~~~~~~
Allow constraints which consist only of type variables, with no repeats.


************************************************************************
*                                                                      *
\subsection[TcDeriv-normal-binds]{Bindings for the various classes}
*                                                                      *
************************************************************************

After all the trouble to figure out the required context for the
derived instance declarations, all that's left is to chug along to
produce them.  They will then be shoved into @tcInstDecls2@, which
will do all its usual business.

There are lots of possibilities for code to generate.  Here are
various general remarks.

PRINCIPLES:
\begin{itemize}
\item
We want derived instances of @Eq@ and @Ord@ (both v common) to be
``you-couldn't-do-better-by-hand'' efficient.

\item
Deriving @Show@---also pretty common--- should also be reasonable good code.

\item
Deriving for the other classes isn't that common or that big a deal.
\end{itemize}

PRAGMATICS:

\begin{itemize}
\item
Deriving @Ord@ is done mostly with the 1.3 @compare@ method.

\item
Deriving @Eq@ also uses @compare@, if we're deriving @Ord@, too.

\item
We {\em normally} generate code only for the non-defaulted methods;
there are some exceptions for @Eq@ and (especially) @Ord@...

\item
Sometimes we use a @_con2tag_<tycon>@ function, which returns a data
constructor's numeric (@Int#@) tag.  These are generated by
@gen_tag_n_con_binds@, and the heuristic for deciding if one of
these is around is given by @hasCon2TagFun@.

The examples under the different sections below will make this
clearer.

\item
Much less often (really just for deriving @Ix@), we use a
@_tag2con_<tycon>@ function.  See the examples.

\item
We use the renamer!!!  Reason: we're supposed to be
producing @LHsBinds Name@ for the methods, but that means
producing correctly-uniquified code on the fly.  This is entirely
possible (the @TcM@ monad has a @UniqueSupply@), but it is painful.
So, instead, we produce @MonoBinds RdrName@ then heave 'em through
the renamer.  What a great hack!
\end{itemize}
-}

-- Generate the InstInfo for the required instance paired with the
--   *representation* tycon for that instance,
-- plus any auxiliary bindings required
--
-- Representation tycons differ from the tycon in the instance signature in
-- case of instances for indexed families.
--
genInst :: CommonAuxiliaries
        -> DerivSpec ThetaType
        -> TcM (InstInfo RdrName, BagDerivStuff, Maybe Name)
genInst comauxs
        spec@(DS { ds_tvs = tvs, ds_tc = rep_tycon, ds_tc_args = rep_tc_args
                 , ds_theta = theta, ds_newtype = is_newtype, ds_tys = tys
                 , ds_name = dfun_name, ds_cls = clas, ds_loc = loc })
  | is_newtype   -- See Note [Bindings for Generalised Newtype Deriving]
  = do { inst_spec <- newDerivClsInst theta spec
       ; traceTc "genInst/is_newtype" (vcat [ppr loc, ppr clas, ppr tvs, ppr tys, ppr rhs_ty])
       ; return ( InstInfo
                    { iSpec   = inst_spec
                    , iBinds  = InstBindings
                        { ib_binds = gen_Newtype_binds loc clas tvs tys rhs_ty
                        , ib_tyvars = map Var.varName tvs   -- Scope over bindings
                        , ib_pragmas = []
                        , ib_extensions = [ LangExt.ImpredicativeTypes
                                          , LangExt.RankNTypes ]
                        , ib_derived = True } }
                , emptyBag
                , Just $ getName $ head $ tyConDataCons rep_tycon ) }
              -- See Note [Newtype deriving and unused constructors]

  | otherwise
  = do { (meth_binds, deriv_stuff) <- genDerivStuff loc clas
                                        dfun_name rep_tycon
                                        (lookup rep_tycon comauxs)
       ; inst_spec <- newDerivClsInst theta spec
       ; traceTc "newder" (ppr inst_spec)
       ; let inst_info = InstInfo { iSpec   = inst_spec
                                  , iBinds  = InstBindings
                                                { ib_binds = meth_binds
                                                , ib_tyvars = map Var.varName tvs
                                                , ib_pragmas = []
                                                , ib_extensions = []
                                                , ib_derived = True } }
       ; return ( inst_info, deriv_stuff, Nothing ) }
  where
    rhs_ty = newTyConInstRhs rep_tycon rep_tc_args

genDerivStuff :: SrcSpan -> Class -> Name -> TyCon
              -> Maybe CommonAuxiliary
              -> TcM (LHsBinds RdrName, BagDerivStuff)
genDerivStuff loc clas dfun_name tycon comaux_maybe
  | let ck = classKey clas
  , ck `elem` [genClassKey, gen1ClassKey]   -- Special case because monadic
  = let gk = if ck == genClassKey then Gen0 else Gen1
        -- TODO NSF: correctly identify when we're building Both instead of One
        Just metaTyCons = comaux_maybe -- well-guarded by commonAuxiliaries and genInst
    in do
      (binds, faminst) <- gen_Generic_binds gk tycon metaTyCons (nameModule dfun_name)
      return (binds, unitBag (DerivFamInst faminst))

  | otherwise                      -- Non-monadic generators
  = do { dflags <- getDynFlags
       ; fix_env <- getDataConFixityFun tycon
       ; return (genDerivedBinds dflags fix_env clas loc tycon) }

getDataConFixityFun :: TyCon -> TcM (Name -> Fixity)
-- If the TyCon is locally defined, we want the local fixity env;
-- but if it is imported (which happens for standalone deriving)
-- we need to get the fixity env from the interface file
-- c.f. RnEnv.lookupFixity, and Trac #9830
getDataConFixityFun tc
  = do { this_mod <- getModule
       ; if nameIsLocalOrFrom this_mod name
         then do { fix_env <- getFixityEnv
                 ; return (lookupFixity fix_env) }
         else do { iface <- loadInterfaceForName doc name
                            -- Should already be loaded!
                 ; return (mi_fix_fn iface . nameOccName) } }
  where
    name = tyConName tc
    doc = ptext (sLit "Data con fixities for") <+> ppr name

{-
Note [Bindings for Generalised Newtype Deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  class Eq a => C a where
     f :: a -> a
  newtype N a = MkN [a] deriving( C )
  instance Eq (N a) where ...

The 'deriving C' clause generates, in effect
  instance (C [a], Eq a) => C (N a) where
     f = coerce (f :: [a] -> [a])

This generates a cast for each method, but allows the superclasse to
be worked out in the usual way.  In this case the superclass (Eq (N
a)) will be solved by the explicit Eq (N a) instance.  We do *not*
create the superclasses by casting the superclass dictionaries for the
representation type.

See the paper "Safe zero-cost coercions for Haskell".


************************************************************************
*                                                                      *
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
*                                                                      *
************************************************************************
-}

derivingNullaryErr :: MsgDoc
derivingNullaryErr = ptext (sLit "Cannot derive instances for nullary classes")

derivingKindErr :: TyCon -> Class -> [Type] -> Kind -> MsgDoc
derivingKindErr tc cls cls_tys cls_kind
  = hang (ptext (sLit "Cannot derive well-kinded instance of form")
                <+> quotes (pprClassPred cls cls_tys <+> parens (ppr tc <+> ptext (sLit "..."))))
       2 (ptext (sLit "Class") <+> quotes (ppr cls)
            <+> ptext (sLit "expects an argument of kind") <+> quotes (pprKind cls_kind))

derivingEtaErr :: Class -> [Type] -> Type -> MsgDoc
derivingEtaErr cls cls_tys inst_ty
  = sep [ptext (sLit "Cannot eta-reduce to an instance of form"),
         nest 2 (ptext (sLit "instance (...) =>")
                <+> pprClassPred cls (cls_tys ++ [inst_ty]))]

derivingThingErr :: Bool -> Class -> [Type] -> Type -> MsgDoc -> MsgDoc
derivingThingErr newtype_deriving clas tys ty why
  = sep [(hang (ptext (sLit "Can't make a derived instance of"))
             2 (quotes (ppr pred))
          $$ nest 2 extra) <> colon,
         nest 2 why]
  where
    extra | newtype_deriving = ptext (sLit "(even with cunning newtype deriving)")
          | otherwise        = Outputable.empty
    pred = mkClassPred clas (tys ++ [ty])

derivingHiddenErr :: TyCon -> SDoc
derivingHiddenErr tc
  = hang (ptext (sLit "The data constructors of") <+> quotes (ppr tc) <+> ptext (sLit "are not all in scope"))
       2 (ptext (sLit "so you cannot derive an instance for it"))

standaloneCtxt :: LHsType Name -> SDoc
standaloneCtxt ty = hang (ptext (sLit "In the stand-alone deriving instance for"))
                       2 (quotes (ppr ty))

derivInstCtxt :: PredType -> MsgDoc
derivInstCtxt pred
  = ptext (sLit "When deriving the instance for") <+> parens (ppr pred)

cond_class :: Condition
cond_class (_, rep_tycon, _)
  | length dataCons /= 1 =
    NotValid $ quotes (ppr rep_tycon)
           <+> text "must have exactly one data constructor."
  | length dataConArgs /= 1 =
    NotValid $ quotes (ppr dataCon)
           <+> text "must have exactly one argument of the form \"Object# a\"."
  | Just (tc, _) <- tcSplitTyConApp_maybe dataConArg
  , isObjectTyCon tc
    = IsValid
  | otherwise = NotValid $ quotes (ppr dataCon)
                       <+> text "must have an argument of the form \"Object# a\"."
  where dataCons = tyConDataCons rep_tycon
        dataCon = head dataCons
        dataConArgs = dataConOrigArgTys dataCon
        dataConArg = head dataConArgs
