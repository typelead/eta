{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


TcInstDecls: Typechecking instance declarations
-}
{-# LANGUAGE CPP #-}

module Eta.TypeCheck.TcInstDcls ( tcInstDecls1, tcInstDecls2 ) where

import Eta.HsSyn.HsSyn
import Eta.TypeCheck.TcBinds
import Eta.TypeCheck.TcTyClsDecls
import Eta.TypeCheck.TcClassDcl( tcClassDecl2,
                   HsSigFun, lookupHsSig, mkHsSigFun,
                   findMethodBind, instantiateMethod, tcInstanceMethodBody )
import Eta.TypeCheck.TcPat      ( addInlinePrags )
import Eta.TypeCheck.TcRnMonad
import Eta.TypeCheck.TcValidity
import Eta.TypeCheck.TcMType
import Eta.TypeCheck.TcType
import Eta.Iface.BuildTyCl
import Eta.TypeCheck.Inst
import Eta.Types.InstEnv
import Eta.TypeCheck.FamInst
import Eta.Types.FamInstEnv
import Eta.TypeCheck.TcDeriv
import Eta.TypeCheck.TcEnv
import Eta.TypeCheck.TcHsType
import Eta.TypeCheck.TcUnify
import Eta.Types.Coercion   ( pprCoAxiom )
import Eta.Core.MkCore     ( nO_METHOD_BINDING_ERROR_ID )
import Eta.Types.Type
import Eta.TypeCheck.TcEvidence
import Eta.Types.TyCon
import Eta.Types.CoAxiom
import Eta.BasicTypes.DataCon
import Eta.Types.Class
import Eta.BasicTypes.Var
import qualified Eta.BasicTypes.Var as Var
import Eta.BasicTypes.VarEnv
import Eta.BasicTypes.VarSet
import Eta.Prelude.PrelNames  ( typeableClassName, genericClassNames )
import Eta.Utils.Bag
import Eta.BasicTypes.BasicTypes
import Eta.Main.DynFlags
import Eta.Main.ErrUtils
import Eta.Utils.FastString
import Eta.Main.HscTypes ( isHsBootOrSig )
import Eta.BasicTypes.Id
import Eta.BasicTypes.MkId
import Eta.BasicTypes.Name
import Eta.BasicTypes.NameSet
import Eta.Utils.Outputable
import qualified Eta.Utils.Outputable as Outputable
import Eta.BasicTypes.SrcLoc
import Eta.Utils.Util
import Eta.Utils.BooleanFormula ( isUnsatisfied, pprBooleanFormulaNice )
import qualified Eta.LanguageExtensions as LangExt
import Control.Monad
import Eta.Utils.Maybes     ( isNothing, isJust, whenIsJust )
import Data.List  ( mapAccumL, partition )

#include "HsVersions.h"

{-
Typechecking instance declarations is done in two passes. The first
pass, made by @tcInstDecls1@, collects information to be used in the
second pass.

This pre-processed info includes the as-yet-unprocessed bindings
inside the instance declaration.  These are type-checked in the second
pass, when the class-instance envs and GVE contain all the info from
all the instance and value decls.  Indeed that's the reason we need
two passes over the instance decls.


Note [How instance declarations are translated]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is how we translation instance declarations into Core

Running example:
        class C a where
           op1, op2 :: Ix b => a -> b -> b
           op2 = <dm-rhs>

        instance C a => C [a]
           {-# INLINE [2] op1 #-}
           op1 = <rhs>
===>
        -- Method selectors
        op1,op2 :: forall a. C a => forall b. Ix b => a -> b -> b
        op1 = ...
        op2 = ...

        -- Default methods get the 'self' dictionary as argument
        -- so they can call other methods at the same type
        -- Default methods get the same type as their method selector
        $dmop2 :: forall a. C a => forall b. Ix b => a -> b -> b
        $dmop2 = /\a. \(d:C a). /\b. \(d2: Ix b). <dm-rhs>
               -- NB: type variables 'a' and 'b' are *both* in scope in <dm-rhs>
               -- Note [Tricky type variable scoping]

        -- A top-level definition for each instance method
        -- Here op1_i, op2_i are the "instance method Ids"
        -- The INLINE pragma comes from the user pragma
        {-# INLINE [2] op1_i #-}  -- From the instance decl bindings
        op1_i, op2_i :: forall a. C a => forall b. Ix b => [a] -> b -> b
        op1_i = /\a. \(d:C a).
               let this :: C [a]
                   this = df_i a d
                     -- Note [Subtle interaction of recursion and overlap]

                   local_op1 :: forall b. Ix b => [a] -> b -> b
                   local_op1 = <rhs>
                     -- Source code; run the type checker on this
                     -- NB: Type variable 'a' (but not 'b') is in scope in <rhs>
                     -- Note [Tricky type variable scoping]

               in local_op1 a d

        op2_i = /\a \d:C a. $dmop2 [a] (df_i a d)

        -- The dictionary function itself
        {-# NOINLINE CONLIKE df_i #-}   -- Never inline dictionary functions
        df_i :: forall a. C a -> C [a]
        df_i = /\a. \d:C a. MkC (op1_i a d) (op2_i a d)
                -- But see Note [Default methods in instances]
                -- We can't apply the type checker to the default-method call

        -- Use a RULE to short-circuit applications of the class ops
        {-# RULE "op1@C[a]" forall a, d:C a.
                            op1 [a] (df_i d) = op1_i a d #-}

Note [Instances and loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Note that df_i may be mutually recursive with both op1_i and op2_i.
  It's crucial that df_i is not chosen as the loop breaker, even
  though op1_i has a (user-specified) INLINE pragma.

* Instead the idea is to inline df_i into op1_i, which may then select
  methods from the MkC record, and thereby break the recursion with
  df_i, leaving a *self*-recursive op1_i.  (If op1_i doesn't call op at
  the same type, it won't mention df_i, so there won't be recursion in
  the first place.)

* If op1_i is marked INLINE by the user there's a danger that we won't
  inline df_i in it, and that in turn means that (since it'll be a
  loop-breaker because df_i isn't), op1_i will ironically never be
  inlined.  But this is OK: the recursion breaking happens by way of
  a RULE (the magic ClassOp rule above), and RULES work inside InlineRule
  unfoldings. See Note [RULEs enabled in SimplGently] in SimplUtils

Note [ClassOp/DFun selection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One thing we see a lot is stuff like
    op2 (df d1 d2)
where 'op2' is a ClassOp and 'df' is DFun.  Now, we could inline *both*
'op2' and 'df' to get
     case (MkD ($cop1 d1 d2) ($cop2 d1 d2) ... of
       MkD _ op2 _ _ _ -> op2
And that will reduce to ($cop2 d1 d2) which is what we wanted.

But it's tricky to make this work in practice, because it requires us to
inline both 'op2' and 'df'.  But neither is keen to inline without having
seen the other's result; and it's very easy to get code bloat (from the
big intermediate) if you inline a bit too much.

Instead we use a cunning trick.
 * We arrange that 'df' and 'op2' NEVER inline.

 * We arrange that 'df' is ALWAYS defined in the stylised form
      df d1 d2 = MkD ($cop1 d1 d2) ($cop2 d1 d2) ...

 * We give 'df' a magical unfolding (DFunUnfolding [$cop1, $cop2, ..])
   that lists its methods.

 * We make CoreUnfold.exprIsConApp_maybe spot a DFunUnfolding and return
   a suitable constructor application -- inlining df "on the fly" as it
   were.

 * ClassOp rules: We give the ClassOp 'op2' a BuiltinRule that
   extracts the right piece iff its argument satisfies
   exprIsConApp_maybe.  This is done in MkId mkDictSelId

 * We make 'df' CONLIKE, so that shared uses still match; eg
      let d = df d1 d2
      in ...(op2 d)...(op1 d)...

Note [Single-method classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the class has just one method (or, more accurately, just one element
of {superclasses + methods}), then we use a different strategy.

   class C a where op :: a -> a
   instance C a => C [a] where op = <blah>

We translate the class decl into a newtype, which just gives a
top-level axiom. The "constructor" MkC expands to a cast, as does the
class-op selector.

   axiom Co:C a :: C a ~ (a->a)

   op :: forall a. C a -> (a -> a)
   op a d = d |> (Co:C a)

   MkC :: forall a. (a->a) -> C a
   MkC = /\a.\op. op |> (sym Co:C a)

The clever RULE stuff doesn't work now, because ($df a d) isn't
a constructor application, so exprIsConApp_maybe won't return
Just <blah>.

Instead, we simply rely on the fact that casts are cheap:

   $df :: forall a. C a => C [a]
   {-# INLINE df #-}  -- NB: INLINE this
   $df = /\a. \d. MkC [a] ($cop_list a d)
       = $cop_list |> forall a. C a -> (sym (Co:C [a]))

   $cop_list :: forall a. C a => [a] -> [a]
   $cop_list = <blah>

So if we see
   (op ($df a d))
we'll inline 'op' and '$df', since both are simply casts, and
good things happen.

Why do we use this different strategy?  Because otherwise we
end up with non-inlined dictionaries that look like
    $df = $cop |> blah
which adds an extra indirection to every use, which seems stupid.  See
Trac #4138 for an example (although the regression reported there
wasn't due to the indirection).

There is an awkward wrinkle though: we want to be very
careful when we have
    instance C a => C [a] where
      {-# INLINE op #-}
      op = ...
then we'll get an INLINE pragma on $cop_list but it's important that
$cop_list only inlines when it's applied to *two* arguments (the
dictionary and the list argument).  So we must not eta-expand $df
above.  We ensure that this doesn't happen by putting an INLINE
pragma on the dfun itself; after all, it ends up being just a cast.

There is one more dark corner to the INLINE story, even more deeply
buried.  Consider this (Trac #3772):

    class DeepSeq a => C a where
      gen :: Int -> a

    instance C a => C [a] where
      gen n = ...

    class DeepSeq a where
      deepSeq :: a -> b -> b

    instance DeepSeq a => DeepSeq [a] where
      {-# INLINE deepSeq #-}
      deepSeq xs b = foldr deepSeq b xs

That gives rise to these defns:

    $cdeepSeq :: DeepSeq a -> [a] -> b -> b
    -- User INLINE( 3 args )!
    $cdeepSeq a (d:DS a) b (x:[a]) (y:b) = ...

    $fDeepSeq[] :: DeepSeq a -> DeepSeq [a]
    -- DFun (with auto INLINE pragma)
    $fDeepSeq[] a d = $cdeepSeq a d |> blah

    $cp1 a d :: C a => DeepSep [a]
    -- We don't want to eta-expand this, lest
    -- $cdeepSeq gets inlined in it!
    $cp1 a d = $fDeepSep[] a (scsel a d)

    $fC[] :: C a => C [a]
    -- Ordinary DFun
    $fC[] a d = MkC ($cp1 a d) ($cgen a d)

Here $cp1 is the code that generates the superclass for C [a].  The
issue is this: we must not eta-expand $cp1 either, or else $fDeepSeq[]
and then $cdeepSeq will inline there, which is definitely wrong.  Like
on the dfun, we solve this by adding an INLINE pragma to $cp1.

Note [Subtle interaction of recursion and overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
  class C a where { op1,op2 :: a -> a }
  instance C a => C [a] where
    op1 x = op2 x ++ op2 x
    op2 x = ...
  instance C [Int] where
    ...

When type-checking the C [a] instance, we need a C [a] dictionary (for
the call of op2).  If we look up in the instance environment, we find
an overlap.  And in *general* the right thing is to complain (see Note
[Overlapping instances] in InstEnv).  But in *this* case it's wrong to
complain, because we just want to delegate to the op2 of this same
instance.

Why is this justified?  Because we generate a (C [a]) constraint in
a context in which 'a' cannot be instantiated to anything that matches
other overlapping instances, or else we would not be executing this
version of op1 in the first place.

It might even be a bit disguised:

  nullFail :: C [a] => [a] -> [a]
  nullFail x = op2 x ++ op2 x

  instance C a => C [a] where
    op1 x = nullFail x

Precisely this is used in package 'regex-base', module Context.hs.
See the overlapping instances for RegexContext, and the fact that they
call 'nullFail' just like the example above.  The DoCon package also
does the same thing; it shows up in module Fraction.hs.

Conclusion: when typechecking the methods in a C [a] instance, we want to
treat the 'a' as an *existential* type variable, in the sense described
by Note [Binding when looking up instances].  That is why isOverlappableTyVar
responds True to an InstSkol, which is the kind of skolem we use in
tcInstDecl2.


Note [Tricky type variable scoping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In our example
        class C a where
           op1, op2 :: Ix b => a -> b -> b
           op2 = <dm-rhs>

        instance C a => C [a]
           {-# INLINE [2] op1 #-}
           op1 = <rhs>

note that 'a' and 'b' are *both* in scope in <dm-rhs>, but only 'a' is
in scope in <rhs>.  In particular, we must make sure that 'b' is in
scope when typechecking <dm-rhs>.  This is achieved by subFunTys,
which brings appropriate tyvars into scope. This happens for both
<dm-rhs> and for <rhs>, but that doesn't matter: the *renamer* will have
complained if 'b' is mentioned in <rhs>.



************************************************************************
*                                                                      *
\subsection{Extracting instance decls}
*                                                                      *
************************************************************************

Gather up the instance declarations from their various sources
-}

tcInstDecls1    -- Deal with both source-code and imported instance decls
   :: [LTyClDecl Name]          -- For deriving stuff
   -> [LInstDecl Name]          -- Source code instance decls
   -> [LDerivDecl Name]         -- Source code stand-alone deriving decls
   -> TcM (TcGblEnv,            -- The full inst env
           [InstInfo Name],     -- Source-code instance decls to process;
                                -- contains all dfuns for this module
           HsValBinds Name)     -- Supporting bindings for derived instances

tcInstDecls1 tycl_decls inst_decls deriv_decls
  = checkNoErrs $
    do {    -- Stop if addInstInfos etc discovers any errors
            -- (they recover, so that we get more than one error each
            -- round)

            -- Do class and family instance declarations
       ; stuff <- mapAndRecoverM tcLocalInstDecl inst_decls
       ; let (local_infos_s, fam_insts_s) = unzip stuff
             fam_insts    = concat fam_insts_s
             local_infos' = concat local_infos_s
             -- Handwritten instances of the poly-kinded Typeable class are
             -- forbidden, so we handle those separately
             (typeable_instances, local_infos)
                = partition bad_typeable_instance local_infos'

       ; addClsInsts local_infos $
         addFamInsts fam_insts   $
    do {    -- Compute instances from "deriving" clauses;
            -- This stuff computes a context for the derived instance
            -- decl, so it needs to know about all the instances possible
            -- NB: class instance declarations can contain derivings as
            --     part of associated data type declarations
         failIfErrsM    -- If the addInsts stuff gave any errors, don't
                        -- try the deriving stuff, because that may give
                        -- more errors still

       ; traceTc "tcDeriving" Outputable.empty
       ; th_stage <- getStage   -- See Note [Deriving inside TH brackets ]
       ; (gbl_env, deriv_inst_info, deriv_binds)
              <- if isBrackStage th_stage
                 then do { gbl_env <- getGblEnv
                         ; return (gbl_env, emptyBag, emptyValBindsOut) }
                 else tcDeriving tycl_decls inst_decls deriv_decls

       -- Fail if there are any handwritten instance of poly-kinded Typeable
       ; mapM_ typeable_err typeable_instances

       -- Check that if the module is compiled with -XSafe, there are no
       -- hand written instances of old Typeable as then unsafe casts could be
       -- performed. Derived instances are OK.
       ; dflags <- getDynFlags
       ; when (safeLanguageOn dflags) $ forM_ local_infos $ \x -> case x of
             _ | genInstCheck x -> addErrAt (getSrcSpan $ iSpec x) (genInstErr x)
             _ -> return ()

       -- As above but for Safe Inference mode.
       ; when (safeInferOn dflags) $ forM_ local_infos $ \x -> case x of
             _ | genInstCheck x -> recordUnsafeInfer
             _ | overlapCheck x -> recordUnsafeInfer
             _ -> return ()

       ; return ( gbl_env
                , bagToList deriv_inst_info ++ local_infos
                , deriv_binds)
    }}
  where
    -- Separate the Typeable instances from the rest
    bad_typeable_instance i
      = typeableClassName == is_cls_nm (iSpec i)


    overlapCheck ty = case overlapMode (is_flag $ iSpec ty) of
                        NoOverlap _ -> False
                        _           -> True
    genInstCheck ty = is_cls_nm (iSpec ty) `elem` genericClassNames
    genInstErr i = hang (ptext (sLit $ "Generic instances can only be "
                            ++ "derived in Safe Haskell.") $+$
                         ptext (sLit "Replace the following instance:"))
                     2 (pprInstanceHdr (iSpec i))

    -- Report an error or a warning for a `Typeable` instances.
    -- If we are working on an .hs-boot file, we just report a warning,
    -- and ignore the instance.  We do this, to give users a chance to fix
    -- their code.
    typeable_err i =
      setSrcSpan (getSrcSpan (iSpec i)) $
        do env <- getGblEnv
           if isHsBootOrSig (tcg_src env)
             then
               do warn <- woptM Opt_WarnDerivingTypeable
                  when warn $ addWarnTc (Reason Opt_WarnDerivingTypeable) $ vcat
                    [ ptext (sLit "`Typeable` instances in .hs-boot files are ignored.")
                    , ptext (sLit "This warning will become an error in future versions of the compiler.")
                    ]
             else addErrTc $ ptext (sLit "Class `Typeable` does not support user-specified instances.")

addClsInsts :: [InstInfo Name] -> TcM a -> TcM a
addClsInsts infos thing_inside
  = tcExtendLocalInstEnv (map iSpec infos) thing_inside

addFamInsts :: [FamInst] -> TcM a -> TcM a
-- Extend (a) the family instance envt
--        (b) the type envt with stuff from data type decls
addFamInsts fam_insts thing_inside
  = tcExtendLocalFamInstEnv fam_insts $
    tcExtendGlobalEnv things  $
    do { traceTc "addFamInsts" (pprFamInsts fam_insts)
       ; tcg_env <- tcAddImplicits things
       ; setGblEnv tcg_env thing_inside }
  where
    axioms = map (toBranchedAxiom . famInstAxiom) fam_insts
    tycons = famInstsRepTyCons fam_insts
    things = map ATyCon tycons ++ map ACoAxiom axioms

{-
Note [Deriving inside TH brackets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a declaration bracket
  [d| data T = A | B deriving( Show ) |]

there is really no point in generating the derived code for deriving(
Show) and then type-checking it. This will happen at the call site
anyway, and the type check should never fail!  Moreover (Trac #6005)
the scoping of the generated code inside the bracket does not seem to
work out.

The easy solution is simply not to generate the derived instances at
all.  (A less brutal solution would be to generate them with no
bindings.)  This will become moot when we shift to the new TH plan, so
the brutal solution will do.
-}

tcLocalInstDecl :: LInstDecl Name
                -> TcM ([InstInfo Name], [FamInst])
        -- A source-file instance declaration
        -- Type-check all the stuff before the "where"
        --
        -- We check for respectable instance type, and context
tcLocalInstDecl (L loc (TyFamInstD { tfid_inst = decl }))
  = do { fam_inst <- tcTyFamInstDecl Nothing (L loc decl)
       ; return ([], [fam_inst]) }

tcLocalInstDecl (L loc (DataFamInstD { dfid_inst = decl }))
  = do { fam_inst <- tcDataFamInstDecl Nothing (L loc decl)
       ; return ([], [fam_inst]) }

tcLocalInstDecl (L loc (ClsInstD { cid_inst = decl }))
  = do { (insts, fam_insts) <- tcClsInstDecl (L loc decl)
       ; return (insts, fam_insts) }

tcClsInstDecl :: LClsInstDecl Name -> TcM ([InstInfo Name], [FamInst])
tcClsInstDecl (L loc (ClsInstDecl { cid_poly_ty = poly_ty, cid_binds = binds
                                  , cid_sigs = uprags, cid_tyfam_insts = ats
                                  , cid_overlap_mode = overlap_mode
                                  , cid_datafam_insts = adts }))
  = setSrcSpan loc                      $
    addErrCtxt (instDeclCtxt1 poly_ty)  $
    do  { is_boot <- tcIsHsBootOrSig
        ; checkTc (not is_boot || (isEmptyLHsBinds binds && null uprags))
                  badBootDeclErr

        ; (tyvars, theta, clas, inst_tys) <- tcHsInstHead InstDeclCtxt poly_ty
        ; let mini_env   = mkVarEnv (classTyVars clas `zip` inst_tys)
              mini_subst = mkTvSubst (mkInScopeSet (mkVarSet tyvars)) mini_env
              mb_info    = Just (clas, mini_env)

        -- Next, process any associated types.
        ; traceTc "tcLocalInstDecl" (ppr poly_ty)
        ; tyfam_insts0  <- tcExtendTyVarEnv tyvars $
                           mapAndRecoverM (tcTyFamInstDecl mb_info) ats
        ; datafam_insts <- tcExtendTyVarEnv tyvars $
                           mapAndRecoverM (tcDataFamInstDecl mb_info) adts

        -- Check for missing associated types and build them
        -- from their defaults (if available)
        ; let defined_ats = mkNameSet (map (tyFamInstDeclName . unLoc) ats)
                            `unionNameSet`
                            mkNameSet (map (unLoc . dfid_tycon . unLoc) adts)
        ; tyfam_insts1 <- mapM (tcATDefault mini_subst defined_ats)
                               (classATItems clas)

        -- Finally, construct the Core representation of the instance.
        -- (This no longer includes the associated types.)
        ; dfun_name <- newDFunName clas inst_tys (getLoc poly_ty)
                -- Dfun location is that of instance *header*

        ; ispec <- newClsInst (fmap unLoc overlap_mode) dfun_name tyvars theta
                              clas inst_tys
        ; let inst_info = InstInfo { iSpec  = ispec
                                   , iBinds = InstBindings
                                     { ib_binds = binds
                                     , ib_tyvars = map Var.varName tyvars -- Scope over bindings
                                     , ib_pragmas = uprags
                                     , ib_extensions = []
                                     , ib_derived = False } }

        ; return ( [inst_info], tyfam_insts0 ++ concat tyfam_insts1 ++ datafam_insts) }


tcATDefault :: TvSubst -> NameSet -> ClassATItem -> TcM [FamInst]
-- ^ Construct default instances for any associated types that
-- aren't given a user definition
-- Returns [] or singleton
tcATDefault inst_subst defined_ats (ATI fam_tc defs)
  -- User supplied instances ==> everything is OK
  | tyConName fam_tc `elemNameSet` defined_ats
  = return []

  -- No user instance, have defaults ==> instantiate them
   -- Example:   class C a where { type F a b :: *; type F a b = () }
   --            instance C [x]
   -- Then we want to generate the decl:   type F [x] b = ()
  | Just (rhs_ty, _loc) <- defs
  = do { let (subst', pat_tys') = mapAccumL subst_tv inst_subst
                                            (tyConTyVars fam_tc)
             rhs'     = substTy subst' rhs_ty
             tv_set'  = tyVarsOfTypes pat_tys'
             tvs'     = varSetElemsKvsFirst tv_set'
       ; rep_tc_name <- newFamInstTyConName (noLoc (tyConName fam_tc)) pat_tys'
       ; let axiom = mkSingleCoAxiom rep_tc_name tvs' fam_tc pat_tys' rhs'
       ; traceTc "mk_deflt_at_instance" (vcat [ ppr fam_tc, ppr rhs_ty
                                              , pprCoAxiom axiom ])
       ; fam_inst <- ASSERT( tyVarsOfType rhs' `subVarSet` tv_set' )
                     newFamInst SynFamilyInst axiom
       ; return [fam_inst] }

   -- No defaults ==> generate a warning
  | otherwise  -- defs = Nothing
  = do { warnMissingMethodOrAT "associated type" (tyConName fam_tc)
       ; return [] }
  where
    subst_tv subst tc_tv
      | Just ty <- lookupVarEnv (getTvSubstEnv subst) tc_tv
      = (subst, ty)
      | otherwise
      = (extendTvSubst subst tc_tv ty', ty')
      where
        ty' = mkTyVarTy (updateTyVarKind (substTy subst) tc_tv)

{-
************************************************************************
*                                                                      *
               Type checking family instances
*                                                                      *
************************************************************************

Family instances are somewhat of a hybrid.  They are processed together with
class instance heads, but can contain data constructors and hence they share a
lot of kinding and type checking code with ordinary algebraic data types (and
GADTs).
-}

tcFamInstDeclCombined :: Maybe (Class, VarEnv Type) -- the class & mini_env if applicable
                      -> Located Name -> TcM TyCon
tcFamInstDeclCombined mb_clsinfo fam_tc_lname
  = do { -- Type family instances require -XTypeFamilies
         -- and can't (currently) be in an hs-boot file
       ; traceTc "tcFamInstDecl" (ppr fam_tc_lname)
       ; type_families <- xoptM LangExt.TypeFamilies
       ; is_boot <- tcIsHsBootOrSig   -- Are we compiling an hs-boot file?
       ; checkTc type_families $ badFamInstDecl fam_tc_lname
       ; checkTc (not is_boot) $ badBootFamInstDeclErr

       -- Look up the family TyCon and check for validity including
       -- check that toplevel type instances are not for associated types.
       ; fam_tc <- tcLookupLocatedTyCon fam_tc_lname
       ; when (isNothing mb_clsinfo &&   -- Not in a class decl
               isTyConAssoc fam_tc)      -- but an associated type
              (addErr $ assocInClassErr fam_tc_lname)

       ; return fam_tc }

tcTyFamInstDecl :: Maybe (Class, VarEnv Type) -- the class & mini_env if applicable
                -> LTyFamInstDecl Name -> TcM FamInst
  -- "type instance"
tcTyFamInstDecl mb_clsinfo (L loc decl@(TyFamInstDecl { tfid_eqn = eqn }))
  = setSrcSpan loc           $
    tcAddTyFamInstCtxt decl  $
    do { let fam_lname = tfe_tycon (unLoc eqn)
       ; fam_tc <- tcFamInstDeclCombined mb_clsinfo fam_lname

         -- (0) Check it's an open type family
       ; checkTc (isFamilyTyCon fam_tc)        (notFamily fam_tc)
       ; checkTc (isTypeFamilyTyCon fam_tc)    (wrongKindOfFamily fam_tc)
       ; checkTc (isOpenTypeFamilyTyCon fam_tc) (notOpenFamily fam_tc)

         -- (1) do the work of verifying the synonym group
       ; co_ax_branch <- tcTyFamInstEqn (famTyConShape fam_tc) eqn

         -- (2) check for validity
       ; checkValidTyFamInst mb_clsinfo fam_tc co_ax_branch

         -- (3) construct coercion axiom
       ; rep_tc_name <- newFamInstAxiomName loc (unLoc fam_lname)
                                            [co_ax_branch]
       ; let axiom = mkUnbranchedCoAxiom rep_tc_name fam_tc co_ax_branch
       ; newFamInst SynFamilyInst axiom }

tcDataFamInstDecl :: Maybe (Class, VarEnv Type)
                  -> LDataFamInstDecl Name -> TcM FamInst
  -- "newtype instance" and "data instance"
tcDataFamInstDecl mb_clsinfo
    (L loc decl@(DataFamInstDecl
       { dfid_pats = pats
       , dfid_tycon = fam_tc_name
       , dfid_defn = defn@HsDataDefn { dd_ND = new_or_data, dd_metaData = (cType, _)
                                     , dd_ctxt = ctxt, dd_cons = cons } }))
  = setSrcSpan loc             $
    tcAddDataFamInstCtxt decl  $
    do { fam_tc <- tcFamInstDeclCombined mb_clsinfo fam_tc_name

         -- Check that the family declaration is for the right kind
       ; checkTc (isFamilyTyCon fam_tc) (notFamily fam_tc)
       ; checkTc (isAlgTyCon fam_tc) (wrongKindOfFamily fam_tc)

         -- Kind check type patterns
       ; tcFamTyPats (famTyConShape fam_tc) pats
                     (kcDataDefn defn) $
           \tvs' pats' res_kind -> do

       { -- Check that left-hand side contains no type family applications
         -- (vanilla synonyms are fine, though, and we checked for
         --  foralls earlier)
         checkValidFamPats fam_tc tvs' pats'
         -- Check that type patterns match class instance head, if any
       ; checkConsistentFamInst mb_clsinfo fam_tc tvs' pats'

         -- Result kind must be '*' (otherwise, we have too few patterns)
       ; checkTc (isLiftedTypeKind res_kind) $ tooFewParmsErr (tyConArity fam_tc)

       ; stupid_theta <- tcHsContext ctxt
       ; gadt_syntax <- dataDeclChecks (tyConName fam_tc) new_or_data stupid_theta cons

         -- Construct representation tycon
       ; rep_tc_name <- newFamInstTyConName fam_tc_name pats'
       ; axiom_name  <- newImplicitBinder rep_tc_name mkInstTyCoOcc
       ; let orig_res_ty = mkTyConApp fam_tc pats'

       ; (rep_tc, fam_inst) <- fixM $ \ ~(rec_rep_tc, _) ->
           do { data_cons <- tcConDecls new_or_data rec_rep_tc
                                        (tvs', orig_res_ty) cons
              ; tc_rhs <- case new_or_data of
                     DataType -> return (mkDataTyConRhs data_cons)
                     NewType  -> ASSERT( not (null data_cons) )
                                 mkNewTyConRhs rep_tc_name rec_rep_tc (head data_cons)
              -- freshen tyvars
              ; let (eta_tvs, eta_pats) = eta_reduce tvs' pats'
                    axiom    = mkSingleCoAxiom axiom_name eta_tvs fam_tc eta_pats
                                               (mkTyConApp rep_tc (mkTyVarTys eta_tvs))
                    parent   = FamInstTyCon axiom fam_tc pats'
                    roles    = map (const Nominal) tvs'
                    rep_tc   = buildAlgTyCon rep_tc_name tvs' roles
                                             (fmap unLoc cType) stupid_theta
                                             tc_rhs
                                             Recursive
                                             False      -- No promotable to the kind level
                                             gadt_syntax parent
                 -- We always assume that indexed types are recursive.  Why?
                 -- (1) Due to their open nature, we can never be sure that a
                 -- further instance might not introduce a new recursive
                 -- dependency.  (2) They are always valid loop breakers as
                 -- they involve a coercion.
              ; fam_inst <- newFamInst (DataFamilyInst rep_tc) axiom
              ; return (rep_tc, fam_inst) }

         -- Remember to check validity; no recursion to worry about here
       ; checkValidTyCon rep_tc
       ; return fam_inst } }
  where
    -- See Note [Eta reduction for data family axioms]
    --  [a,b,c,d].T [a] c Int c d  ==>  [a,b,c]. T [a] c Int c
    eta_reduce tvs pats = go (reverse tvs) (reverse pats)
    go (tv:tvs) (pat:pats)
      | Just tv' <- getTyVar_maybe pat
      , tv == tv'
      , not (tv `elemVarSet` tyVarsOfTypes pats)
      = go tvs pats
    go tvs pats = (reverse tvs, reverse pats)

{-
Note [Eta reduction for data family axioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
   data family T a b :: *
   newtype instance T Int a = MkT (IO a) deriving( Monad )
We'd like this to work.  From the 'newtype instance' you might
think we'd get:
   newtype TInt a = MkT (IO a)
   axiom ax1 a :: T Int a ~ TInt a   -- The type-instance part
   axiom ax2 a :: TInt a ~ IO a      -- The newtype part

But now what can we do?  We have this problem
   Given:   d  :: Monad IO
   Wanted:  d' :: Monad (T Int) = d |> ????
What coercion can we use for the ???

Solution: eta-reduce both axioms, thus:
   axiom ax1 :: T Int ~ TInt
   axiom ax2 :: TInt ~ IO
Now
   d' = d |> Monad (sym (ax2 ; ax1))

This eta reduction happens both for data instances and newtype instances.

See Note [Newtype eta] in TyCon.



************************************************************************
*                                                                      *
      Type-checking instance declarations, pass 2
*                                                                      *
************************************************************************
-}

tcInstDecls2 :: [LTyClDecl Name] -> [InstInfo Name]
             -> TcM (LHsBinds Id)
-- (a) From each class declaration,
--      generate any default-method bindings
-- (b) From each instance decl
--      generate the dfun binding

tcInstDecls2 tycl_decls inst_decls
  = do  { -- (a) Default methods from class decls
          let class_decls = filter (isClassDecl . unLoc) tycl_decls
        ; dm_binds_s <- mapM tcClassDecl2 class_decls
        ; let dm_binds = unionManyBags dm_binds_s

          -- (b) instance declarations
        ; let dm_ids = collectHsBindsBinders dm_binds
              -- Add the default method Ids (again)
              -- See Note [Default methods and instances]
        ; inst_binds_s <- tcExtendLetEnv TopLevel TopLevel dm_ids $
                          mapM tcInstDecl2 inst_decls

          -- Done
        ; return (dm_binds `unionBags` unionManyBags inst_binds_s) }

{-
See Note [Default methods and instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The default method Ids are already in the type environment (see Note
[Default method Ids and Template Haskell] in TcTyClsDcls), BUT they
don't have their InlinePragmas yet.  Usually that would not matter,
because the simplifier propagates information from binding site to
use.  But, unusually, when compiling instance decls we *copy* the
INLINE pragma from the default method to the method for that
particular operation (see Note [INLINE and default methods] below).

So right here in tcInstDecls2 we must re-extend the type envt with
the default method Ids replete with their INLINE pragmas.  Urk.
-}

tcInstDecl2 :: InstInfo Name -> TcM (LHsBinds Id)
            -- Returns a binding for the dfun
tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = ibinds })
  = recoverM (return emptyLHsBinds)             $
    setSrcSpan loc                              $
    addErrCtxt (instDeclCtxt2 (idType dfun_id)) $
    do {  -- Instantiate the instance decl with skolem constants
       ; (inst_tyvars, dfun_theta, inst_head) <- tcSkolDFunType (idType dfun_id)
                     -- We instantiate the dfun_id with superSkolems.
                     -- See Note [Subtle interaction of recursion and overlap]
                     -- and Note [Binding when looking up instances]
       ; let (clas, inst_tys) = tcSplitDFunHead inst_head
             (class_tyvars, sc_theta, _, op_items) = classBigSig clas
             sc_theta' = substTheta (zipOpenTvSubst class_tyvars inst_tys) sc_theta

       ; dfun_ev_vars <- newEvVars dfun_theta

       ; sc_ev_vars <- tcSuperClasses dfun_id inst_tyvars dfun_ev_vars sc_theta'

       -- Deal with 'SPECIALISE instance' pragmas
       -- See Note [SPECIALISE instance pragmas]
       ; spec_inst_info@(spec_inst_prags,_) <- tcSpecInstPrags dfun_id ibinds

        -- Typecheck the methods
       ; (meth_ids, meth_binds)
           <- tcInstanceMethods dfun_id clas inst_tyvars dfun_ev_vars
                                inst_tys spec_inst_info
                                op_items ibinds

       -- Create the result bindings
       ; self_dict <- newDict clas inst_tys
       ; let class_tc      = classTyCon clas
             [dict_constr] = tyConDataCons class_tc
             dict_bind     = mkVarBind self_dict (L loc con_app_args)

                     -- We don't produce a binding for the dict_constr; instead we
                     -- rely on the simplifier to unfold this saturated application
                     -- We do this rather than generate an HsCon directly, because
                     -- it means that the special cases (e.g. dictionary with only one
                     -- member) are dealt with by the common MkId.mkDataConWrapId
                     -- code rather than needing to be repeated here.
                     --    con_app_tys  = MkD ty1 ty2
                     --    con_app_scs  = MkD ty1 ty2 sc1 sc2
                     --    con_app_args = MkD ty1 ty2 sc1 sc2 op1 op2
             con_app_tys  = wrapId (mkWpTyApps inst_tys)
                                   (dataConWrapId dict_constr)
             con_app_scs  = mkHsWrap (mkWpEvApps (map EvId sc_ev_vars)) con_app_tys
             con_app_args = foldl app_to_meth con_app_scs meth_ids

             app_to_meth :: HsExpr Id -> Id -> HsExpr Id
             app_to_meth fun meth_id = L loc fun `HsApp` L loc (wrapId arg_wrapper meth_id)

             inst_tv_tys = mkTyVarTys inst_tyvars
             arg_wrapper = mkWpEvVarApps dfun_ev_vars <.> mkWpTyApps inst_tv_tys

                -- Do not inline the dfun; instead give it a magic DFunFunfolding
             dfun_spec_prags
                | isNewTyCon class_tc = SpecPrags []
                    -- Newtype dfuns just inline unconditionally,
                    -- so don't attempt to specialise them
                | otherwise
                = SpecPrags spec_inst_prags

             export = ABE { abe_wrap = idHsWrapper, abe_poly = dfun_id
                          , abe_mono = self_dict, abe_prags = dfun_spec_prags }
                          -- NB: see Note [SPECIALISE instance pragmas]
             main_bind = AbsBinds { abs_tvs = inst_tyvars
                                  , abs_ev_vars = dfun_ev_vars
                                  , abs_exports = [export]
                                  , abs_ev_binds = emptyTcEvBinds
                                  , abs_binds = unitBag dict_bind }

       ; return (unitBag (L loc main_bind) `unionBags`
                 listToBag meth_binds)
       }
 where
   dfun_id = instanceDFunId ispec
   loc     = getSrcSpan dfun_id

------------------------------
tcSuperClasses :: DFunId -> [TcTyVar] -> [EvVar] -> TcThetaType
               -> TcM [EvVar]
-- See Note [Silent superclass arguments]
tcSuperClasses dfun_id inst_tyvars dfun_ev_vars sc_theta
  | null inst_tyvars && null dfun_ev_vars
  = emitWanteds ScOrigin sc_theta

  | otherwise
  = do {   -- Check that all superclasses can be deduced from
           -- the originally-specified dfun arguments
       ; _ <- checkConstraints InstSkol inst_tyvars orig_ev_vars $
              emitWanteds ScOrigin sc_theta

       ; return (map (find dfun_ev_vars) sc_theta) }
  where
    n_silent     = dfunNSilent dfun_id
    orig_ev_vars = drop n_silent dfun_ev_vars

    find [] pred
      = pprPanic "tcInstDecl2" (ppr dfun_id $$ ppr (idType dfun_id) $$ ppr pred)
    find (ev:evs) pred
      | pred `eqPred` evVarPred ev = ev
      | otherwise                  = find evs pred

----------------------
mkMethIds :: HsSigFun -> Class -> [TcTyVar] -> [EvVar]
          -> [TcType] -> Id -> TcM (TcId, TcSigInfo, HsWrapper)
mkMethIds sig_fn clas tyvars dfun_ev_vars inst_tys sel_id
  = do  { poly_meth_name  <- newName (mkClassOpAuxOcc sel_occ)
        ; local_meth_name <- newName sel_occ
                  -- Base the local_meth_name on the selector name, because
                  -- type errors from tcInstanceMethodBody come from here
        ; let poly_meth_id  = mkLocalId poly_meth_name  poly_meth_ty
              local_meth_id = mkLocalId local_meth_name local_meth_ty

        ; case lookupHsSig sig_fn sel_name of
            Just lhs_ty  -- There is a signature in the instance declaration
                         -- See Note [Instance method signatures]
               -> setSrcSpan (getLoc lhs_ty) $
                  do { inst_sigs <- xoptM LangExt.InstanceSigs
                     ; checkTc inst_sigs (misplacedInstSig sel_name lhs_ty)
                     ; sig_ty  <- tcHsSigType (FunSigCtxt sel_name) lhs_ty
                     ; let poly_sig_ty = mkSigmaTy tyvars theta sig_ty
                     ; tc_sig  <- instTcTySig lhs_ty sig_ty Nothing [] local_meth_name
                     ; hs_wrap <- addErrCtxtM (methSigCtxt sel_name poly_sig_ty poly_meth_ty) $
                                  tcSubType (FunSigCtxt sel_name) poly_sig_ty poly_meth_ty
                     ; return (poly_meth_id, tc_sig, hs_wrap) }

            Nothing     -- No type signature
               -> do { tc_sig <- instTcTySigFromId local_meth_id
                     ; return (poly_meth_id, tc_sig, idHsWrapper) } }
              -- Absent a type sig, there are no new scoped type variables here
              -- Only the ones from the instance decl itself, which are already
              -- in scope.  Example:
              --      class C a where { op :: forall b. Eq b => ... }
              --      instance C [c] where { op = <rhs> }
              -- In <rhs>, 'c' is scope but 'b' is not!
  where
    sel_name      = idName sel_id
    sel_occ       = nameOccName sel_name
    local_meth_ty = instantiateMethod clas sel_id inst_tys
    poly_meth_ty  = mkSigmaTy tyvars theta local_meth_ty
    theta         = map idType dfun_ev_vars

methSigCtxt :: Name -> TcType -> TcType -> TidyEnv -> TcM (TidyEnv, MsgDoc)
methSigCtxt sel_name sig_ty meth_ty env0
  = do { (env1, sig_ty)  <- zonkTidyTcType env0 sig_ty
       ; (env2, meth_ty) <- zonkTidyTcType env1 meth_ty
       ; let msg = hang (ptext (sLit "When checking that instance signature for") <+> quotes (ppr sel_name))
                      2 (vcat [ ptext (sLit "is more general than its signature in the class")
                              , ptext (sLit "Instance sig:") <+> ppr sig_ty
                              , ptext (sLit "   Class sig:") <+> ppr meth_ty ])
       ; return (env2, msg) }

misplacedInstSig :: Name -> LHsType Name -> SDoc
misplacedInstSig name hs_ty
  = vcat [ hang (ptext (sLit "Illegal type signature in instance declaration:"))
              2 (hang (pprPrefixName name)
                    2 (dcolon <+> ppr hs_ty))
         , ptext (sLit "(Use InstanceSigs to allow this)") ]

------------------------------
tcSpecInstPrags :: DFunId -> InstBindings Name
                -> TcM ([Located TcSpecPrag], PragFun)
tcSpecInstPrags dfun_id (InstBindings { ib_binds = binds, ib_pragmas = uprags })
  = do { spec_inst_prags <- mapM (wrapLocM (tcSpecInst dfun_id)) $
                            filter isSpecInstLSig uprags
             -- The filter removes the pragmas for methods
       ; return (spec_inst_prags, mkPragFun uprags binds) }

{-
Note [Instance method signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With -XInstanceSigs we allow the user to supply a signature for the
method in an instance declaration.  Here is an artificial example:

       data Age = MkAge Int
       instance Ord Age where
         compare :: a -> a -> Bool
         compare = error "You can't compare Ages"

We achieve this by building a TcSigInfo for the method, whether or not
there is an instance method signature, and using that to typecheck
the declaration (in tcInstanceMethodBody).  That means, conveniently,
that the type variables bound in the signature will scope over the body.

What about the check that the instance method signature is more
polymorphic than the instantiated class method type?  We just do a
tcSubType call in mkMethIds, and use the HsWrapper thus generated in
the method AbsBind.  It's very like the tcSubType impedance-matching
call in mkExport.  We have to pass the HsWrapper into
tcInstanceMethodBody.


Note [Silent superclass arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #3731, #4809, #5751, #5913, #6117, which all
describe somewhat more complicated situations, but ones
encountered in practice.

      THE PROBLEM

The problem is that it is all too easy to create a class whose
superclass is bottom when it should not be.

Consider the following (extreme) situation:
        class C a => D a where ...
        instance D [a] => D [a] where ...   (dfunD)
        instance C [a] => C [a] where ...   (dfunC)
Although this looks wrong (assume D [a] to prove D [a]), it is only a
more extreme case of what happens with recursive dictionaries, and it
can, just about, make sense because the methods do some work before
recursing.

To implement the dfunD we must generate code for the superclass C [a],
which we had better not get by superclass selection from the supplied
argument:
       dfunD :: forall a. D [a] -> D [a]
       dfunD = \d::D [a] -> MkD (scsel d) ..

Otherwise if we later encounter a situation where
we have a [Wanted] dw::D [a] we might solve it thus:
     dw := dfunD dw
Which is all fine except that now ** the superclass C is bottom **!

The instance we want is:
       dfunD :: forall a. D [a] -> D [a]
       dfunD = \d::D [a] -> MkD (dfunC (scsel d)) ...

      THE SOLUTION

Our solution to this problem "silent superclass arguments".  We pass
to each dfun some ``silent superclass arguments’’, which are the
immediate superclasses of the dictionary we are trying to
construct. In our example:
       dfun :: forall a. C [a] -> D [a] -> D [a]
       dfun = \(dc::C [a]) (dd::D [a]) -> DOrd dc ...
Notice the extra (dc :: C [a]) argument compared to the previous version.

This gives us:

     -----------------------------------------------------------
     DFun Superclass Invariant
     ~~~~~~~~~~~~~~~~~~~~~~~~
     In the body of a DFun, every superclass argument to the
     returned dictionary is
       either   * one of the arguments of the DFun,
       or       * constant, bound at top level
     -----------------------------------------------------------

This net effect is that it is safe to treat a dfun application as
wrapping a dictionary constructor around its arguments (in particular,
a dfun never picks superclasses from the arguments under the
dictionary constructor). No superclass is hidden inside a dfun
application.

The extra arguments required to satisfy the DFun Superclass Invariant
always come first, and are called the "silent" arguments.  You can
find out how many silent arguments there are using Id.dfunNSilent;
and then you can just drop that number of arguments to see the ones
that were in the original instance declaration.

DFun types are built (only) by MkId.mkDictFunId, so that is where we
decide what silent arguments are to be added.

In our example, if we had  [Wanted] dw :: D [a] we would get via the instance:
    dw := dfun d1 d2
    [Wanted] (d1 :: C [a])
    [Wanted] (d2 :: D [a])

And now, though we *can* solve:
     d2 := dw
That's fine; and we solve d1:C[a] separately.

Test case SCLoop tests this fix.

Note [SPECIALISE instance pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   instance (Ix a, Ix b) => Ix (a,b) where
     {-# SPECIALISE instance Ix (Int,Int) #-}
     range (x,y) = ...

We make a specialised version of the dictionary function, AND
specialised versions of each *method*.  Thus we should generate
something like this:

  $dfIxPair :: (Ix a, Ix b) => Ix (a,b)
  {-# DFUN [$crangePair, ...] #-}
  {-# SPECIALISE $dfIxPair :: Ix (Int,Int) #-}
  $dfIxPair da db = Ix ($crangePair da db) (...other methods...)

  $crange :: (Ix a, Ix b) -> ((a,b),(a,b)) -> [(a,b)]
  {-# SPECIALISE $crange :: ((Int,Int),(Int,Int)) -> [(Int,Int)] #-}
  $crange da db = <blah>

The SPECIALISE pragmas are acted upon by the desugarer, which generate

  dii :: Ix Int
  dii = ...

  $s$dfIxPair :: Ix ((Int,Int),(Int,Int))
  {-# DFUN [$crangePair di di, ...] #-}
  $s$dfIxPair = Ix ($crangePair di di) (...)

  {-# RULE forall (d1,d2:Ix Int). $dfIxPair Int Int d1 d2 = $s$dfIxPair #-}

  $s$crangePair :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
  $c$crangePair = ...specialised RHS of $crangePair...

  {-# RULE forall (d1,d2:Ix Int). $crangePair Int Int d1 d2 = $s$crangePair #-}

Note that

  * The specialised dictionary $s$dfIxPair is very much needed, in case we
    call a function that takes a dictionary, but in a context where the
    specialised dictionary can be used.  See Trac #7797.

  * The ClassOp rule for 'range' works equally well on $s$dfIxPair, because
    it still has a DFunUnfolding.  See Note [ClassOp/DFun selection]

  * A call (range ($dfIxPair Int Int d1 d2)) might simplify two ways:
       --> {ClassOp rule for range}     $crangePair Int Int d1 d2
       --> {SPEC rule for $crangePair}  $s$crangePair
    or thus:
       --> {SPEC rule for $dfIxPair}    range $s$dfIxPair
       --> {ClassOpRule for range}      $s$crangePair
    It doesn't matter which way.

  * We want to specialise the RHS of both $dfIxPair and $crangePair,
    but the SAME HsWrapper will do for both!  We can call tcSpecPrag
    just once, and pass the result (in spec_inst_info) to tcInstanceMethods.
-}

tcSpecInst :: Id -> Sig Name -> TcM TcSpecPrag
tcSpecInst dfun_id prag@(SpecInstSig _src hs_ty)
  = addErrCtxt (spec_ctxt prag) $
    do  { (tyvars, theta, clas, tys) <- tcHsInstHead SpecInstCtxt hs_ty
        ; let (_, spec_dfun_ty) = mkDictFunTy tyvars theta clas tys

        ; co_fn <- tcSubType SpecInstCtxt (idType dfun_id) spec_dfun_ty
        ; return (SpecPrag dfun_id co_fn defaultInlinePragma) }
  where
    spec_ctxt prag = hang (ptext (sLit "In the SPECIALISE pragma")) 2 (ppr prag)

tcSpecInst _  _ = panic "tcSpecInst"

{-
************************************************************************
*                                                                      *
      Type-checking an instance method
*                                                                      *
************************************************************************

tcInstanceMethod
- Make the method bindings, as a [(NonRec, HsBinds)], one per method
- Remembering to use fresh Name (the instance method Name) as the binder
- Bring the instance method Ids into scope, for the benefit of tcInstSig
- Use sig_fn mapping instance method Name -> instance tyvars
- Ditto prag_fn
- Use tcValBinds to do the checking
-}

tcInstanceMethods :: DFunId -> Class -> [TcTyVar]
                  -> [EvVar]
                  -> [TcType]
                  -> ([Located TcSpecPrag], PragFun)
                  -> [(Id, DefMeth)]
                  -> InstBindings Name
                  -> TcM ([Id], [LHsBind Id])
        -- The returned inst_meth_ids all have types starting
        --      forall tvs. theta => ...
tcInstanceMethods dfun_id clas tyvars dfun_ev_vars inst_tys
                  (spec_inst_prags, prag_fn)
                  op_items (InstBindings { ib_binds = binds
                                         , ib_tyvars = lexical_tvs
                                         , ib_pragmas = sigs
                                         , ib_extensions = exts
                                         , ib_derived    = is_derived })
  = tcExtendTyVarEnv2 (lexical_tvs `zip` tyvars) $
       -- The lexical_tvs scope over the 'where' part
    do { traceTc "tcInstMeth" (ppr sigs $$ ppr binds)
       ; let hs_sig_fn = mkHsSigFun sigs
       ; checkMinimalDefinition
       ; set_exts exts $ mapAndUnzipM (tc_item hs_sig_fn) op_items }
  where
    set_exts :: [LangExt.Extension] -> TcM a -> TcM a
    set_exts es thing = foldr setXOptM thing es

    ----------------------
    tc_item :: HsSigFun -> (Id, DefMeth) -> TcM (Id, LHsBind Id)
    tc_item sig_fn (sel_id, dm_info)
      = case findMethodBind (idName sel_id) binds of
            Just (user_bind, bndr_loc)
                     -> tc_body sig_fn sel_id user_bind bndr_loc
            Nothing  -> do { traceTc "tc_def" (ppr sel_id)
                           ; tc_default sig_fn sel_id dm_info }

    ----------------------
    tc_body :: HsSigFun -> Id -> LHsBind Name
            -> SrcSpan -> TcM (TcId, LHsBind Id)
    tc_body sig_fn sel_id rn_bind bndr_loc
      = add_meth_ctxt sel_id rn_bind $
        do { traceTc "tc_item" (ppr sel_id <+> ppr (idType sel_id))
           ; (meth_id, local_meth_sig, hs_wrap)
                  <- setSrcSpan bndr_loc $
                     mkMethIds sig_fn clas tyvars dfun_ev_vars
                               inst_tys sel_id
           ; let prags = prag_fn (idName sel_id)
           ; meth_id1 <- addInlinePrags meth_id prags
           ; spec_prags <- tcSpecPrags meth_id1 prags
           ; bind <- tcInstanceMethodBody InstSkol
                          tyvars dfun_ev_vars
                          meth_id1 local_meth_sig hs_wrap
                          (mk_meth_spec_prags meth_id1 spec_prags)
                          rn_bind
           ; return (meth_id1, bind) }

    ----------------------
    tc_default :: HsSigFun -> Id -> DefMeth -> TcM (TcId, LHsBind Id)

    tc_default sig_fn sel_id (GenDefMeth dm_name)
      = do { meth_bind <- mkGenericDefMethBind clas inst_tys sel_id dm_name
           ; tc_body sig_fn sel_id meth_bind inst_loc }

    tc_default sig_fn sel_id NoDefMeth     -- No default method at all
      = do { traceTc "tc_def: warn" (ppr sel_id)
           ; (meth_id, _, _) <- mkMethIds sig_fn clas tyvars dfun_ev_vars
                                          inst_tys sel_id
           ; dflags <- getDynFlags
           ; return (meth_id,
                     mkVarBind meth_id $
                       mkLHsWrap lam_wrapper (error_rhs dflags)) }
      where
        error_rhs dflags = L inst_loc $ HsApp error_fun (error_msg dflags)
        error_fun    = L inst_loc $ wrapId (WpTyApp meth_tau) nO_METHOD_BINDING_ERROR_ID
        error_msg dflags = L inst_loc (HsLit (HsStringPrim ""
                                    (unsafeMkByteString (error_string dflags))))
        meth_tau     = funResultTy (applyTys (idType sel_id) inst_tys)
        error_string dflags = showSDoc dflags (hcat [ppr inst_loc, text "|", ppr sel_id ])
        lam_wrapper  = mkWpTyLams tyvars <.> mkWpLams dfun_ev_vars

    tc_default sig_fn sel_id (DefMeth dm_name) -- A polymorphic default method
      = do {     -- Build the typechecked version directly,
                 -- without calling typecheck_method;
                 -- see Note [Default methods in instances]
                 -- Generate   /\as.\ds. let self = df as ds
                 --                      in $dm inst_tys self
                 -- The 'let' is necessary only because HsSyn doesn't allow
                 -- you to apply a function to a dictionary *expression*.

           ; self_dict <- newDict clas inst_tys
           ; let self_ev_bind = EvBind self_dict
                                (EvDFunApp dfun_id (mkTyVarTys tyvars) (map EvId dfun_ev_vars))

           ; (meth_id, local_meth_sig, hs_wrap)
                   <- mkMethIds sig_fn clas tyvars dfun_ev_vars inst_tys sel_id
           ; dm_id <- tcLookupId dm_name
           ; let dm_inline_prag = idInlinePragma dm_id
                 rhs = HsWrap (mkWpEvVarApps [self_dict] <.> mkWpTyApps inst_tys) $
                       HsVar dm_id

                 local_meth_id = sig_id local_meth_sig
                 meth_bind = mkVarBind local_meth_id (L inst_loc rhs)
                 meth_id1 = meth_id `setInlinePragma` dm_inline_prag
                        -- Copy the inline pragma (if any) from the default
                        -- method to this version. Note [INLINE and default methods]


                 export = ABE { abe_wrap = hs_wrap, abe_poly = meth_id1
                              , abe_mono = local_meth_id
                              , abe_prags = mk_meth_spec_prags meth_id1 [] }
                 bind = AbsBinds { abs_tvs = tyvars, abs_ev_vars = dfun_ev_vars
                                 , abs_exports = [export]
                                 , abs_ev_binds = EvBinds (unitBag self_ev_bind)
                                 , abs_binds    = unitBag meth_bind }
             -- Default methods in an instance declaration can't have their own
             -- INLINE or SPECIALISE pragmas. It'd be possible to allow them, but
             -- currently they are rejected with
             --           "INLINE pragma lacks an accompanying binding"

           ; return (meth_id1, L inst_loc bind) }

    ----------------------
    mk_meth_spec_prags :: Id -> [LTcSpecPrag] -> TcSpecPrags
        -- Adapt the 'SPECIALISE instance' pragmas to work for this method Id
        -- There are two sources:
        --   * spec_prags_for_me: {-# SPECIALISE op :: <blah> #-}
        --   * spec_prags_from_inst: derived from {-# SPECIALISE instance :: <blah> #-}
        --     These ones have the dfun inside, but [perhaps surprisingly]
        --     the correct wrapper.
    mk_meth_spec_prags meth_id spec_prags_for_me
      = SpecPrags (spec_prags_for_me ++ spec_prags_from_inst)
      where
        spec_prags_from_inst
           | isInlinePragma (idInlinePragma meth_id)
           = []  -- Do not inherit SPECIALISE from the instance if the
                 -- method is marked INLINE, because then it'll be inlined
                 -- and the specialisation would do nothing. (Indeed it'll provoke
                 -- a warning from the desugarer
           | otherwise
           = [ L inst_loc (SpecPrag meth_id wrap inl)
             | L inst_loc (SpecPrag _ wrap inl) <- spec_inst_prags]

    inst_loc = getSrcSpan dfun_id

        -- For instance decls that come from deriving clauses
        -- we want to print out the full source code if there's an error
        -- because otherwise the user won't see the code at all
    add_meth_ctxt sel_id rn_bind thing
      | is_derived = addLandmarkErrCtxt (derivBindCtxt sel_id clas inst_tys rn_bind) thing
      | otherwise  = thing

    ----------------------

    -- check if one of the minimal complete definitions is satisfied
    checkMinimalDefinition
      = whenIsJust (isUnsatisfied methodExists (classMinimalDef clas)) $
          warnUnsatisfiedMinimalDefinition
      where
      methodExists meth = isJust (findMethodBind meth binds)

mkGenericDefMethBind :: Class -> [Type] -> Id -> Name -> TcM (LHsBind Name)
mkGenericDefMethBind clas inst_tys sel_id dm_name
  =     -- A generic default method
        -- If the method is defined generically, we only have to call the
        -- dm_name.
    do  { dflags <- getDynFlags
        ; liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Filling in method body"
                   (vcat [ppr clas <+> ppr inst_tys,
                          nest 2 (ppr sel_id <+> equals <+> ppr rhs)]))

        ; return (noLoc $ mkTopFunBind Generated (noLoc (idName sel_id))
                                       [mkSimpleMatch [] rhs]) }
  where
    rhs = nlHsVar dm_name

----------------------
wrapId :: HsWrapper -> id -> HsExpr id
wrapId wrapper id = mkHsWrap wrapper (HsVar id)

derivBindCtxt :: Id -> Class -> [Type ] -> LHsBind Name -> SDoc
derivBindCtxt sel_id clas tys _bind
   = vcat [ ptext (sLit "When typechecking the code for ") <+> quotes (ppr sel_id)
          , nest 2 (ptext (sLit "in a derived instance for")
                    <+> quotes (pprClassPred clas tys) <> colon)
          , nest 2 $ ptext (sLit "To see the code I am typechecking, use -ddump-deriv") ]

warnMissingMethodOrAT :: String -> Name -> TcM ()
warnMissingMethodOrAT what name
  = do { warn <- woptM Opt_WarnMissingMethods
       ; traceTc "warn" (ppr name <+> ppr warn <+> ppr (not (startsWithUnderscore (getOccName name))))
       ; warnTc (Reason Opt_WarnMissingMethods) (warn  -- Warn only if -fwarn-missing-methods
                 && not (startsWithUnderscore (getOccName name)))
                                        -- Don't warn about _foo methods
                (ptext (sLit "No explicit") <+> text what <+> ptext (sLit "or default declaration for")
                 <+> quotes (ppr name)) }

warnUnsatisfiedMinimalDefinition :: ClassMinimalDef -> TcM ()
warnUnsatisfiedMinimalDefinition mindef
  = do { warn <- woptM Opt_WarnMissingMethods
       ; warnTc (Reason Opt_WarnMissingMethods) warn message
       }
  where
    message = vcat [ptext (sLit "No explicit implementation for")
                   ,nest 2 $ pprBooleanFormulaNice mindef
                   ]

{-
Note [Export helper functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange to export the "helper functions" of an instance declaration,
so that they are not subject to preInlineUnconditionally, even if their
RHS is trivial.  Reason: they are mentioned in the DFunUnfolding of
the dict fun as Ids, not as CoreExprs, so we can't substitute a
non-variable for them.

We could change this by making DFunUnfoldings have CoreExprs, but it
seems a bit simpler this way.

Note [Default methods in instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this

   class Baz v x where
      foo :: x -> x
      foo y = <blah>

   instance Baz Int Int

From the class decl we get

   $dmfoo :: forall v x. Baz v x => x -> x
   $dmfoo y = <blah>

Notice that the type is ambiguous.  That's fine, though. The instance
decl generates

   $dBazIntInt = MkBaz fooIntInt
   fooIntInt = $dmfoo Int Int $dBazIntInt

BUT this does mean we must generate the dictionary translation of
fooIntInt directly, rather than generating source-code and
type-checking it.  That was the bug in Trac #1061. In any case it's
less work to generate the translated version!

Note [INLINE and default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Default methods need special case.  They are supposed to behave rather like
macros.  For example

  class Foo a where
    op1, op2 :: Bool -> a -> a

    {-# INLINE op1 #-}
    op1 b x = op2 (not b) x

  instance Foo Int where
    -- op1 via default method
    op2 b x = <blah>

The instance declaration should behave

   just as if 'op1' had been defined with the
   code, and INLINE pragma, from its original
   definition.

That is, just as if you'd written

  instance Foo Int where
    op2 b x = <blah>

    {-# INLINE op1 #-}
    op1 b x = op2 (not b) x

So for the above example we generate:

  {-# INLINE $dmop1 #-}
  -- $dmop1 has an InlineCompulsory unfolding
  $dmop1 d b x = op2 d (not b) x

  $fFooInt = MkD $cop1 $cop2

  {-# INLINE $cop1 #-}
  $cop1 = $dmop1 $fFooInt

  $cop2 = <blah>

Note carefully:

* We *copy* any INLINE pragma from the default method $dmop1 to the
  instance $cop1.  Otherwise we'll just inline the former in the
  latter and stop, which isn't what the user expected

* Regardless of its pragma, we give the default method an
  unfolding with an InlineCompulsory source. That means
  that it'll be inlined at every use site, notably in
  each instance declaration, such as $cop1.  This inlining
  must happen even though
    a) $dmop1 is not saturated in $cop1
    b) $cop1 itself has an INLINE pragma

  It's vital that $dmop1 *is* inlined in this way, to allow the mutual
  recursion between $fooInt and $cop1 to be broken

* To communicate the need for an InlineCompulsory to the desugarer
  (which makes the Unfoldings), we use the IsDefaultMethod constructor
  in TcSpecPrags.


************************************************************************
*                                                                      *
\subsection{Error messages}
*                                                                      *
************************************************************************
-}

instDeclCtxt1 :: LHsType Name -> SDoc
instDeclCtxt1 hs_inst_ty
  = inst_decl_ctxt (case unLoc hs_inst_ty of
                        HsForAllTy _ _ _ _ (L _ ty') -> ppr ty'
                        _                            -> ppr hs_inst_ty)     -- Don't expect this
instDeclCtxt2 :: Type -> SDoc
instDeclCtxt2 dfun_ty
  = inst_decl_ctxt (ppr (mkClassPred cls tys))
  where
    (_,_,cls,tys) = tcSplitDFunTy dfun_ty

inst_decl_ctxt :: SDoc -> SDoc
inst_decl_ctxt doc = hang (ptext (sLit "In the instance declaration for"))
                        2 (quotes doc)

badBootFamInstDeclErr :: SDoc
badBootFamInstDeclErr
  = ptext (sLit "Illegal family instance in hs-boot file")

notFamily :: TyCon -> SDoc
notFamily tycon
  = vcat [ ptext (sLit "Illegal family instance for") <+> quotes (ppr tycon)
         , nest 2 $ parens (ppr tycon <+> ptext (sLit "is not an indexed type family"))]

tooFewParmsErr :: Arity -> SDoc
tooFewParmsErr arity
  = ptext (sLit "Family instance has too few parameters; expected") <+>
    ppr arity

assocInClassErr :: Located Name -> SDoc
assocInClassErr name
 = ptext (sLit "Associated type") <+> quotes (ppr name) <+>
   ptext (sLit "must be inside a class instance")

badFamInstDecl :: Located Name -> SDoc
badFamInstDecl tc_name
  = vcat [ ptext (sLit "Illegal family instance for") <+>
           quotes (ppr tc_name)
         , nest 2 (parens $ ptext (sLit "Use TypeFamilies to allow indexed type families")) ]

notOpenFamily :: TyCon -> SDoc
notOpenFamily tc
  = ptext (sLit "Illegal instance for closed family") <+> quotes (ppr tc)
