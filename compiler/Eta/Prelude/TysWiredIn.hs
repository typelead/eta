{-
(c) The GRASP Project, Glasgow University, 1994-1998

\section[TysWiredIn]{Wired-in knowledge about {\em non-primitive} types}
-}

{-# LANGUAGE CPP #-}

-- | This module is about types that can be defined in Haskell, but which
--   must be wired into the compiler nonetheless.  C.f module TysPrim
module Eta.Prelude.TysWiredIn (
        -- * All wired in things
        wiredInTyCons, isBuiltInOcc_maybe,

        -- * Bool
        boolTy, boolTyCon, boolTyCon_RDR, boolTyConName,
        trueDataCon,  trueDataConId,  true_RDR,
        falseDataCon, falseDataConId, false_RDR,
        promotedBoolTyCon, promotedFalseDataCon, promotedTrueDataCon,

        -- * Ordering
        ltDataCon, ltDataConId,
        eqDataCon, eqDataConId,
        gtDataCon, gtDataConId,
        promotedOrderingTyCon,
        promotedLTDataCon, promotedEQDataCon, promotedGTDataCon,

        -- * Char
        charTyCon, charDataCon, charTyCon_RDR,
        charTy, stringTy, charTyConName,

        -- * Double
        doubleTyCon, doubleDataCon, doubleTy, doubleTyConName, doubleTyCon_RDR,

        -- * Float
        floatTyCon, floatDataCon, floatTy, floatTyConName, floatTyCon_RDR,

        -- * Int
        intTyCon, intDataCon, intTyCon_RDR, intDataCon_RDR, intTyConName,
        intTy, int64TyCon_RDR,

        -- * Word
        wordTyCon, wordDataCon, wordTyConName, wordTy,

        -- * Word8
        word8TyCon, word8DataCon, word8TyConName, word8Ty,

        -- * JString
        jstringTyCon, jstringDataCon, jstringTy, sobjectTyCon_RDR, javaTyCon_RDR,
        byteTyCon_RDR, shortTyCon_RDR, jcharTyCon_RDR, extendsClass_RDR,

        -- * List
        listTyCon, nilDataCon, nilDataConName, consDataCon, consDataConName,
        listTyCon_RDR, consDataCon_RDR, listTyConName,
        mkListTy, mkPromotedListTy,

        -- * Maybe
        maybeTyCon, maybeTyConName,
        nothingDataCon, nothingDataConName, promotedNothingDataCon,
        justDataCon, justDataConName, promotedJustDataCon,

        -- * Tuples
        mkTupleTy, mkBoxedTupleTy,
        tupleTyCon, tupleCon, tupleDataCon,
        promotedTupleTyCon, promotedTupleDataCon,
        unitTyCon, unitDataCon, unitDataConId, pairTyCon,
        unboxedUnitTyCon, unboxedUnitDataCon,
        unboxedSingletonTyCon, unboxedSingletonDataCon,
        unboxedPairTyCon, unboxedPairDataCon,

        -- * Unit
        unitTy, unitTyCon_RDR,

        -- * Kinds
        typeNatKindCon, typeNatKind, typeSymbolKindCon, typeSymbolKind,

        -- * Parallel arrays
        mkPArrTy,
        parrTyCon, parrFakeCon, isPArrTyCon, isPArrFakeCon,
        parrTyCon_RDR, parrTyConName,

        -- * Equality predicates
        eqTyCon_RDR, eqTyCon, eqTyConName, eqBoxDataCon,
        coercibleTyCon, coercibleTyConName, coercibleDataCon, coercibleClass,

        -- * Implicit Parameters
        ipTyCon, ipDataCon, ipClass,

        callStackTyCon,

        mkWiredInTyConName -- This is used in TcTypeNats to define the
                           -- built-in functions for evaluation.
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Eta.BasicTypes.MkId      ( mkDataConWorkId )
import {-# SOURCE #-} Eta.Prelude.KnownUniques ( mkTupleTyConUnique, mkTupleDataConUnique )

-- friends:
import Eta.Prelude.PrelNames
import Eta.Prelude.TysPrim
import Eta.Types.CoAxiom
import Eta.Types.Coercion
-- others:
import Eta.Main.Constants        ( mAX_TUPLE_SIZE )
import Eta.BasicTypes.Module           ( Module )
import Eta.Types.Type             ( mkTyConApp )
import Eta.BasicTypes.DataCon
import Eta.BasicTypes.ConLike
import Eta.BasicTypes.Var
import Eta.Types.TyCon
import Eta.Types.Class            ( Class, mkClass )
import Eta.Types.TypeRep
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.Name
import Eta.BasicTypes.BasicTypes       ( TupleSort(..), tupleSortBoxity,
                          Arity, RecFlag(..), Boxity(..), boxityNormalTupleSort )
import Eta.Prelude.ForeignCall
import Eta.BasicTypes.Unique           ( incrUnique, mkPArrDataConUnique )
import Data.Array
import Eta.Utils.FastString
import Eta.Utils.Outputable
import Eta.Utils.Util
import Eta.Utils.BooleanFormula   ( mkAnd )

alpha_tyvar :: [TyVar]
alpha_tyvar = [alphaTyVar]

alpha_ty :: [Type]
alpha_ty = [alphaTy]

{-
************************************************************************
*                                                                      *
\subsection{Wired in type constructors}
*                                                                      *
************************************************************************

If you change which things are wired in, make sure you change their
names in PrelNames, so they use wTcQual, wDataQual, etc
-}

-- This list is used only to define PrelInfo.wiredInThings. That in turn
-- is used to initialise the name environment carried around by the renamer.
-- This means that if we look up the name of a TyCon (or its implicit binders)
-- that occurs in this list that name will be assigned the wired-in key we
-- define here.
--
-- Because of their infinite nature, this list excludes tuples, Any and implicit
-- parameter TyCons. Instead, we have a hack in lookupOrigNameCache to deal with
-- these names.
--
-- See also Note [Known-key names]
wiredInTyCons :: [TyCon]

wiredInTyCons = [ -- Units are not treated like other tuples, because then
                  -- are defined in GHC.Base, and there's only a few of them. We
                  -- put them in wiredInTyCons so that they will pre-populate
                  -- the name cache, so the parser in isBuiltInOcc_maybe doesn't
                  -- need to look out for them.
                unitTyCon
              , unboxedUnitTyCon
              , boolTyCon
              , charTyCon
              , doubleTyCon
              , floatTyCon
              , intTyCon
              , wordTyCon
              , listTyCon
              , maybeTyCon
              , jstringTyCon
              , parrTyCon
              , eqTyCon
              , coercibleTyCon
              , typeNatKindCon
              , typeSymbolKindCon
              , ipTyCon
              ]

mkWiredInTyConName :: BuiltInSyntax -> Module -> FastString -> Unique -> TyCon -> Name
mkWiredInTyConName built_in modu fs unique tycon
  = mkWiredInName modu (mkTcOccFS fs) unique
                  (ATyCon tycon)        -- Relevant TyCon
                  built_in

mkWiredInDataConName :: BuiltInSyntax -> Module -> FastString -> Unique -> DataCon -> Name
mkWiredInDataConName built_in modu fs unique datacon
  = mkWiredInName modu (mkDataOccFS fs) unique
                  (AConLike (RealDataCon datacon))    -- Relevant DataCon
                  built_in

mkWiredInCoAxiomName :: BuiltInSyntax -> Module -> FastString -> Unique
                     -> CoAxiom Branched -> Name
mkWiredInCoAxiomName built_in modu fs unique ax
  = mkWiredInName modu (mkTcOccFS fs) unique
                  (ACoAxiom ax)        -- Relevant CoAxiom
                  built_in

-- See Note [Kind-changing of (~) and Coercible]
eqTyConName, eqBoxDataConName :: Name
eqTyConName      = mkWiredInTyConName   BuiltInSyntax gHC_TYPES (fsLit "~")   eqTyConKey      eqTyCon
eqBoxDataConName = mkWiredInDataConName UserSyntax    gHC_TYPES (fsLit "Eq#") eqBoxDataConKey eqBoxDataCon

-- See Note [Kind-changing of (~) and Coercible]
coercibleTyConName, coercibleDataConName :: Name
coercibleTyConName   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Coercible")  coercibleTyConKey   coercibleTyCon
coercibleDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "MkCoercible") coercibleDataConKey coercibleDataCon

charTyConName, charDataConName, intTyConName, intDataConName :: Name
charTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Char") charTyConKey charTyCon
charDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "C#") charDataConKey charDataCon
intTyConName      = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Int") intTyConKey   intTyCon
intDataConName    = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "I#") intDataConKey  intDataCon

boolTyConName, falseDataConName, trueDataConName :: Name
boolTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Bool") boolTyConKey boolTyCon
falseDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "False") falseDataConKey falseDataCon
trueDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "True")  trueDataConKey  trueDataCon

listTyConName, nilDataConName, consDataConName :: Name
listTyConName     = mkWiredInTyConName   BuiltInSyntax gHC_TYPES (fsLit "[]") listTyConKey listTyCon
nilDataConName    = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit "[]") nilDataConKey nilDataCon
consDataConName   = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit ":") consDataConKey consDataCon

maybeTyConName, nothingDataConName, justDataConName :: Name
maybeTyConName     = mkWiredInTyConName   UserSyntax gHC_MAYBE (fsLit "Maybe")
                                          maybeTyConKey maybeTyCon
nothingDataConName = mkWiredInDataConName UserSyntax gHC_MAYBE (fsLit "Nothing")
                                          nothingDataConKey nothingDataCon
justDataConName    = mkWiredInDataConName UserSyntax gHC_MAYBE (fsLit "Just")
                                          justDataConKey justDataCon

wordTyConName, wordDataConName, word8TyConName, word8DataConName,
  floatTyConName, floatDataConName, doubleTyConName, doubleDataConName :: Name
wordTyConName      = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Word")   wordTyConKey     wordTyCon
wordDataConName    = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "W#")     wordDataConKey   wordDataCon
word8TyConName     = mkWiredInTyConName   UserSyntax gHC_WORD  (fsLit "Word8")  word8TyConKey    word8TyCon
word8DataConName   = mkWiredInDataConName UserSyntax gHC_WORD  (fsLit "W8#")    word8DataConKey  word8DataCon
floatTyConName     = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Float")  floatTyConKey    floatTyCon
floatDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "F#")     floatDataConKey  floatDataCon
doubleTyConName    = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Double") doubleTyConKey   doubleTyCon
doubleDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "D#")     doubleDataConKey doubleDataCon

-- ETA-specific
jstringTyConName, jstringDataConName :: Name
jstringTyConName   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "JString") jstringTyConKey    jstringTyCon
jstringDataConName = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "JS#")     jstringDataConKey  jstringDataCon

-- Kinds
typeNatKindConName, typeSymbolKindConName :: Name
typeNatKindConName    = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Nat")    typeNatKindConNameKey    typeNatKindCon
typeSymbolKindConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Symbol") typeSymbolKindConNameKey typeSymbolKindCon

parrTyConName, parrDataConName :: Name
parrTyConName   = mkWiredInTyConName   BuiltInSyntax
                    gHC_PARR' (fsLit "[::]") parrTyConKey parrTyCon
parrDataConName = mkWiredInDataConName UserSyntax
                    gHC_PARR' (fsLit "PArr") parrDataConKey parrDataCon

boolTyCon_RDR, false_RDR, true_RDR, intTyCon_RDR, charTyCon_RDR,
    intDataCon_RDR, listTyCon_RDR, consDataCon_RDR, parrTyCon_RDR, eqTyCon_RDR,
    floatTyCon_RDR, doubleTyCon_RDR, int64TyCon_RDR, sobjectTyCon_RDR,
    unitTyCon_RDR, javaTyCon_RDR, byteTyCon_RDR, shortTyCon_RDR, jcharTyCon_RDR,
    extendsClass_RDR :: RdrName
boolTyCon_RDR    = nameRdrName boolTyConName
false_RDR        = nameRdrName falseDataConName
true_RDR         = nameRdrName trueDataConName
intTyCon_RDR     = nameRdrName intTyConName
charTyCon_RDR    = nameRdrName charTyConName
intDataCon_RDR   = nameRdrName intDataConName
listTyCon_RDR    = nameRdrName listTyConName
consDataCon_RDR  = nameRdrName consDataConName
parrTyCon_RDR    = nameRdrName parrTyConName
eqTyCon_RDR      = nameRdrName eqTyConName
floatTyCon_RDR   = nameRdrName floatTyConName
doubleTyCon_RDR  = nameRdrName doubleTyConName
int64TyCon_RDR   = nameRdrName int64TyConName
sobjectTyCon_RDR = nameRdrName sobjectTyConName
unitTyCon_RDR    = getRdrName unitTyCon
javaTyCon_RDR    = getRdrName javaTyConName
byteTyCon_RDR    = nameRdrName byteTyConName
shortTyCon_RDR   = nameRdrName shortTyConName
jcharTyCon_RDR   = nameRdrName jcharTyConName
extendsClass_RDR = nameRdrName extendsClassName

{-
************************************************************************
*                                                                      *
\subsection{mkWiredInTyCon}
*                                                                      *
************************************************************************
-}

pcNonRecDataTyCon :: Name -> Maybe CType -> [TyVar] -> [DataCon] -> TyCon
-- Not an enumeration, not promotable
pcNonRecDataTyCon = pcTyCon False NonRecursive False

-- This function assumes that the types it creates have all parameters at
-- Representational role!
pcTyCon :: Bool -> RecFlag -> Bool -> Name -> Maybe CType -> [TyVar] -> [DataCon] -> TyCon
pcTyCon is_enum is_rec is_prom name cType tyvars cons
  = buildAlgTyCon name
        tyvars
        (map (const Representational) tyvars)
        cType
        []              -- No stupid theta
        (DataTyCon cons is_enum)
        is_rec
        is_prom
        False           -- Not in GADT syntax
        NoParentTyCon

pcDataCon :: Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataCon = pcDataConWithFixity False

pcDataConWithFixity :: Bool -> Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataConWithFixity infx n = pcDataConWithFixity' infx n (incrUnique (nameUnique n))
-- The Name's unique is the first of two free uniques;
-- the first is used for the datacon itself,
-- the second is used for the "worker name"
--
-- To support this the mkPreludeDataConUnique function "allocates"
-- one DataCon unique per pair of Ints.

pcDataConWithFixity' :: Bool -> Name -> Unique -> [TyVar] -> [Type] -> TyCon -> DataCon
-- The Name should be in the DataName name space; it's the name
-- of the DataCon itself.

pcDataConWithFixity' declared_infix dc_name wrk_key tyvars arg_tys tycon
  = data_con
  where
    data_con = mkDataCon dc_name declared_infix
                (map (const no_bang) arg_tys)
                []      -- No labelled fields
                tyvars
                []      -- No existential type variables
                []      -- No equality spec
                []      -- No theta
                arg_tys (mkTyConApp tycon (mkTyVarTys tyvars))
                tycon
                []      -- No stupid theta
                (mkDataConWorkId wrk_name data_con)
                NoDataConRep    -- Wired-in types are too simple to need wrappers

    no_bang = HsSrcBang Nothing NoSrcUnpack NoSrcStrict
    wrk_name = mkDataConWorkerName data_con wrk_key

mkDataConWorkerName :: DataCon -> Unique -> Name
mkDataConWorkerName data_con wrk_key =
    mkWiredInName modu wrk_occ wrk_key
                  (AnId (dataConWorkId data_con)) UserSyntax
  where
    modu     = ASSERT( isExternalName dc_name )
               nameModule dc_name
    dc_name = dataConName data_con
    dc_occ  = nameOccName dc_name
    wrk_occ = mkDataConWorkerOcc dc_occ
{-
************************************************************************
*                                                                      *
      Kinds
*                                                                      *
************************************************************************
-}

typeNatKindCon, typeSymbolKindCon :: TyCon
-- data Nat
-- data Symbol
typeNatKindCon    = pcTyCon False NonRecursive True typeNatKindConName    Nothing [] []
typeSymbolKindCon = pcTyCon False NonRecursive True typeSymbolKindConName Nothing [] []

typeNatKind, typeSymbolKind :: Kind
typeNatKind    = TyConApp (promoteTyCon typeNatKindCon)    []
typeSymbolKind = TyConApp (promoteTyCon typeSymbolKindCon) []

{-
************************************************************************
*                                                                      *
                Stuff for dealing with tuples
*                                                                      *
************************************************************************

Note [How tuples work]  See also Note [Known-key names] in PrelNames
~~~~~~~~~~~~~~~~~~~~~~
* There are three families of tuple TyCons and corresponding
  DataCons, (boxed, unboxed, and constraint tuples), expressed by the
  type BasicTypes.TupleSort.

* DataCons (and workers etc) for BoxedTuple and ConstraintTuple have
    - distinct Uniques
    - the same OccName
  Using the same OccName means (hack!) that a single copy of the
  runtime library code (info tables etc) works for both.

* When looking up an OccName in the original-name cache
  (IfaceEnv.lookupOrigNameCache), we spot the tuple OccName to make sure
  we get the right wired-in name.  This guy can't tell the difference
  between BoxedTuple and ConstraintTuple (same OccName!), so tuples
  are not serialised into interface files using OccNames at all.

* Serialization to interface files works via the usual mechanism for known-key
  things: instead of serializing the OccName we just serialize the key. During
  deserialization we lookup the Name associated with the unique with the logic
  in KnownUniques. See Note [Symbol table representation of names] for details.

Note [One-tuples]
~~~~~~~~~~~~~~~~~
GHC supports both boxed and unboxed one-tuples:
 - Unboxed one-tuples are sometimes useful when returning a
   single value after CPR analysis
 - A boxed one-tuple is used by DsUtils.mkSelectorBinds, when
   there is just one binder
Basically it keeps everything uniform.

However the /naming/ of the type/data constructors for one-tuples is a
bit odd:
  3-tuples:  (,,)   (,,)#
  2-tuples:  (,)    (,)#
  1-tuples:  ??
  0-tuples:  ()     ()#

Zero-tuples have used up the logical name. So we use 'Unit' and 'Unit#'
for one-tuples.  So in ghc-prim:GHC.Tuple we see the declarations:
  data ()     = ()
  data Unit a = Unit a
  data (a,b)  = (a,b)

NB (Feb 16): for /constraint/ one-tuples I have 'Unit%' but no class
decl in GHC.Classes, so I think this part may not work properly. But
it's unused I think.

-}

isBuiltInOcc_maybe :: OccName -> Maybe Name
-- Built in syntax isn't "in scope" so these OccNames
-- map to wired-in Names with BuiltInSyntax
isBuiltInOcc_maybe occ
  = case occNameString occ of
        "[]"             -> choose_ns listTyCon nilDataCon
        ":"              -> Just consDataConName

        "[::]"           -> Just parrTyConName

        -- boxed tuple data/tycon
        "()"             -> choose_ns unitTyCon        unitDataCon
        '(':',':rest     -> parse_tuple BoxedTuple   2 rest

        "(##)"           -> choose_ns unboxedUnitTyCon unboxedUnitDataCon
        '(':'#':',':rest -> parse_tuple UnboxedTuple 2 rest

        _other           -> Nothing
  where
    ns = occNameSpace occ

    parse_tuple sort n rest
      | (',' : rest2) <- rest   = parse_tuple sort (n+1) rest2
      | tail_matches sort rest  = choose_ns (tupleTyCon sort n)
                                            (tupleCon   sort n)
      | otherwise               = Nothing

    tail_matches BoxedTuple   ")"  = True
    tail_matches UnboxedTuple "#)" = True
    tail_matches _            _    = False

    choose_ns tc dc
      | isTcClsNameSpace ns   = Just (getName tc)
      | isDataConNameSpace ns = Just (getName dc)
      | otherwise             = Just (getName (dataConWorkId dc))

mkTupleOcc :: NameSpace -> TupleSort -> Arity -> OccName
-- No need to cache these, the caching is done in mk_tuple
mkTupleOcc ns BoxedTuple   ar    = mkOccName ns (mkBoxedTupleStr      ar)
mkTupleOcc ns UnboxedTuple ar    = mkOccName ns (mkUnboxedTupleStr    ar)
mkTupleOcc ns ConstraintTuple ar = mkOccName ns (mkConstraintTupleStr ar)

-- mkCTupleOcc :: NameSpace -> Arity -> OccName
-- mkCTupleOcc ns ar = mkOccName ns (mkConstraintTupleStr ar)

mkBoxedTupleStr :: Arity -> String
mkBoxedTupleStr 0  = "()"
mkBoxedTupleStr 1  = "Unit"   -- See Note [One-tuples]
mkBoxedTupleStr ar = '(' : commas ar ++ ")"

mkUnboxedTupleStr :: Arity -> String
mkUnboxedTupleStr 0  = "(##)"
mkUnboxedTupleStr 1  = "Unit#"  -- See Note [One-tuples]
mkUnboxedTupleStr ar = "(#" ++ commas ar ++ "#)"

-- TODO: Refactor constraint tuples
mkConstraintTupleStr :: Arity -> String
mkConstraintTupleStr 0  = "()"
mkConstraintTupleStr 1  = "Unit"   -- See Note [One-tuples]
mkConstraintTupleStr ar = "(" ++ commas ar ++ ")"

commas :: Arity -> String
commas ar = take (ar-1) (repeat ',')

    -- Cute hack: we reuse the standard tuple OccNames (and hence code)
    -- for fact tuples, but give them different Uniques so they are not equal.
    --
    -- You might think that this will go wrong because isBuiltInOcc_maybe won't
    -- be able to tell the difference between boxed tuples and constraint tuples. BUT:
    --  1. Constraint tuples never occur directly in user code, so it doesn't matter
    --     that we can't detect them in Orig OccNames originating from the user
    --     programs (or those built by setRdrNameSpace used on an Exact tuple Name)
    --  2. Interface files have a special representation for tuple *occurrences*
    --     in IfaceTyCons, their workers (in IfaceSyn) and their DataCons (in case
    --     alternatives). Thus we don't rely on the OccName to figure out what kind
    --     of tuple an occurrence was trying to use in these situations.
    --  3. We *don't* represent tuple data type declarations specially, so those
    --     are still turned into wired-in names via isBuiltInOcc_maybe. But that's OK
    --     because we don't actually need to declare constraint tuples thanks to this hack.
    --
    -- So basically any OccName like (,,) flowing to isBuiltInOcc_maybe will always
    -- refer to the standard boxed tuple. Cool :-)


tupleTyCon :: TupleSort -> Arity -> TyCon
tupleTyCon sort i | i > mAX_TUPLE_SIZE = fst (mk_tuple sort i)  -- Build one specially
tupleTyCon BoxedTuple   i = fst (boxedTupleArr   ! i)
tupleTyCon UnboxedTuple i = fst (unboxedTupleArr ! i)
tupleTyCon ConstraintTuple    i = fst (factTupleArr    ! i)

promotedTupleTyCon :: TupleSort -> Arity -> TyCon
promotedTupleTyCon sort i = promoteTyCon (tupleTyCon sort i)

promotedTupleDataCon :: TupleSort -> Arity -> TyCon
promotedTupleDataCon sort i = promoteDataCon (tupleCon sort i)

tupleDataCon :: Boxity -> Arity -> DataCon
tupleDataCon sort i | i > mAX_TUPLE_SIZE = snd (mk_tuple (boxityNormalTupleSort sort) i)    -- Build one specially
tupleDataCon Boxed   i = snd (boxedTupleArr   ! i)
tupleDataCon Unboxed i = snd (unboxedTupleArr ! i)

tupleCon :: TupleSort -> Arity -> DataCon
tupleCon sort i | i > mAX_TUPLE_SIZE = snd (mk_tuple sort i)    -- Build one specially
tupleCon BoxedTuple   i = snd (boxedTupleArr   ! i)
tupleCon UnboxedTuple i = snd (unboxedTupleArr ! i)
tupleCon ConstraintTuple    i = snd (factTupleArr    ! i)

boxedTupleArr, unboxedTupleArr, factTupleArr :: Array Int (TyCon,DataCon)
boxedTupleArr   = listArray (0,mAX_TUPLE_SIZE) [mk_tuple BoxedTuple i | i <- [0..mAX_TUPLE_SIZE]]
unboxedTupleArr = listArray (0,mAX_TUPLE_SIZE) [mk_tuple UnboxedTuple i | i <- [0..mAX_TUPLE_SIZE]]
factTupleArr = listArray (0,mAX_TUPLE_SIZE) [mk_tuple ConstraintTuple i | i <- [0..mAX_TUPLE_SIZE]]

mk_tuple :: TupleSort -> Int -> (TyCon,DataCon)
mk_tuple sort arity = (tycon, tuple_con)
  where
        tycon   = mkTupleTyCon tc_name tc_kind arity tyvars tuple_con sort prom_tc
        prom_tc = case sort of
          BoxedTuple      -> Just (mkPromotedTyCon tycon (promoteKind tc_kind))
          UnboxedTuple    -> Nothing
          ConstraintTuple -> Nothing

        modu    = mkTupleModule sort
        tc_name = mkWiredInName modu (mkTupleOcc tcName sort arity) tc_uniq
                                (ATyCon tycon) BuiltInSyntax
        tc_kind = mkArrowKinds (map tyVarKind tyvars) res_kind
        res_kind = case sort of
          BoxedTuple      -> liftedTypeKind
          UnboxedTuple    -> unliftedTypeKind
          ConstraintTuple -> constraintKind

        tyvars = take arity $ case sort of
          BoxedTuple      -> alphaTyVars
          UnboxedTuple    -> openAlphaTyVars
          ConstraintTuple -> mkTemplateTyVars $ repeat constraintKind

        tuple_con = pcDataCon dc_name tyvars tyvar_tys tycon
        tyvar_tys = mkTyVarTys tyvars
        dc_name   = mkWiredInName modu (mkTupleOcc dataName sort arity) dc_uniq
                                  (AConLike (RealDataCon tuple_con)) BuiltInSyntax
        tc_uniq   = mkTupleTyConUnique   sort arity
        dc_uniq   = mkTupleDataConUnique sort arity

unitTyCon :: TyCon
unitTyCon     = tupleTyCon BoxedTuple 0
unitDataCon :: DataCon
unitDataCon   = head (tyConDataCons unitTyCon)
unitDataConId :: Id
unitDataConId = dataConWorkId unitDataCon

pairTyCon :: TyCon
pairTyCon = tupleTyCon BoxedTuple 2

unboxedUnitTyCon :: TyCon
unboxedUnitTyCon   = tupleTyCon UnboxedTuple 0
unboxedUnitDataCon :: DataCon
unboxedUnitDataCon = tupleCon   UnboxedTuple 0

unboxedSingletonTyCon :: TyCon
unboxedSingletonTyCon   = tupleTyCon UnboxedTuple 1
unboxedSingletonDataCon :: DataCon
unboxedSingletonDataCon = tupleCon   UnboxedTuple 1

unboxedPairTyCon :: TyCon
unboxedPairTyCon   = tupleTyCon UnboxedTuple 2
unboxedPairDataCon :: DataCon
unboxedPairDataCon = tupleCon   UnboxedTuple 2

{-
************************************************************************
*                                                                      *
\subsection[TysWiredIn-boxed-prim]{The ``boxed primitive'' types (@Char@, @Int@, etc)}
*                                                                      *
************************************************************************
-}

eqTyCon :: TyCon
eqTyCon = mkAlgTyCon eqTyConName
            (ForAllTy kv $ mkArrowKinds [k, k] constraintKind)
            [kv, a, b]
            [Nominal, Nominal, Nominal]
            Nothing
            []      -- No stupid theta
            (DataTyCon [eqBoxDataCon] False)
            NoParentTyCon
            NonRecursive
            False
            Nothing   -- No parent for constraint-kinded types
  where
    kv = kKiVar
    k = mkTyVarTy kv
    [a,b] = mkTemplateTyVars [k,k]

eqBoxDataCon :: DataCon
eqBoxDataCon = pcDataCon eqBoxDataConName args [TyConApp eqPrimTyCon (map mkTyVarTy args)] eqTyCon
  where
    kv = kKiVar
    k = mkTyVarTy kv
    [a,b] = mkTemplateTyVars [k,k]
    args = [kv, a, b]


coercibleTyCon :: TyCon
coercibleTyCon = mkClassTyCon
    coercibleTyConName kind tvs [Nominal, Representational, Representational]
    rhs coercibleClass NonRecursive
  where kind = (ForAllTy kv $ mkArrowKinds [k, k] constraintKind)
        kv = kKiVar
        k = mkTyVarTy kv
        [a,b] = mkTemplateTyVars [k,k]
        tvs = [kv, a, b]
        rhs = DataTyCon [coercibleDataCon] False

coercibleDataCon :: DataCon
coercibleDataCon = pcDataCon coercibleDataConName args [TyConApp eqReprPrimTyCon (map mkTyVarTy args)] coercibleTyCon
  where
    kv = kKiVar
    k = mkTyVarTy kv
    [a,b] = mkTemplateTyVars [k,k]
    args = [kv, a, b]

coercibleClass :: Class
coercibleClass = mkClass (tyConTyVars coercibleTyCon) [] [] [] [] [] (mkAnd []) coercibleTyCon


{-
Note [The Implicit Parameter class]

Implicit parameters `?x :: a` are desugared into dictionaries for the
class `IP "x" a`, which is defined (in GHC.Classes) as

  class IP (x :: Symbol) a | x -> a

This class is wired-in so that `error` and `undefined`, which have
wired-in types, can use the implicit-call-stack feature to provide
a call-stack alongside the error message.
-}

ipDataConName, ipTyConName, ipCoName :: Name
ipDataConName = mkWiredInDataConName UserSyntax gHC_CLASSES (fsLit "IP")
                  ipDataConKey ipDataCon
ipTyConName   = mkWiredInTyConName UserSyntax gHC_CLASSES (fsLit "IP")
                  ipTyConKey ipTyCon
ipCoName      = mkWiredInCoAxiomName BuiltInSyntax gHC_CLASSES (fsLit "NTCo:IP")
                  ipCoNameKey (toBranchedAxiom ipCoAxiom)

-- See Note [The Implicit Parameter class]
ipTyCon :: TyCon
ipTyCon = mkClassTyCon ipTyConName kind [ip,a] [] rhs ipClass NonRecursive
  where
    kind = mkArrowKinds [typeSymbolKind, liftedTypeKind] constraintKind
    [ip,a] = mkTemplateTyVars [typeSymbolKind, liftedTypeKind]
    rhs = NewTyCon ipDataCon (mkTyVarTy a) ([], mkTyVarTy a) ipCoAxiom

ipCoAxiom :: CoAxiom Unbranched
ipCoAxiom = mkNewTypeCo ipCoName ipTyCon [ip,a] [Nominal, Nominal] (mkTyVarTy a)
  where
    [ip,a] = mkTemplateTyVars [typeSymbolKind, liftedTypeKind]

ipDataCon :: DataCon
ipDataCon = pcDataCon ipDataConName [ip,a] ts ipTyCon
  where
    [ip,a] = mkTemplateTyVars [typeSymbolKind, liftedTypeKind]
    ts  = [mkTyVarTy a]

ipClass :: Class
ipClass = mkClass (tyConTyVars ipTyCon) [([ip], [a])] [] [] [] [] (mkAnd [])
            ipTyCon
  where
    [ip, a] = tyConTyVars ipTyCon

-- this is a fake version of the CallStack TyCon so we can refer to it
-- in MkCore.errorTy
callStackTyCon :: TyCon
callStackTyCon = pcNonRecDataTyCon callStackTyConName Nothing [] []

charTy :: Type
charTy = mkTyConTy charTyCon

charTyCon :: TyCon
charTyCon   = pcNonRecDataTyCon charTyConName
                                (Just (CType "" Nothing (fsLit "HsChar")))
                                [] [charDataCon]
charDataCon :: DataCon
charDataCon = pcDataCon charDataConName [] [charPrimTy] charTyCon

stringTy :: Type
stringTy = mkListTy charTy -- convenience only

intTy :: Type
intTy = mkTyConTy intTyCon

intTyCon :: TyCon
intTyCon = pcNonRecDataTyCon intTyConName
                             (Just (CType "" Nothing (fsLit "HsInt"))) []
                             [intDataCon]
intDataCon :: DataCon
intDataCon = pcDataCon intDataConName [] [intPrimTy] intTyCon

wordTy :: Type
wordTy = mkTyConTy wordTyCon

wordTyCon :: TyCon
wordTyCon = pcNonRecDataTyCon wordTyConName
                              (Just (CType "" Nothing (fsLit "HsWord"))) []
                              [wordDataCon]
wordDataCon :: DataCon
wordDataCon = pcDataCon wordDataConName [] [wordPrimTy] wordTyCon

word8Ty :: Type
word8Ty = mkTyConTy word8TyCon

word8TyCon :: TyCon
word8TyCon = pcNonRecDataTyCon word8TyConName
                     (Just (CType "" Nothing (fsLit "HsWord8"))) []
                     [word8DataCon]

word8DataCon :: DataCon
word8DataCon = pcDataCon word8DataConName [] [wordPrimTy] word8TyCon

floatTy :: Type
floatTy = mkTyConTy floatTyCon

floatTyCon :: TyCon
floatTyCon   = pcNonRecDataTyCon floatTyConName
                                 (Just (CType "" Nothing (fsLit "HsFloat"))) []
                                 [floatDataCon]
floatDataCon :: DataCon
floatDataCon = pcDataCon         floatDataConName [] [floatPrimTy] floatTyCon

doubleTy :: Type
doubleTy = mkTyConTy doubleTyCon

doubleTyCon :: TyCon
doubleTyCon = pcNonRecDataTyCon doubleTyConName
                                (Just (CType "" Nothing (fsLit "HsDouble"))) []
                                [doubleDataCon]

doubleDataCon :: DataCon
doubleDataCon = pcDataCon doubleDataConName [] [doublePrimTy] doubleTyCon

jstringTy :: Type
jstringTy = mkTyConTy jstringTyCon

jstringTyCon :: TyCon
jstringTyCon = pcNonRecDataTyCon jstringTyConName
                              (Just (CType "" Nothing (fsLit "java.lang.String"))) []
                              [jstringDataCon]
jstringDataCon :: DataCon
jstringDataCon = pcDataCon jstringDataConName [] [mkObjectPrimTy jstringTy] jstringTyCon

{-
************************************************************************
*                                                                      *
\subsection[TysWiredIn-Bool]{The @Bool@ type}
*                                                                      *
************************************************************************

An ordinary enumeration type, but deeply wired in.  There are no
magical operations on @Bool@ (just the regular Prelude code).

{\em BEGIN IDLE SPECULATION BY SIMON}

This is not the only way to encode @Bool@.  A more obvious coding makes
@Bool@ just a boxed up version of @Bool#@, like this:
\begin{verbatim}
type Bool# = Int#
data Bool = MkBool Bool#
\end{verbatim}

Unfortunately, this doesn't correspond to what the Report says @Bool@
looks like!  Furthermore, we get slightly less efficient code (I
think) with this coding. @gtInt@ would look like this:

\begin{verbatim}
gtInt :: Int -> Int -> Bool
gtInt x y = case x of I# x# ->
            case y of I# y# ->
            case (gtIntPrim x# y#) of
                b# -> MkBool b#
\end{verbatim}

Notice that the result of the @gtIntPrim@ comparison has to be turned
into an integer (here called @b#@), and returned in a @MkBool@ box.

The @if@ expression would compile to this:
\begin{verbatim}
case (gtInt x y) of
  MkBool b# -> case b# of { 1# -> e1; 0# -> e2 }
\end{verbatim}

I think this code is a little less efficient than the previous code,
but I'm not certain.  At all events, corresponding with the Report is
important.  The interesting thing is that the language is expressive
enough to describe more than one alternative; and that a type doesn't
necessarily need to be a straightforwardly boxed version of its
primitive counterpart.

{\em END IDLE SPECULATION BY SIMON}
-}

boolTy :: Type
boolTy = mkTyConTy boolTyCon

boolTyCon :: TyCon
boolTyCon = pcTyCon True NonRecursive True boolTyConName
                    (Just (CType "" Nothing (fsLit "HsBool")))
                    [] [falseDataCon, trueDataCon]

falseDataCon, trueDataCon :: DataCon
falseDataCon = pcDataCon falseDataConName [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConName  [] [] boolTyCon

falseDataConId, trueDataConId :: Id
falseDataConId = dataConWorkId falseDataCon
trueDataConId  = dataConWorkId trueDataCon

orderingTyCon :: TyCon
orderingTyCon = pcTyCon True NonRecursive True orderingTyConName Nothing
                        [] [ltDataCon, eqDataCon, gtDataCon]

ltDataCon, eqDataCon, gtDataCon :: DataCon
ltDataCon = pcDataCon ltDataConName  [] [] orderingTyCon
eqDataCon = pcDataCon eqDataConName  [] [] orderingTyCon
gtDataCon = pcDataCon gtDataConName  [] [] orderingTyCon

ltDataConId, eqDataConId, gtDataConId :: Id
ltDataConId = dataConWorkId ltDataCon
eqDataConId = dataConWorkId eqDataCon
gtDataConId = dataConWorkId gtDataCon

{-
************************************************************************
*                                                                      *
\subsection[TysWiredIn-List]{The @List@ type (incl ``build'' magic)}
*                                                                      *
************************************************************************

Special syntax, deeply wired in, but otherwise an ordinary algebraic
data types:
\begin{verbatim}
data [] a = [] | a : (List a)
data () = ()
data (,) a b = (,,) a b
...
\end{verbatim}
-}

mkListTy :: Type -> Type
mkListTy ty = mkTyConApp listTyCon [ty]

listTyCon :: TyCon
listTyCon = pcTyCon False Recursive True
                    listTyConName Nothing alpha_tyvar [nilDataCon, consDataCon]

mkPromotedListTy :: Type -> Type
mkPromotedListTy ty = mkTyConApp promotedListTyCon [ty]

promotedListTyCon :: TyCon
promotedListTyCon = promoteTyCon listTyCon

nilDataCon :: DataCon
nilDataCon  = pcDataCon nilDataConName alpha_tyvar [] listTyCon

consDataCon :: DataCon
consDataCon = pcDataConWithFixity True {- Declared infix -}
               consDataConName
               alpha_tyvar [alphaTy, mkTyConApp listTyCon alpha_ty] listTyCon
-- Interesting: polymorphic recursion would help here.
-- We can't use (mkListTy alphaTy) in the defn of consDataCon, else mkListTy
-- gets the over-specific type (Type -> Type)

-- Wired-in type Maybe

maybeTyCon :: TyCon
maybeTyCon = pcTyCon False NonRecursive True maybeTyConName Nothing alpha_tyvar
                     [nothingDataCon, justDataCon]

nothingDataCon :: DataCon
nothingDataCon = pcDataCon nothingDataConName alpha_tyvar [] maybeTyCon

justDataCon :: DataCon
justDataCon = pcDataCon justDataConName alpha_tyvar [alphaTy] maybeTyCon

-- hasCallStackTyCon :: TyCon
-- hasCallStackTyCon = pcTyCon False NonRecursive True maybeTyConName Nothing alpha_tyvar
--                      [nothingDataCon, justDataCon]


{-
************************************************************************
*                                                                      *
\subsection[TysWiredIn-Tuples]{The @Tuple@ types}
*                                                                      *
************************************************************************

The tuple types are definitely magic, because they form an infinite
family.

\begin{itemize}
\item
They have a special family of type constructors, of type @TyCon@
These contain the tycon arity, but don't require a Unique.

\item
They have a special family of constructors, of type
@Id@. Again these contain their arity but don't need a Unique.

\item
There should be a magic way of generating the info tables and
entry code for all tuples.

But at the moment we just compile a Haskell source
file\srcloc{lib/prelude/...} containing declarations like:
\begin{verbatim}
data Tuple0             = Tup0
data Tuple2  a b        = Tup2  a b
data Tuple3  a b c      = Tup3  a b c
data Tuple4  a b c d    = Tup4  a b c d
...
\end{verbatim}
The print-names associated with the magic @Id@s for tuple constructors
``just happen'' to be the same as those generated by these
declarations.

\item
The instance environment should have a magic way to know
that each tuple type is an instances of classes @Eq@, @Ix@, @Ord@ and
so on. \ToDo{Not implemented yet.}

\item
There should also be a way to generate the appropriate code for each
of these instances, but (like the info tables and entry code) it is
done by enumeration\srcloc{lib/prelude/InTup?.hs}.
\end{itemize}
-}

mkTupleTy :: TupleSort -> [Type] -> Type
-- Special case for *boxed* 1-tuples, which are represented by the type itself
mkTupleTy sort [ty] | Boxed <- tupleSortBoxity sort = ty
mkTupleTy sort tys = mkTyConApp (tupleTyCon sort (length tys)) tys

-- | Build the type of a small tuple that holds the specified type of thing
mkBoxedTupleTy :: [Type] -> Type
mkBoxedTupleTy tys = mkTupleTy BoxedTuple tys

unitTy :: Type
unitTy = mkTupleTy BoxedTuple []

{-
************************************************************************
*                                                                      *
\subsection[TysWiredIn-PArr]{The @[::]@ type}
*                                                                      *
************************************************************************

Special syntax for parallel arrays needs some wired in definitions.
-}

-- | Construct a type representing the application of the parallel array constructor
mkPArrTy    :: Type -> Type
mkPArrTy ty  = mkTyConApp parrTyCon [ty]

-- | Represents the type constructor of parallel arrays
--
--  * This must match the definition in @PrelPArr@
--
-- NB: Although the constructor is given here, it will not be accessible in
--     user code as it is not in the environment of any compiled module except
--     @PrelPArr@.
--
parrTyCon :: TyCon
parrTyCon  = pcNonRecDataTyCon parrTyConName Nothing alpha_tyvar [parrDataCon]

parrDataCon :: DataCon
parrDataCon  = pcDataCon
                 parrDataConName
                 alpha_tyvar            -- forall'ed type variables
                 [intTy,                -- 1st argument: Int
                  mkTyConApp            -- 2nd argument: Array# a
                    arrayPrimTyCon
                    alpha_ty]
                 parrTyCon

-- | Check whether a type constructor is the constructor for parallel arrays
isPArrTyCon    :: TyCon -> Bool
isPArrTyCon tc  = tyConName tc == parrTyConName

-- | Fake array constructors
--
-- * These constructors are never really used to represent array values;
--   however, they are very convenient during desugaring (and, in particular,
--   in the pattern matching compiler) to treat array pattern just like
--   yet another constructor pattern
--
parrFakeCon                        :: Arity -> DataCon
parrFakeCon i | i > mAX_TUPLE_SIZE  = mkPArrFakeCon  i  -- build one specially
parrFakeCon i                       = parrFakeConArr!i

-- pre-defined set of constructors
--
parrFakeConArr :: Array Int DataCon
parrFakeConArr  = array (0, mAX_TUPLE_SIZE) [(i, mkPArrFakeCon i)
                                            | i <- [0..mAX_TUPLE_SIZE]]

-- build a fake parallel array constructor for the given arity
--
mkPArrFakeCon       :: Int -> DataCon
mkPArrFakeCon arity  = data_con
  where
        data_con  = pcDataCon name [tyvar] tyvarTys parrTyCon
        tyvar     = head alphaTyVars
        tyvarTys  = replicate arity $ mkTyVarTy tyvar
        nameStr   = mkFastString ("MkPArr" ++ show arity)
        name      = mkWiredInName gHC_PARR' (mkDataOccFS nameStr) unique
                                  (AConLike (RealDataCon data_con)) UserSyntax
        unique      = mkPArrDataConUnique arity

-- | Checks whether a data constructor is a fake constructor for parallel arrays
isPArrFakeCon      :: DataCon -> Bool
isPArrFakeCon dcon  = dcon == parrFakeCon (dataConSourceArity dcon)

-- Promoted Booleans

promotedBoolTyCon, promotedFalseDataCon, promotedTrueDataCon :: TyCon
promotedBoolTyCon     = promoteTyCon boolTyCon
promotedTrueDataCon   = promoteDataCon trueDataCon
promotedFalseDataCon  = promoteDataCon falseDataCon

-- Promoted Maybe
promotedNothingDataCon, promotedJustDataCon :: TyCon
promotedNothingDataCon = promoteDataCon nothingDataCon
promotedJustDataCon    = promoteDataCon justDataCon

-- Promoted Ordering

promotedOrderingTyCon
  , promotedLTDataCon
  , promotedEQDataCon
  , promotedGTDataCon
  :: TyCon
promotedOrderingTyCon = promoteTyCon orderingTyCon
promotedLTDataCon     = promoteDataCon ltDataCon
promotedEQDataCon     = promoteDataCon eqDataCon
promotedGTDataCon     = promoteDataCon gtDataCon
