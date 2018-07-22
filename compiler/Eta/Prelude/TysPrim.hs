{-
(c) Rahul Muttineni, 2016-2017
(c) The AQUA Project, Glasgow University, 1994-1998


\section[TysPrim]{Wired-in knowledge about primitive types}
-}

{-# LANGUAGE CPP, OverloadedStrings #-}

-- | This module defines TyCons that can't be expressed in Haskell.
--   They are all, therefore, wired-in TyCons.  C.f module TysWiredIn
module Eta.Prelude.TysPrim(
        mkTemplateTyVars, alphaTyVars, betaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,
        alphaTy, betaTy, gammaTy, deltaTy,
        openAlphaTy, openBetaTy, openAlphaTyVar, openBetaTyVar, openAlphaTyVars,
        kKiVar,

        -- Kind constructors...
        superKindTyCon, superKind, anyKindTyCon, liftedTypeKindTyCon,
        openTypeKindTyCon, unliftedTypeKindTyCon, constraintKindTyCon,

        superKindTyConName, anyKindTyConName, liftedTypeKindTyConName,
        openTypeKindTyConName, unliftedTypeKindTyConName,
        constraintKindTyConName,

        -- Kinds
        anyKind, liftedTypeKind, unliftedTypeKind, openTypeKind, constraintKind,
        mkArrowKind, mkArrowKinds,

        funTyCon, funTyConName,
        primTyCons,

        charPrimTyCon,          charPrimTy,
        intPrimTyCon,           intPrimTy,
        wordPrimTyCon,          wordPrimTy,
        addrPrimTyCon,          addrPrimTy,
        floatPrimTyCon,         floatPrimTy,
        doublePrimTyCon,        doublePrimTy,

        voidPrimTyCon,          voidPrimTy,
        statePrimTyCon,         mkStatePrimTy,
        realWorldTyCon,         realWorldTy, realWorldStatePrimTy,

        proxyPrimTyCon,         mkProxyPrimTy,

        arrayPrimTyCon, mkArrayPrimTy,
        byteArrayPrimTyCon,     byteArrayPrimTy,
        arrayArrayPrimTyCon, mkArrayArrayPrimTy,
        smallArrayPrimTyCon, mkSmallArrayPrimTy,
        mutableArrayPrimTyCon, mkMutableArrayPrimTy,
        mutableByteArrayPrimTyCon, mkMutableByteArrayPrimTy,
        mutableArrayArrayPrimTyCon, mkMutableArrayArrayPrimTy,
        smallMutableArrayPrimTyCon, mkSmallMutableArrayPrimTy,
        mutVarPrimTyCon, mkMutVarPrimTy,

        mVarPrimTyCon,                  mkMVarPrimTy,
        tVarPrimTyCon,                  mkTVarPrimTy,
        stablePtrPrimTyCon,             mkStablePtrPrimTy,
        stableNamePrimTyCon,            mkStableNamePrimTy,
        bcoPrimTyCon,                   bcoPrimTy,
        weakPrimTyCon,                  mkWeakPrimTy,
        threadIdPrimTyCon,              threadIdPrimTy,

        int32PrimTyCon,         int32PrimTy,
        word32PrimTyCon,        word32PrimTy,

        int64PrimTyCon,         int64PrimTy,
        word64PrimTyCon,        word64PrimTy,

        eqPrimTyCon,            -- ty1 ~# ty2
        eqReprPrimTyCon,        -- ty1 ~R# ty2  (at role Representational)

        -- * Any
        anyTy, anyTyCon, anyTypeOfKind,
        -- * ETA
        jcharPrimTy,
        jboolPrimTy,
        jbytePrimTy,
        jshortPrimTy,
        jcharPrimTyCon,
        jboolPrimTyCon,
        jbytePrimTyCon,
        jshortPrimTyCon,
        jobjectPrimTyCon, jobjectPrimTyConName,
        mkObjectPrimTy,

        -- * SIMD
        -- TODO: Currently vector operations are disabled in ETA
        -- int8X16PrimTy, int8X16PrimTyCon,
        -- int16X8PrimTy, int16X8PrimTyCon,
        -- int32X4PrimTy, int32X4PrimTyCon,
        -- int64X2PrimTy, int64X2PrimTyCon,
        -- int8X32PrimTy, int8X32PrimTyCon,
        -- int16X16PrimTy, int16X16PrimTyCon,
        -- int32X8PrimTy, int32X8PrimTyCon,
        -- int64X4PrimTy, int64X4PrimTyCon,
        -- int8X64PrimTy, int8X64PrimTyCon,
        -- int16X32PrimTy, int16X32PrimTyCon,
        -- int32X16PrimTy, int32X16PrimTyCon,
        -- int64X8PrimTy, int64X8PrimTyCon,
        -- word8X16PrimTy, word8X16PrimTyCon,
        -- word16X8PrimTy, word16X8PrimTyCon,
        -- word32X4PrimTy, word32X4PrimTyCon,
        -- word64X2PrimTy, word64X2PrimTyCon,
        -- word8X32PrimTy, word8X32PrimTyCon,
        -- word16X16PrimTy, word16X16PrimTyCon,
        -- word32X8PrimTy, word32X8PrimTyCon,
        -- word64X4PrimTy, word64X4PrimTyCon,
        -- word8X64PrimTy, word8X64PrimTyCon,
        -- word16X32PrimTy, word16X32PrimTyCon,
        -- word32X16PrimTy, word32X16PrimTyCon,
        -- word64X8PrimTy, word64X8PrimTyCon,
        -- floatX4PrimTy, floatX4PrimTyCon,
        -- doubleX2PrimTy, doubleX2PrimTyCon,
        -- floatX8PrimTy, floatX8PrimTyCon,
        -- doubleX4PrimTy, doubleX4PrimTyCon,
        -- floatX16PrimTy, floatX16PrimTyCon,
        -- doubleX8PrimTy, doubleX8PrimTyCon,
  ) where

#include "HsVersions.h"

import Eta.BasicTypes.Var              ( TyVar, KindVar, mkTyVar )
import Eta.BasicTypes.Name             ( Name, BuiltInSyntax(..), mkInternalName, mkWiredInName )
import Eta.BasicTypes.OccName          ( mkTyVarOccFS, mkTcOccFS )
import Eta.Types.TyCon
import Eta.Types.TypeRep
import Eta.BasicTypes.SrcLoc
import Eta.BasicTypes.Unique           ( mkAlphaTyVarUnique )
import Eta.Prelude.PrelNames
import Eta.Utils.FastString

import Eta.CodeGen.Rts

import Data.Char

{-
************************************************************************
*                                                                      *
\subsection{Primitive type constructors}
*                                                                      *
************************************************************************
-}

primTyCons :: [TyCon]
primTyCons
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , arrayArrayPrimTyCon
    , smallArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , int32PrimTyCon
    , int64PrimTyCon
    , bcoPrimTyCon
    , weakPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , mutableArrayArrayPrimTyCon
    , smallMutableArrayPrimTyCon
    , mVarPrimTyCon
    , tVarPrimTyCon
    , mutVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , stableNamePrimTyCon
    , statePrimTyCon
    , voidPrimTyCon
    , proxyPrimTyCon
    , threadIdPrimTyCon
    , wordPrimTyCon
    , word32PrimTyCon
    , word64PrimTyCon
    , anyTyCon
    , eqPrimTyCon
    , eqReprPrimTyCon

    , liftedTypeKindTyCon
    , unliftedTypeKindTyCon
    , openTypeKindTyCon
    , constraintKindTyCon
    , superKindTyCon
    , anyKindTyCon

    -- ETA-specific TyCons start here
    , jcharPrimTyCon
    , jbytePrimTyCon
    , jboolPrimTyCon
    , jshortPrimTyCon
    , jobjectPrimTyCon
    -- TODO: Vector operations disabled in ETA
    -- , int8X16PrimTyCon
    -- , int16X8PrimTyCon
    -- , int32X4PrimTyCon
    -- , int64X2PrimTyCon
    -- , int8X32PrimTyCon
    -- , int16X16PrimTyCon
    -- , int32X8PrimTyCon
    -- , int64X4PrimTyCon
    -- , int8X64PrimTyCon
    -- , int16X32PrimTyCon
    -- , int32X16PrimTyCon
    -- , int64X8PrimTyCon
    -- , word8X16PrimTyCon
    -- , word16X8PrimTyCon
    -- , word32X4PrimTyCon
    -- , word64X2PrimTyCon
    -- , word8X32PrimTyCon
    -- , word16X16PrimTyCon
    -- , word32X8PrimTyCon
    -- , word64X4PrimTyCon
    -- , word8X64PrimTyCon
    -- , word16X32PrimTyCon
    -- , word32X16PrimTyCon
    -- , word64X8PrimTyCon
    -- , floatX4PrimTyCon
    -- , doubleX2PrimTyCon
    -- , floatX8PrimTyCon
    -- , doubleX4PrimTyCon
    -- , floatX16PrimTyCon
    -- , doubleX8PrimTyCon
    ]

mkPrimTc :: FastString -> Unique -> TyCon -> Name
mkPrimTc fs unique tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS fs)
                  unique
                  (ATyCon tycon)        -- Relevant TyCon
                  UserSyntax

mkBuiltInPrimTc :: FastString -> Unique -> TyCon -> Name
mkBuiltInPrimTc fs unique tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS fs)
                  unique
                  (ATyCon tycon)        -- Relevant TyCon
                  BuiltInSyntax


charPrimTyConName, intPrimTyConName, int32PrimTyConName, int64PrimTyConName, wordPrimTyConName,
  word32PrimTyConName, word64PrimTyConName, addrPrimTyConName, floatPrimTyConName,
   doublePrimTyConName, statePrimTyConName, proxyPrimTyConName, realWorldTyConName,
   arrayPrimTyConName, arrayArrayPrimTyConName, smallArrayPrimTyConName, byteArrayPrimTyConName,
   mutableArrayPrimTyConName, mutableByteArrayPrimTyConName, mutableArrayArrayPrimTyConName,
   smallMutableArrayPrimTyConName, mutVarPrimTyConName, mVarPrimTyConName, tVarPrimTyConName,
   stablePtrPrimTyConName, stableNamePrimTyConName, bcoPrimTyConName, weakPrimTyConName,
   threadIdPrimTyConName, eqPrimTyConName, eqReprPrimTyConName, voidPrimTyConName,
   jcharPrimTyConName, jboolPrimTyConName, jbytePrimTyConName, jshortPrimTyConName, jobjectPrimTyConName :: Name
charPrimTyConName             = mkPrimTc (fsLit "Char#") charPrimTyConKey charPrimTyCon
intPrimTyConName              = mkPrimTc (fsLit "Int#") intPrimTyConKey  intPrimTyCon
int32PrimTyConName            = mkPrimTc (fsLit "Int32#") int32PrimTyConKey int32PrimTyCon
int64PrimTyConName            = mkPrimTc (fsLit "Int64#") int64PrimTyConKey int64PrimTyCon
wordPrimTyConName             = mkPrimTc (fsLit "Word#") wordPrimTyConKey wordPrimTyCon
word32PrimTyConName           = mkPrimTc (fsLit "Word32#") word32PrimTyConKey word32PrimTyCon
word64PrimTyConName           = mkPrimTc (fsLit "Word64#") word64PrimTyConKey word64PrimTyCon
addrPrimTyConName             = mkPrimTc (fsLit "Addr#") addrPrimTyConKey addrPrimTyCon
floatPrimTyConName            = mkPrimTc (fsLit "Float#") floatPrimTyConKey floatPrimTyCon
doublePrimTyConName           = mkPrimTc (fsLit "Double#") doublePrimTyConKey doublePrimTyCon
statePrimTyConName            = mkPrimTc (fsLit "State#") statePrimTyConKey statePrimTyCon
voidPrimTyConName             = mkPrimTc (fsLit "Void#") voidPrimTyConKey voidPrimTyCon
proxyPrimTyConName            = mkPrimTc (fsLit "Proxy#") proxyPrimTyConKey proxyPrimTyCon
eqPrimTyConName               = mkPrimTc (fsLit "~#") eqPrimTyConKey eqPrimTyCon
eqReprPrimTyConName           = mkBuiltInPrimTc (fsLit "~R#") eqReprPrimTyConKey eqReprPrimTyCon
realWorldTyConName            = mkPrimTc (fsLit "RealWorld") realWorldTyConKey realWorldTyCon
arrayPrimTyConName            = mkPrimTc (fsLit "Array#") arrayPrimTyConKey arrayPrimTyCon
byteArrayPrimTyConName        = mkPrimTc (fsLit "ByteArray#") byteArrayPrimTyConKey byteArrayPrimTyCon
arrayArrayPrimTyConName           = mkPrimTc (fsLit "ArrayArray#") arrayArrayPrimTyConKey arrayArrayPrimTyCon
smallArrayPrimTyConName       = mkPrimTc (fsLit "SmallArray#") smallArrayPrimTyConKey smallArrayPrimTyCon
mutableArrayPrimTyConName     = mkPrimTc (fsLit "MutableArray#") mutableArrayPrimTyConKey mutableArrayPrimTyCon
mutableByteArrayPrimTyConName = mkPrimTc (fsLit "MutableByteArray#") mutableByteArrayPrimTyConKey mutableByteArrayPrimTyCon
mutableArrayArrayPrimTyConName= mkPrimTc (fsLit "MutableArrayArray#") mutableArrayArrayPrimTyConKey mutableArrayArrayPrimTyCon
smallMutableArrayPrimTyConName= mkPrimTc (fsLit "SmallMutableArray#") smallMutableArrayPrimTyConKey smallMutableArrayPrimTyCon
mutVarPrimTyConName           = mkPrimTc (fsLit "MutVar#") mutVarPrimTyConKey mutVarPrimTyCon
mVarPrimTyConName             = mkPrimTc (fsLit "MVar#") mVarPrimTyConKey mVarPrimTyCon
tVarPrimTyConName             = mkPrimTc (fsLit "TVar#") tVarPrimTyConKey tVarPrimTyCon
stablePtrPrimTyConName        = mkPrimTc (fsLit "StablePtr#") stablePtrPrimTyConKey stablePtrPrimTyCon
stableNamePrimTyConName       = mkPrimTc (fsLit "StableName#") stableNamePrimTyConKey stableNamePrimTyCon
bcoPrimTyConName              = mkPrimTc (fsLit "BCO#") bcoPrimTyConKey bcoPrimTyCon
weakPrimTyConName             = mkPrimTc (fsLit "Weak#") weakPrimTyConKey weakPrimTyCon
threadIdPrimTyConName         = mkPrimTc (fsLit "ThreadId#") threadIdPrimTyConKey threadIdPrimTyCon
-- ETA-specific names
jcharPrimTyConName             = mkPrimTc (fsLit "JChar#") jcharPrimTyConKey jcharPrimTyCon
jboolPrimTyConName             = mkPrimTc (fsLit "JBool#") jboolPrimTyConKey jboolPrimTyCon
jbytePrimTyConName             = mkPrimTc (fsLit "JByte#") jbytePrimTyConKey jbytePrimTyCon
jshortPrimTyConName             = mkPrimTc (fsLit "JShort#") jshortPrimTyConKey jshortPrimTyCon
jobjectPrimTyConName = mkPrimTc (fsLit "Object#") jobjectPrimTyConKey jobjectPrimTyCon
{-
************************************************************************
*                                                                      *
\subsection{Support code}
*                                                                      *
************************************************************************

alphaTyVars is a list of type variables for use in templates:
        ["a", "b", ..., "z", "t1", "t2", ... ]
-}

mkTemplateTyVars :: [Kind] -> [TyVar]
mkTemplateTyVars kinds =
  [ mkTyVar (mkInternalName (mkAlphaTyVarUnique u)
                            (mkTyVarOccFS (mkFastString name))
                            noSrcSpan) k
  | (k,u) <- zip kinds [2..],
    let name | c <= 'z'  = [c]
             | otherwise = 't':show u
          where c = chr (u-2 + ord 'a')
  ]

alphaTyVars :: [TyVar]
alphaTyVars = mkTemplateTyVars $ repeat liftedTypeKind

betaTyVars :: [TyVar]
betaTyVars = tail alphaTyVars

alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

alphaTys :: [Type]
alphaTys = mkTyVarTys alphaTyVars
alphaTy, betaTy, gammaTy, deltaTy :: Type
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys

        -- openAlphaTyVar is prepared to be instantiated
        -- to a lifted or unlifted type variable.  It's used for the
        -- result type for "error", so that we can have (error Int# "Help")
openAlphaTyVars :: [TyVar]
openAlphaTyVar, openBetaTyVar :: TyVar
openAlphaTyVars@(openAlphaTyVar:openBetaTyVar:_)
  = mkTemplateTyVars $ repeat openTypeKind

openAlphaTy, openBetaTy :: Type
openAlphaTy = mkTyVarTy openAlphaTyVar
openBetaTy  = mkTyVarTy openBetaTyVar

kKiVar :: KindVar
kKiVar = (mkTemplateTyVars $ repeat superKind) !! 10

{-
************************************************************************
*                                                                      *
                FunTyCon
*                                                                      *
************************************************************************
-}

funTyConName :: Name
funTyConName = mkPrimTyConName (fsLit "(->)") funTyConKey funTyCon

funTyCon :: TyCon
funTyCon = mkFunTyCon funTyConName $
           mkArrowKinds [liftedTypeKind, liftedTypeKind] liftedTypeKind
        -- You might think that (->) should have type (?? -> ? -> *), and you'd be right
        -- But if we do that we get kind errors when saying
        --      instance Control.Arrow (->)
        -- because the expected kind is (*->*->*).  The trouble is that the
        -- expected/actual stuff in the unifier does not go contra-variant, whereas
        -- the kind sub-typing does.  Sigh.  It really only matters if you use (->) in
        -- a prefix way, thus:  (->) Int# Int#.  And this is unusual.
        -- because they are never in scope in the source

-- One step to remove subkinding.
-- (->) :: * -> * -> *
-- but we should have (and want) the following typing rule for fully applied arrows
--      Gamma |- tau   :: k1    k1 in {*, #}
--      Gamma |- sigma :: k2    k2 in {*, #, (#)}
--      -----------------------------------------
--      Gamma |- tau -> sigma :: *
-- Currently we have the following rule which achieves more or less the same effect
--      Gamma |- tau   :: ??
--      Gamma |- sigma :: ?
--      --------------------------
--      Gamma |- tau -> sigma :: *
-- In the end we don't want subkinding at all.

{-
************************************************************************
*                                                                      *
                Kinds
*                                                                      *
************************************************************************

Note [SuperKind (BOX)]
~~~~~~~~~~~~~~~~~~~~~~
Kinds are classified by "super-kinds".  There is only one super-kind, namely BOX.

Perhaps surprisingly we give BOX the kind BOX, thus   BOX :: BOX
Reason: we want to have kind equalities, thus (without the kind applications)
            keq :: * ~ * = Eq# <refl *>
Remember that
   (~)  :: forall (k:BOX). k -> k -> Constraint
   (~#) :: forall (k:BOX). k -> k -> #
   Eq#  :: forall (k:BOX). forall (a:k) (b:k). (~#) k a b -> (~) k a b

So the full defn of keq is
   keq :: (~) BOX * * = Eq# BOX * * <refl *>

So you can see it's convenient to have BOX:BOX
-}

-- | See "Type#kind_subtyping" for details of the distinction between the 'Kind' 'TyCon's
superKindTyCon, anyKindTyCon, liftedTypeKindTyCon,
      openTypeKindTyCon, unliftedTypeKindTyCon,
      constraintKindTyCon
   :: TyCon
superKindTyConName, anyKindTyConName, liftedTypeKindTyConName,
      openTypeKindTyConName, unliftedTypeKindTyConName,
      constraintKindTyConName
   :: Name

superKindTyCon        = mkKindTyCon superKindTyConName        superKind
   -- See Note [SuperKind (BOX)]

anyKindTyCon          = mkKindTyCon anyKindTyConName          superKind
liftedTypeKindTyCon   = mkKindTyCon liftedTypeKindTyConName   superKind
openTypeKindTyCon     = mkKindTyCon openTypeKindTyConName     superKind
unliftedTypeKindTyCon = mkKindTyCon unliftedTypeKindTyConName superKind
constraintKindTyCon   = mkKindTyCon constraintKindTyConName   superKind

--------------------------
-- ... and now their names

-- If you edit these, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
superKindTyConName        = mkPrimTyConName (fsLit "BOX")        superKindTyConKey        superKindTyCon
anyKindTyConName          = mkPrimTyConName (fsLit "AnyK")       anyKindTyConKey          anyKindTyCon
liftedTypeKindTyConName   = mkPrimTyConName (fsLit "*")          liftedTypeKindTyConKey   liftedTypeKindTyCon
openTypeKindTyConName     = mkPrimTyConName (fsLit "OpenKind")   openTypeKindTyConKey     openTypeKindTyCon
unliftedTypeKindTyConName = mkPrimTyConName (fsLit "#")          unliftedTypeKindTyConKey unliftedTypeKindTyCon

mkPrimTyConName :: FastString -> Unique -> TyCon -> Name
mkPrimTyConName = mkPrimTcName BuiltInSyntax
  -- All of the super kinds and kinds are defined in Prim,
  -- and use BuiltInSyntax, because they are never in scope in the source

constraintKindTyConName -- Unlike the others, Constraint does *not* use BuiltInSyntax,
                        -- and can be imported/exported like any other type constructor
  = mkPrimTcName UserSyntax (fsLit "Constraint") constraintKindTyConKey   constraintKindTyCon


mkPrimTcName :: BuiltInSyntax -> FastString -> Unique -> TyCon -> Name
mkPrimTcName built_in_syntax occ key tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS occ) key (ATyCon tycon) built_in_syntax

kindTyConType :: TyCon -> Type
kindTyConType kind = TyConApp kind []   -- mkTyConApp isn't defined yet

-- | See "Type#kind_subtyping" for details of the distinction between these 'Kind's
anyKind, liftedTypeKind, unliftedTypeKind, openTypeKind, constraintKind, superKind :: Kind

superKind        = kindTyConType superKindTyCon
anyKind          = kindTyConType anyKindTyCon  -- See Note [Any kinds]
liftedTypeKind   = kindTyConType liftedTypeKindTyCon
unliftedTypeKind = kindTyConType unliftedTypeKindTyCon
openTypeKind     = kindTyConType openTypeKindTyCon
constraintKind   = kindTyConType constraintKindTyCon

-- | Given two kinds @k1@ and @k2@, creates the 'Kind' @k1 -> k2@
mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = FunTy k1 k2

-- | Iterated application of 'mkArrowKind'
mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-basic]{Basic primitive types (@Char#@, @Int#@, etc.)}
*                                                                      *
************************************************************************
-}

-- only used herein
pcPrimTyCon :: Name -> [Role] -> PrimRep -> TyCon
pcPrimTyCon name roles rep
  = mkPrimTyCon name kind roles rep
  where
    kind        = mkArrowKinds (map (const liftedTypeKind) roles) result_kind
    result_kind = unliftedTypeKind

pcPrimTyCon0 :: Name -> PrimRep -> TyCon
pcPrimTyCon0 name rep
  = mkPrimTyCon name result_kind [] rep
  where
    result_kind = unliftedTypeKind

charPrimTy :: Type
charPrimTy      = mkTyConTy charPrimTyCon
charPrimTyCon :: TyCon
charPrimTyCon   = pcPrimTyCon0 charPrimTyConName WordRep

intPrimTy :: Type
intPrimTy       = mkTyConTy intPrimTyCon
intPrimTyCon :: TyCon
intPrimTyCon    = pcPrimTyCon0 intPrimTyConName IntRep

int32PrimTy :: Type
int32PrimTy     = mkTyConTy int32PrimTyCon
int32PrimTyCon :: TyCon
int32PrimTyCon  = pcPrimTyCon0 int32PrimTyConName IntRep

int64PrimTy :: Type
int64PrimTy     = mkTyConTy int64PrimTyCon
int64PrimTyCon :: TyCon
int64PrimTyCon  = pcPrimTyCon0 int64PrimTyConName Int64Rep

wordPrimTy :: Type
wordPrimTy      = mkTyConTy wordPrimTyCon
wordPrimTyCon :: TyCon
wordPrimTyCon   = pcPrimTyCon0 wordPrimTyConName WordRep

word32PrimTy :: Type
word32PrimTy    = mkTyConTy word32PrimTyCon
word32PrimTyCon :: TyCon
word32PrimTyCon = pcPrimTyCon0 word32PrimTyConName WordRep

word64PrimTy :: Type
word64PrimTy    = mkTyConTy word64PrimTyCon
word64PrimTyCon :: TyCon
word64PrimTyCon = pcPrimTyCon0 word64PrimTyConName Word64Rep

addrPrimTy :: Type
addrPrimTy      = mkTyConTy addrPrimTyCon
addrPrimTyCon :: TyCon
addrPrimTyCon   = pcPrimTyCon0 addrPrimTyConName AddrRep

floatPrimTy     :: Type
floatPrimTy     = mkTyConTy floatPrimTyCon
floatPrimTyCon :: TyCon
floatPrimTyCon  = pcPrimTyCon0 floatPrimTyConName FloatRep

doublePrimTy :: Type
doublePrimTy    = mkTyConTy doublePrimTyCon
doublePrimTyCon :: TyCon
doublePrimTyCon = pcPrimTyCon0 doublePrimTyConName DoubleRep

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-state]{The @State#@ type (and @_RealWorld@ types)}
*                                                                      *
************************************************************************

Note [The ~# TyCon)
~~~~~~~~~~~~~~~~~~~~
There is a perfectly ordinary type constructor ~# that represents the type
of coercions (which, remember, are values).  For example
   Refl Int :: ~# * Int Int

It is a kind-polymorphic type constructor like Any:
   Refl Maybe :: ~# (* -> *) Maybe Maybe

(~) only appears saturated. So we check that in CoreLint (and, in an
assertion, in Kind.typeKind).

Note [The State# TyCon]
~~~~~~~~~~~~~~~~~~~~~~~
State# is the primitive, unlifted type of states.  It has one type parameter,
thus
        State# RealWorld
or
        State# s

where s is a type variable. The only purpose of the type parameter is to
keep different state threads separate.  It is represented by nothing at all.

The type parameter to State# is intended to keep separate threads separate.
Even though this parameter is not used in the definition of State#, it is
given role Nominal to enforce its intended use.
-}

mkStatePrimTy :: Type -> Type
mkStatePrimTy ty = TyConApp statePrimTyCon [ty]

statePrimTyCon :: TyCon   -- See Note [The State# TyCon]
statePrimTyCon   = pcPrimTyCon statePrimTyConName [Nominal] VoidRep

voidPrimTy :: Type
voidPrimTy = TyConApp voidPrimTyCon []

voidPrimTyCon :: TyCon
voidPrimTyCon    = pcPrimTyCon voidPrimTyConName [] VoidRep

mkProxyPrimTy :: Type -> Type -> Type
mkProxyPrimTy k ty = TyConApp proxyPrimTyCon [k, ty]

proxyPrimTyCon :: TyCon
proxyPrimTyCon = mkPrimTyCon proxyPrimTyConName kind [Nominal,Nominal] VoidRep
  where kind = ForAllTy kv $ mkArrowKind k unliftedTypeKind
        kv   = kKiVar
        k    = mkTyVarTy kv

eqPrimTyCon :: TyCon  -- The representation type for equality predicates
                      -- See Note [The ~# TyCon]
eqPrimTyCon  = mkPrimTyCon eqPrimTyConName kind [Nominal, Nominal, Nominal] VoidRep
  where kind = ForAllTy kv $ mkArrowKinds [k, k] unliftedTypeKind
        kv = kKiVar
        k = mkTyVarTy kv

-- like eqPrimTyCon, but the type for *Representational* coercions
-- this should only ever appear as the type of a covar. Its role is
-- interpreted in coercionRole
eqReprPrimTyCon :: TyCon
eqReprPrimTyCon = mkPrimTyCon eqReprPrimTyConName kind
                                  -- the roles really should be irrelevant!
                              [Nominal, Representational, Representational] VoidRep
  where kind = ForAllTy kv $ mkArrowKinds [k, k] unliftedTypeKind
        kv = kKiVar
        k  = mkTyVarTy kv

{-
RealWorld is deeply magical.  It is *primitive*, but it is not
*unlifted* (hence ptrArg).  We never manipulate values of type
RealWorld; it's only used in the type system, to parameterise State#.
-}

realWorldTyCon :: TyCon
realWorldTyCon = mkLiftedPrimTyCon realWorldTyConName liftedTypeKind [] PtrRep
realWorldTy :: Type
realWorldTy          = mkTyConTy realWorldTyCon
realWorldStatePrimTy :: Type
realWorldStatePrimTy = mkStatePrimTy realWorldTy        -- State# RealWorld

{-
Note: the ``state-pairing'' types are not truly primitive, so they are
defined in \tr{TysWiredIn.lhs}, not here.

************************************************************************
*                                                                      *
\subsection[TysPrim-arrays]{The primitive array types}
*                                                                      *
************************************************************************
-}

arrayPrimTyCon, mutableArrayPrimTyCon, mutableByteArrayPrimTyCon,
    byteArrayPrimTyCon, arrayArrayPrimTyCon, mutableArrayArrayPrimTyCon,
    smallArrayPrimTyCon, smallMutableArrayPrimTyCon :: TyCon
arrayPrimTyCon             = pcPrimTyCon arrayPrimTyConName             [Representational] (ObjectRep stgArray)
mutableArrayPrimTyCon      = pcPrimTyCon  mutableArrayPrimTyConName     [Nominal, Representational] (ObjectRep stgArray)
mutableByteArrayPrimTyCon  = pcPrimTyCon mutableByteArrayPrimTyConName  [Nominal] (ObjectRep stgByteArray)
byteArrayPrimTyCon         = pcPrimTyCon0 byteArrayPrimTyConName        (ObjectRep stgByteArray)
arrayArrayPrimTyCon        = pcPrimTyCon0 arrayArrayPrimTyConName       (ObjectRep stgArray)
mutableArrayArrayPrimTyCon = pcPrimTyCon mutableArrayArrayPrimTyConName [Nominal] (ObjectRep stgArray)
smallArrayPrimTyCon        = pcPrimTyCon smallArrayPrimTyConName        [Representational] (ObjectRep stgArray)
smallMutableArrayPrimTyCon = pcPrimTyCon smallMutableArrayPrimTyConName [Nominal, Representational] (ObjectRep stgArray)

mkArrayPrimTy :: Type -> Type
mkArrayPrimTy elt           = TyConApp arrayPrimTyCon [elt]
byteArrayPrimTy :: Type
byteArrayPrimTy             = mkTyConTy byteArrayPrimTyCon
mkArrayArrayPrimTy :: Type
mkArrayArrayPrimTy = mkTyConTy arrayArrayPrimTyCon
mkSmallArrayPrimTy :: Type -> Type
mkSmallArrayPrimTy elt = TyConApp smallArrayPrimTyCon [elt]
mkMutableArrayPrimTy :: Type -> Type -> Type
mkMutableArrayPrimTy s elt  = TyConApp mutableArrayPrimTyCon [s, elt]
mkMutableByteArrayPrimTy :: Type -> Type
mkMutableByteArrayPrimTy s  = TyConApp mutableByteArrayPrimTyCon [s]
mkMutableArrayArrayPrimTy :: Type -> Type
mkMutableArrayArrayPrimTy s = TyConApp mutableArrayArrayPrimTyCon [s]
mkSmallMutableArrayPrimTy :: Type -> Type -> Type
mkSmallMutableArrayPrimTy s elt = TyConApp smallMutableArrayPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-mut-var]{The mutable variable type}
*                                                                      *
************************************************************************
-}

mutVarPrimTyCon :: TyCon
mutVarPrimTyCon = pcPrimTyCon mutVarPrimTyConName [Nominal, Representational]
                                                  (ObjectRep stgMutVar)

mkMutVarPrimTy :: Type -> Type -> Type
mkMutVarPrimTy s elt        = TyConApp mutVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-synch-var]{The synchronizing variable type}
*                                                                      *
************************************************************************
-}

mVarPrimTyCon :: TyCon
mVarPrimTyCon = pcPrimTyCon mVarPrimTyConName [Nominal, Representational]
                                              (ObjectRep stgMVar)

mkMVarPrimTy :: Type -> Type -> Type
mkMVarPrimTy s elt          = TyConApp mVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-stm-var]{The transactional variable type}
*                                                                      *
************************************************************************
-}

tVarPrimTyCon :: TyCon
tVarPrimTyCon = pcPrimTyCon tVarPrimTyConName [Nominal, Representational]
                                              (ObjectRep stgTVar)

mkTVarPrimTy :: Type -> Type -> Type
mkTVarPrimTy s elt = TyConApp tVarPrimTyCon [s, elt]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-stable-ptrs]{The stable-pointer type}
*                                                                      *
************************************************************************
-}

stablePtrPrimTyCon :: TyCon
stablePtrPrimTyCon = pcPrimTyCon stablePtrPrimTyConName [Representational] IntRep

mkStablePtrPrimTy :: Type -> Type
mkStablePtrPrimTy ty = TyConApp stablePtrPrimTyCon [ty]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-stable-names]{The stable-name type}
*                                                                      *
************************************************************************
-}

stableNamePrimTyCon :: TyCon
stableNamePrimTyCon = pcPrimTyCon stableNamePrimTyConName [Representational] IntRep

mkStableNamePrimTy :: Type -> Type
mkStableNamePrimTy ty = TyConApp stableNamePrimTyCon [ty]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-BCOs]{The ``bytecode object'' type}
*                                                                      *
************************************************************************
-}

bcoPrimTy    :: Type
bcoPrimTy    = mkTyConTy bcoPrimTyCon
bcoPrimTyCon :: TyCon
bcoPrimTyCon = pcPrimTyCon0 bcoPrimTyConName (ObjectRep stgBCO)

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-Weak]{The ``weak pointer'' type}
*                                                                      *
************************************************************************
-}

weakPrimTyCon :: TyCon
weakPrimTyCon = pcPrimTyCon weakPrimTyConName [Representational] (ObjectRep stgWeak)

mkWeakPrimTy :: Type -> Type
mkWeakPrimTy v = TyConApp weakPrimTyCon [v]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-thread-ids]{The ``thread id'' type}
*                                                                      *
************************************************************************

A thread id is represented by a pointer to the TSO itself, to ensure
that they are always unique and we can always find the TSO for a given
thread id.  However, this has the unfortunate consequence that a
ThreadId# for a given thread is treated as a root by the garbage
collector and can keep TSOs around for too long.

Hence the programmer API for thread manipulation uses a weak pointer
to the thread id internally.
-}

threadIdPrimTy :: Type
threadIdPrimTy    = mkTyConTy threadIdPrimTyCon
threadIdPrimTyCon :: TyCon
threadIdPrimTyCon = pcPrimTyCon0 threadIdPrimTyConName (ObjectRep stgTSO)

{-
************************************************************************
*                                                                      *
                Any
*                                                                      *
************************************************************************

Note [Any types]
~~~~~~~~~~~~~~~~
The type constructor Any of kind forall k. k has these properties:

  * It is defined in module GHC.Prim, and exported so that it is
    available to users.  For this reason it's treated like any other
    primitive type:
      - has a fixed unique, anyTyConKey,
      - lives in the global name cache

  * It is a *closed* type family, with no instances.  This means that
    if   ty :: '(k1, k2)  we add a given coercion
             g :: ty ~ (Fst ty, Snd ty)
    If Any was a *data* type, then we'd get inconsistency because 'ty'
    could be (Any '(k1,k2)) and then we'd have an equality with Any on
    one side and '(,) on the other. See also #9097.

  * It is lifted, and hence represented by a pointer

  * It is inhabited by at least one value, namely bottom

  * You can unsafely coerce any lifted type to Any, and back.

  * It does not claim to be a *data* type, and that's important for
    the code generator, because the code gen may *enter* a data value
    but never enters a function value.

  * It is used to instantiate otherwise un-constrained type variables
    For example         length Any []
    See Note [Strangely-kinded void TyCons]

Note [Any kinds]
~~~~~~~~~~~~~~~~

The type constructor AnyK (of sort BOX) is used internally only to zonk kind
variables with no constraints on them. It appears in similar circumstances to
Any, but at the kind level. For example:

  type family Length (l :: [k]) :: Nat
  type instance Length [] = Zero

Length is kind-polymorphic, and when applied to the empty (promoted) list it
will have the kind Length AnyK [].

Note [Strangely-kinded void TyCons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #959 for more examples

When the type checker finds a type variable with no binding, which
means it can be instantiated with an arbitrary type, it usually
instantiates it to Void.  Eg.

        length []
===>
        length Any (Nil Any)

But in really obscure programs, the type variable might have a kind
other than *, so we need to invent a suitably-kinded type.

This commit uses
        Any for kind *
        Any(*->*) for kind *->*
        etc
-}

anyTyConName :: Name
anyTyConName = mkPrimTc (fsLit "Any") anyTyConKey anyTyCon

anyTy :: Type
anyTy = mkTyConTy anyTyCon

anyTyCon :: TyCon
anyTyCon = mkFamilyTyCon anyTyConName kind [kKiVar]
                         AbstractClosedSynFamilyTyCon
                         NoParentTyCon
  where
    kind = ForAllTy kKiVar (mkTyVarTy kKiVar)

anyTypeOfKind :: Kind -> Type
anyTypeOfKind kind = TyConApp anyTyCon [kind]

{-
************************************************************************
*                                                                      *
\subsection[TysPrim-ETA]{The ETA-specific types}
*                                                                      *
************************************************************************
-}
jcharPrimTy, jboolPrimTy, jbytePrimTy, jshortPrimTy :: Type
jcharPrimTy = mkTyConTy jcharPrimTyCon
jboolPrimTy = mkTyConTy jboolPrimTyCon
jbytePrimTy = mkTyConTy jbytePrimTyCon
jshortPrimTy = mkTyConTy jshortPrimTyCon

jcharPrimTyCon, jboolPrimTyCon, jbytePrimTyCon, jshortPrimTyCon,
  jobjectPrimTyCon :: TyCon
jcharPrimTyCon   = pcPrimTyCon0 jcharPrimTyConName CharRep
jboolPrimTyCon   = pcPrimTyCon0 jboolPrimTyConName BoolRep
jbytePrimTyCon   = pcPrimTyCon0 jbytePrimTyConName ByteRep
jshortPrimTyCon  = pcPrimTyCon0 jshortPrimTyConName ShortRep
jobjectPrimTyCon = pcPrimTyCon jobjectPrimTyConName [Nominal] $ ObjectRep ""

mkObjectPrimTy :: Type -> Type
mkObjectPrimTy ty = TyConApp jobjectPrimTyCon [ty]

{-
************************************************************************
*                                                                      *
\subsection{SIMD vector types}
TODO: Vector operations disabled in ETA
*                                                                      *
************************************************************************
-}

-- int8X16PrimTyConName :: Name
-- int8X16PrimTyConName = mkPrimTc (fsLit "Int8X16#") int8X16PrimTyConKey int8X16PrimTyCon
-- int8X16PrimTy :: Type
-- int8X16PrimTy = mkTyConTy int8X16PrimTyCon
-- int8X16PrimTyCon :: TyCon
-- int8X16PrimTyCon = pcPrimTyCon0 int8X16PrimTyConName (VecRep 16 Int8ElemRep)
-- int16X8PrimTyConName :: Name
-- int16X8PrimTyConName = mkPrimTc (fsLit "Int16X8#") int16X8PrimTyConKey int16X8PrimTyCon
-- int16X8PrimTy :: Type
-- int16X8PrimTy = mkTyConTy int16X8PrimTyCon
-- int16X8PrimTyCon :: TyCon
-- int16X8PrimTyCon = pcPrimTyCon0 int16X8PrimTyConName (VecRep 8 Int16ElemRep)
-- int32X4PrimTyConName :: Name
-- int32X4PrimTyConName = mkPrimTc (fsLit "Int32X4#") int32X4PrimTyConKey int32X4PrimTyCon
-- int32X4PrimTy :: Type
-- int32X4PrimTy = mkTyConTy int32X4PrimTyCon
-- int32X4PrimTyCon :: TyCon
-- int32X4PrimTyCon = pcPrimTyCon0 int32X4PrimTyConName (VecRep 4 Int32ElemRep)
-- int64X2PrimTyConName :: Name
-- int64X2PrimTyConName = mkPrimTc (fsLit "Int64X2#") int64X2PrimTyConKey int64X2PrimTyCon
-- int64X2PrimTy :: Type
-- int64X2PrimTy = mkTyConTy int64X2PrimTyCon
-- int64X2PrimTyCon :: TyCon
-- int64X2PrimTyCon = pcPrimTyCon0 int64X2PrimTyConName (VecRep 2 Int64ElemRep)
-- int8X32PrimTyConName :: Name
-- int8X32PrimTyConName = mkPrimTc (fsLit "Int8X32#") int8X32PrimTyConKey int8X32PrimTyCon
-- int8X32PrimTy :: Type
-- int8X32PrimTy = mkTyConTy int8X32PrimTyCon
-- int8X32PrimTyCon :: TyCon
-- int8X32PrimTyCon = pcPrimTyCon0 int8X32PrimTyConName (VecRep 32 Int8ElemRep)
-- int16X16PrimTyConName :: Name
-- int16X16PrimTyConName = mkPrimTc (fsLit "Int16X16#") int16X16PrimTyConKey int16X16PrimTyCon
-- int16X16PrimTy :: Type
-- int16X16PrimTy = mkTyConTy int16X16PrimTyCon
-- int16X16PrimTyCon :: TyCon
-- int16X16PrimTyCon = pcPrimTyCon0 int16X16PrimTyConName (VecRep 16 Int16ElemRep)
-- int32X8PrimTyConName :: Name
-- int32X8PrimTyConName = mkPrimTc (fsLit "Int32X8#") int32X8PrimTyConKey int32X8PrimTyCon
-- int32X8PrimTy :: Type
-- int32X8PrimTy = mkTyConTy int32X8PrimTyCon
-- int32X8PrimTyCon :: TyCon
-- int32X8PrimTyCon = pcPrimTyCon0 int32X8PrimTyConName (VecRep 8 Int32ElemRep)
-- int64X4PrimTyConName :: Name
-- int64X4PrimTyConName = mkPrimTc (fsLit "Int64X4#") int64X4PrimTyConKey int64X4PrimTyCon
-- int64X4PrimTy :: Type
-- int64X4PrimTy = mkTyConTy int64X4PrimTyCon
-- int64X4PrimTyCon :: TyCon
-- int64X4PrimTyCon = pcPrimTyCon0 int64X4PrimTyConName (VecRep 4 Int64ElemRep)
-- int8X64PrimTyConName :: Name
-- int8X64PrimTyConName = mkPrimTc (fsLit "Int8X64#") int8X64PrimTyConKey int8X64PrimTyCon
-- int8X64PrimTy :: Type
-- int8X64PrimTy = mkTyConTy int8X64PrimTyCon
-- int8X64PrimTyCon :: TyCon
-- int8X64PrimTyCon = pcPrimTyCon0 int8X64PrimTyConName (VecRep 64 Int8ElemRep)
-- int16X32PrimTyConName :: Name
-- int16X32PrimTyConName = mkPrimTc (fsLit "Int16X32#") int16X32PrimTyConKey int16X32PrimTyCon
-- int16X32PrimTy :: Type
-- int16X32PrimTy = mkTyConTy int16X32PrimTyCon
-- int16X32PrimTyCon :: TyCon
-- int16X32PrimTyCon = pcPrimTyCon0 int16X32PrimTyConName (VecRep 32 Int16ElemRep)
-- int32X16PrimTyConName :: Name
-- int32X16PrimTyConName = mkPrimTc (fsLit "Int32X16#") int32X16PrimTyConKey int32X16PrimTyCon
-- int32X16PrimTy :: Type
-- int32X16PrimTy = mkTyConTy int32X16PrimTyCon
-- int32X16PrimTyCon :: TyCon
-- int32X16PrimTyCon = pcPrimTyCon0 int32X16PrimTyConName (VecRep 16 Int32ElemRep)
-- int64X8PrimTyConName :: Name
-- int64X8PrimTyConName = mkPrimTc (fsLit "Int64X8#") int64X8PrimTyConKey int64X8PrimTyCon
-- int64X8PrimTy :: Type
-- int64X8PrimTy = mkTyConTy int64X8PrimTyCon
-- int64X8PrimTyCon :: TyCon
-- int64X8PrimTyCon = pcPrimTyCon0 int64X8PrimTyConName (VecRep 8 Int64ElemRep)
-- word8X16PrimTyConName :: Name
-- word8X16PrimTyConName = mkPrimTc (fsLit "Word8X16#") word8X16PrimTyConKey word8X16PrimTyCon
-- word8X16PrimTy :: Type
-- word8X16PrimTy = mkTyConTy word8X16PrimTyCon
-- word8X16PrimTyCon :: TyCon
-- word8X16PrimTyCon = pcPrimTyCon0 word8X16PrimTyConName (VecRep 16 Word8ElemRep)
-- word16X8PrimTyConName :: Name
-- word16X8PrimTyConName = mkPrimTc (fsLit "Word16X8#") word16X8PrimTyConKey word16X8PrimTyCon
-- word16X8PrimTy :: Type
-- word16X8PrimTy = mkTyConTy word16X8PrimTyCon
-- word16X8PrimTyCon :: TyCon
-- word16X8PrimTyCon = pcPrimTyCon0 word16X8PrimTyConName (VecRep 8 Word16ElemRep)
-- word32X4PrimTyConName :: Name
-- word32X4PrimTyConName = mkPrimTc (fsLit "Word32X4#") word32X4PrimTyConKey word32X4PrimTyCon
-- word32X4PrimTy :: Type
-- word32X4PrimTy = mkTyConTy word32X4PrimTyCon
-- word32X4PrimTyCon :: TyCon
-- word32X4PrimTyCon = pcPrimTyCon0 word32X4PrimTyConName (VecRep 4 Word32ElemRep)
-- word64X2PrimTyConName :: Name
-- word64X2PrimTyConName = mkPrimTc (fsLit "Word64X2#") word64X2PrimTyConKey word64X2PrimTyCon
-- word64X2PrimTy :: Type
-- word64X2PrimTy = mkTyConTy word64X2PrimTyCon
-- word64X2PrimTyCon :: TyCon
-- word64X2PrimTyCon = pcPrimTyCon0 word64X2PrimTyConName (VecRep 2 Word64ElemRep)
-- word8X32PrimTyConName :: Name
-- word8X32PrimTyConName = mkPrimTc (fsLit "Word8X32#") word8X32PrimTyConKey word8X32PrimTyCon
-- word8X32PrimTy :: Type
-- word8X32PrimTy = mkTyConTy word8X32PrimTyCon
-- word8X32PrimTyCon :: TyCon
-- word8X32PrimTyCon = pcPrimTyCon0 word8X32PrimTyConName (VecRep 32 Word8ElemRep)
-- word16X16PrimTyConName :: Name
-- word16X16PrimTyConName = mkPrimTc (fsLit "Word16X16#") word16X16PrimTyConKey word16X16PrimTyCon
-- word16X16PrimTy :: Type
-- word16X16PrimTy = mkTyConTy word16X16PrimTyCon
-- word16X16PrimTyCon :: TyCon
-- word16X16PrimTyCon = pcPrimTyCon0 word16X16PrimTyConName (VecRep 16 Word16ElemRep)
-- word32X8PrimTyConName :: Name
-- word32X8PrimTyConName = mkPrimTc (fsLit "Word32X8#") word32X8PrimTyConKey word32X8PrimTyCon
-- word32X8PrimTy :: Type
-- word32X8PrimTy = mkTyConTy word32X8PrimTyCon
-- word32X8PrimTyCon :: TyCon
-- word32X8PrimTyCon = pcPrimTyCon0 word32X8PrimTyConName (VecRep 8 Word32ElemRep)
-- word64X4PrimTyConName :: Name
-- word64X4PrimTyConName = mkPrimTc (fsLit "Word64X4#") word64X4PrimTyConKey word64X4PrimTyCon
-- word64X4PrimTy :: Type
-- word64X4PrimTy = mkTyConTy word64X4PrimTyCon
-- word64X4PrimTyCon :: TyCon
-- word64X4PrimTyCon = pcPrimTyCon0 word64X4PrimTyConName (VecRep 4 Word64ElemRep)
-- word8X64PrimTyConName :: Name
-- word8X64PrimTyConName = mkPrimTc (fsLit "Word8X64#") word8X64PrimTyConKey word8X64PrimTyCon
-- word8X64PrimTy :: Type
-- word8X64PrimTy = mkTyConTy word8X64PrimTyCon
-- word8X64PrimTyCon :: TyCon
-- word8X64PrimTyCon = pcPrimTyCon0 word8X64PrimTyConName (VecRep 64 Word8ElemRep)
-- word16X32PrimTyConName :: Name
-- word16X32PrimTyConName = mkPrimTc (fsLit "Word16X32#") word16X32PrimTyConKey word16X32PrimTyCon
-- word16X32PrimTy :: Type
-- word16X32PrimTy = mkTyConTy word16X32PrimTyCon
-- word16X32PrimTyCon :: TyCon
-- word16X32PrimTyCon = pcPrimTyCon0 word16X32PrimTyConName (VecRep 32 Word16ElemRep)
-- word32X16PrimTyConName :: Name
-- word32X16PrimTyConName = mkPrimTc (fsLit "Word32X16#") word32X16PrimTyConKey word32X16PrimTyCon
-- word32X16PrimTy :: Type
-- word32X16PrimTy = mkTyConTy word32X16PrimTyCon
-- word32X16PrimTyCon :: TyCon
-- word32X16PrimTyCon = pcPrimTyCon0 word32X16PrimTyConName (VecRep 16 Word32ElemRep)
-- word64X8PrimTyConName :: Name
-- word64X8PrimTyConName = mkPrimTc (fsLit "Word64X8#") word64X8PrimTyConKey word64X8PrimTyCon
-- word64X8PrimTy :: Type
-- word64X8PrimTy = mkTyConTy word64X8PrimTyCon
-- word64X8PrimTyCon :: TyCon
-- word64X8PrimTyCon = pcPrimTyCon0 word64X8PrimTyConName (VecRep 8 Word64ElemRep)
-- floatX4PrimTyConName :: Name
-- floatX4PrimTyConName = mkPrimTc (fsLit "FloatX4#") floatX4PrimTyConKey floatX4PrimTyCon
-- floatX4PrimTy :: Type
-- floatX4PrimTy = mkTyConTy floatX4PrimTyCon
-- floatX4PrimTyCon :: TyCon
-- floatX4PrimTyCon = pcPrimTyCon0 floatX4PrimTyConName (VecRep 4 FloatElemRep)
-- doubleX2PrimTyConName :: Name
-- doubleX2PrimTyConName = mkPrimTc (fsLit "DoubleX2#") doubleX2PrimTyConKey doubleX2PrimTyCon
-- doubleX2PrimTy :: Type
-- doubleX2PrimTy = mkTyConTy doubleX2PrimTyCon
-- doubleX2PrimTyCon :: TyCon
-- doubleX2PrimTyCon = pcPrimTyCon0 doubleX2PrimTyConName (VecRep 2 DoubleElemRep)
-- floatX8PrimTyConName :: Name
-- floatX8PrimTyConName = mkPrimTc (fsLit "FloatX8#") floatX8PrimTyConKey floatX8PrimTyCon
-- floatX8PrimTy :: Type
-- floatX8PrimTy = mkTyConTy floatX8PrimTyCon
-- floatX8PrimTyCon :: TyCon
-- floatX8PrimTyCon = pcPrimTyCon0 floatX8PrimTyConName (VecRep 8 FloatElemRep)
-- doubleX4PrimTyConName :: Name
-- doubleX4PrimTyConName = mkPrimTc (fsLit "DoubleX4#") doubleX4PrimTyConKey doubleX4PrimTyCon
-- doubleX4PrimTy :: Type
-- doubleX4PrimTy = mkTyConTy doubleX4PrimTyCon
-- doubleX4PrimTyCon :: TyCon
-- doubleX4PrimTyCon = pcPrimTyCon0 doubleX4PrimTyConName (VecRep 4 DoubleElemRep)
-- floatX16PrimTyConName :: Name
-- floatX16PrimTyConName = mkPrimTc (fsLit "FloatX16#") floatX16PrimTyConKey floatX16PrimTyCon
-- floatX16PrimTy :: Type
-- floatX16PrimTy = mkTyConTy floatX16PrimTyCon
-- floatX16PrimTyCon :: TyCon
-- floatX16PrimTyCon = pcPrimTyCon0 floatX16PrimTyConName (VecRep 16 FloatElemRep)
-- doubleX8PrimTyConName :: Name
-- doubleX8PrimTyConName = mkPrimTc (fsLit "DoubleX8#") doubleX8PrimTyConKey doubleX8PrimTyCon
-- doubleX8PrimTy :: Type
-- doubleX8PrimTy = mkTyConTy doubleX8PrimTyCon
-- doubleX8PrimTyCon :: TyCon
-- doubleX8PrimTyCon = pcPrimTyCon0 doubleX8PrimTyConName (VecRep 8 DoubleElemRep)
