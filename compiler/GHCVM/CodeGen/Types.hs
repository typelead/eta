{-# LANGUAGE OverloadedStrings #-}
module GHCVM.CodeGen.Types
  (TopLevelFlag(..),
   RepArity,
   CgLoc(..),
   CgIdInfo(..),
   NonVoid(..),
   LambdaFormInfo(..),
   StandardFormInfo(..),
   ArgDescr(..),
   RecFlag(..),
   Sequel(..),
   SelfLoopInfo,
   CgBindings,
   RecIndexes,
   storeDefault,
   locArgRep,
   mkRepLocDirect,
   mkLocDirect,
   mkLocLocal,
   getNonVoidFts,
   enterMethod,
   evaluateMethod,
   loadLoc,
   storeLoc,
   locFt,
   isRec,
   isNonRec,
   mkCgIdInfo,
   mkCgIdInfoWithLoc,
   unsafeStripNV,
   nonVoidIds,
   getJavaInfo,
   getNonVoids,
   getLocField,
   isLFThunk,
   lfFieldType,
   lfStaticThunk)
where

import GHCVM.BasicTypes.Id
import GHCVM.BasicTypes.VarEnv
import GHCVM.BasicTypes.DataCon
import GHCVM.Types.TyCon
import GHCVM.Types.Type
import GHCVM.BasicTypes.Module
import GHCVM.BasicTypes.Name
import GHCVM.Main.DynFlags

import Codec.JVM

import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Rts
import GHCVM.CodeGen.ArgRep
import GHCVM.Debug
import GHCVM.Util

import Data.Maybe
import Data.Text (Text)
import Data.Monoid ((<>))

type SelfLoopInfo = (Id, Label, [CgLoc])

data Sequel
  = Return
  | AssignTo [CgLoc]

data CgLoc = LocLocal Bool FieldType !Int
           | LocStatic FieldType Text Text
           | LocField Bool FieldType Text Text
           | LocDirect Bool FieldType Code
           | LocLne Label [CgLoc]

instance Outputable CgLoc where
  ppr (LocLocal isClosure ft int) = str "local: " <+> ppr int <+> ppr isClosure
  ppr LocStatic {} = str "static"
  ppr LocField {} = str "field"
  ppr LocDirect {} = str "direct"
  ppr LocLne {} = str "lne"

mkLocDirect :: Bool -> (FieldType, Code) -> CgLoc
mkLocDirect isClosure (ft, code) = LocDirect isClosure ft code

mkLocLocal :: Bool -> FieldType -> Int -> CgLoc
mkLocLocal isClosure ft int = LocLocal isClosure ft int

mkRepLocDirect :: (PrimRep, Code) -> CgLoc
mkRepLocDirect (rep, code) = LocDirect isClosure ft code
  where isClosure = isGcPtrRep rep
        ft = expectJust "mkRepLocDirect" $ primRepFieldType_maybe rep

locArgRep :: CgLoc -> ArgRep
locArgRep loc = case loc of
  LocLocal isClosure ft _ -> locRep isClosure ft
  LocStatic ft _ _ -> P
  LocField isClosure ft _ _ -> locRep isClosure ft
  LocDirect isClosure ft _ -> locRep isClosure ft
  LocLne _ _ -> panic "logArgRep: Cannot pass a let-no-escape binding!"
  where locRep isClosure ft = if isClosure then P else ftArgRep ft

locFt :: CgLoc -> FieldType
locFt (LocLocal _ ft _) = ft
locFt (LocStatic ft _ _) = ft
locFt (LocField _ ft _ _) = ft
locFt (LocDirect _ ft _) = ft

storeLoc :: CgLoc -> Code -> Code
storeLoc (LocLocal _ ft n) code = code <> gstore ft n

storeDefault :: CgLoc -> Code
storeDefault cgLoc = storeLoc cgLoc $ defaultValue (locFt cgLoc)

loadLoc :: CgLoc -> Code
loadLoc (LocLocal _ ft n) = gload ft n
loadLoc (LocStatic ft modClass clName) =
  getstatic $ mkFieldRef modClass (closure clName) ft
loadLoc (LocField _ ft clClass fieldName) =
     gload (obj clClass) 0
  <> getfield (mkFieldRef clClass fieldName ft)
loadLoc (LocDirect _ _ code) = code

type CgBindings = IdEnv CgIdInfo

data CgIdInfo =
  CgIdInfo { cgId         :: Id,
             cgLambdaForm :: LambdaFormInfo,
             cgLocation   :: CgLoc }

instance Outputable CgIdInfo where
  ppr CgIdInfo {..} = ppr cgId <+> str "-->" <+> ppr cgLocation

splitStaticLoc :: CgLoc -> (Text, Text)
splitStaticLoc (LocStatic ft modClass clName) = (modClass, clName)
splitStaticLoc _ = error $ "splitStaticLoc: Not LocStatic"

getJavaInfo :: DynFlags -> CgIdInfo -> (Text, Text, Text)
getJavaInfo dflags CgIdInfo { cgLocation, cgLambdaForm }
  = (modClass, clName, clClass)
  where (modClass, clName) = splitStaticLoc cgLocation
        -- TODO: Reduce duplication
        clClass = fromMaybe (qualifiedName modClass clName)
                            $ maybeDataConClass dflags cgLambdaForm

maybeDataConClass :: DynFlags -> LambdaFormInfo -> Maybe Text
maybeDataConClass dflags (LFCon dataCon) = Just $ dataConClass dflags dataCon
maybeDataConClass dflags _ = Nothing

mkCgIdInfo :: DynFlags -> Id -> LambdaFormInfo -> CgIdInfo
mkCgIdInfo dflags id lfInfo =
  CgIdInfo { cgId = id
           , cgLambdaForm = lfInfo
           , cgLocation = loc }
  where loc = mkStaticLoc dflags id lfInfo

mkCgIdInfoWithLoc :: Id -> LambdaFormInfo -> CgLoc -> CgIdInfo
mkCgIdInfoWithLoc id lfInfo cgLoc =
  CgIdInfo { cgId = id
           , cgLambdaForm = lfInfo
           , cgLocation = cgLoc }

mkStaticLoc :: DynFlags -> Id -> LambdaFormInfo -> CgLoc
mkStaticLoc dflags id lfInfo = LocStatic closureType modClass clName
  where name = idName id
        mod = fromMaybe (error "mkStaticLoc: No module")
            $ nameModule_maybe name
        clName = nameText dflags True name
        modClass = moduleJavaClass mod
        -- clClass
        --   | Just c <- maybeDataConClass lfInfo = c
        --   | Just c <- maybeTyConClass (idType id) = c
        --   | otherwise = qualifiedName modClass clName

-- maybeTyConClass :: Type -> Maybe Text
-- maybeTyConClass ty = case repType ty of
--   UnaryRep (TyConApp tc _) -> Just $ tyConClass tc
--   _ -> Nothing

type Liveness = [Bool]   -- One Bool per word; True  <=> non-ptr or dead

data StandardFormInfo
  = NonStandardThunk
        -- The usual case: not of the standard forms

  | SelectorThunk
        -- A SelectorThunk is of form
        --      case x of
        --           con a1,..,an -> ak
        -- and the constructor is from a single-constr type.
      Int -- Field position
      ArgRep -- Field type
        --WordOff         -- 0-origin offset of ak within the "goods" of
                        -- constructor (Recall that the a1,...,an may be laid
                        -- out in the heap in a non-obvious order.)

  | ApThunk
        -- An ApThunk is of form
        --        x1 ... xn
        -- The code for the thunk just pushes x2..xn on the stack and enters x1.
        -- There are a few of these (for 1 <= n <= MAX_SPEC_AP_SIZE) pre-compiled
        -- in the RTS to save space.
        RepArity                -- Arity, n

data ArgDescr
  = ArgSpec             -- Fits one of the standard patterns
        !Int            -- RTS type identifier ARG_P, ARG_N, ...

  | ArgGen              -- General case
        Liveness        -- Details about the arguments

data LambdaFormInfo
  = LFReEntrant {
      lfTopLevelFlag :: TopLevelFlag,
      lfArity :: !RepArity,
      lfNoFreeVars :: !Bool,
      lfArgDescriptor :: ArgDescr }

  | LFThunk {
      lfTopLevelFlag :: TopLevelFlag,
      lfNoFreeVars :: !Bool,
      lfUpdatable :: !Bool,
      lfStandardFormInfo :: StandardFormInfo,
      lfMaybeFunction :: !Bool }

  | LFCon { lfDataCon :: DataCon }

  | LFUnknown { lfMaybeFunction :: !Bool }

  | LFUnLifted          -- A value of unboxed type;
                        -- always a value, needs evaluation

  | LFLetNoEscape       -- See LetNoEscape module for precise description

lfFieldType :: LambdaFormInfo -> FieldType
lfFieldType LFReEntrant {} = funType
lfFieldType LFThunk {} = thunkType
lfFieldType LFCon {} = conType
lfFieldType _ = closureType

isLFSimple :: LambdaFormInfo -> Bool
isLFSimple LFUnLifted = True
isLFSimple LFUnknown {} = True
isLFSimple LFLetNoEscape = True
isLFSimple _ = False

isLFThunk :: LambdaFormInfo -> Bool
isLFThunk LFThunk {} = True
isLFThunk _          = False

lfStaticThunk :: LambdaFormInfo -> Bool
lfStaticThunk (LFThunk topLevel _ _ _ _) = isTopLevel topLevel
lfStaticThunk _ = False

-------------------------------------
--        Non-void types
-------------------------------------
-- We frequently need the invariant that an Id or a an argument
-- is of a non-void type. This type is a witness to the invariant.

newtype NonVoid a = NonVoid a
  deriving (Eq, Show)

instance Outputable a => Outputable (NonVoid a) where
  ppr (NonVoid x) = ppr x

-- Use with care; if used inappropriately, it could break invariants.
unsafeStripNV :: NonVoid a -> a
unsafeStripNV (NonVoid a) = a

nonVoidIds :: [Id] -> [NonVoid Id]
nonVoidIds ids = [NonVoid id | id <- ids, not (isVoidRep (idPrimRep id))]

data TopLevelFlag
  = TopLevel
  | NotTopLevel

isTopLevel :: TopLevelFlag -> Bool
isTopLevel TopLevel = True
isTopLevel _ = False

type RepArity = Int

data RecFlag = Recursive
             | NonRecursive
             deriving Eq

isRec :: RecFlag -> Bool
isRec Recursive    = True
isRec NonRecursive = False

isNonRec :: RecFlag -> Bool
isNonRec Recursive    = False
isNonRec NonRecursive = True

getNonVoids :: [(Maybe FieldType, a)] -> [NonVoid a]
getNonVoids = mapMaybe (\(mft, val) -> case mft of
                           Just _ -> Just (NonVoid val)
                           Nothing -> Nothing)

getNonVoidFts :: [(Maybe FieldType, a)] -> [(FieldType, NonVoid a)]
getNonVoidFts = mapMaybe (\(mft, val) -> case mft of
                           Just ft -> Just (ft, NonVoid val)
                           Nothing -> Nothing)

getLocField :: CgLoc -> Maybe FieldRef
getLocField (LocStatic ft modClass clName) =
  Just $ mkFieldRef modClass (closure clName) ft
getLocField (LocField _ ft clClass fieldName) =
  Just $ mkFieldRef clClass fieldName ft
getLocField _ =
  Nothing

enterMethod :: CgLoc -> Code
enterMethod cgLoc
  = loadLoc cgLoc
 <> loadContext
 -- TODO: Do better than stgClosure
 <> invokevirtual (mkMethodRef stgClosure "enter" [contextType] void)

evaluateMethod :: CgLoc -> Code
evaluateMethod cgLoc
  = loadLoc cgLoc
 <> loadContext
 <> invokevirtual (mkMethodRef stgClosure "evaluate" [contextType] void)
 -- TODO: Narrrow the invokevirtual call with locFt

type RecIndexes = [(Int, Id)]
