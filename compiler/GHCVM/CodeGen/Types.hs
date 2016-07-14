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
   loadLoc,
   isRec,
   isNonRec,
   mkCgIdInfo,
   mkCgIdInfoWithLoc,
   unsafeStripNV,
   nonVoidIds,
   getJavaInfo,
   getNonVoids,
   isLFThunk,
   lfStaticThunk)
where

import Id
import VarEnv
import DataCon
import TyCon
import Type
import Module
import Name

import Codec.JVM

import GHCVM.Primitive
import GHCVM.CodeGen.Name
import GHCVM.CodeGen.Rts

import Data.Maybe
import Data.Text (Text)
import Data.Monoid ((<>))

-- TODO: Select appropriate fields
type SelfLoopInfo = (Id, Int, [CgLoc])

data Sequel
  = Return
  | AssignTo [CgLoc]

data CgLoc = LocLocal FieldType !Int
           | LocStatic FieldType Text Text
           | LocField FieldType Text Text

locFt :: CgLoc -> FieldType
locFt (LocLocal ft _) = ft
locFt (LocStatic ft _ _) = ft
locFt (LocField ft _ _) = ft

loadLoc :: CgLoc -> Code
loadLoc (LocLocal ft n) = gload ft n
loadLoc (LocStatic ft modClass clName) =
  getstatic $ mkFieldRef modClass clNameWithSuffix ft
  where clNameWithSuffix = closure clName
loadLoc (LocField ft clClass fieldName) =
     gload (obj clClass) 0
  <> getfield (mkFieldRef clClass fieldName ft)

type CgBindings = IdEnv CgIdInfo

data CgIdInfo =
  CgIdInfo { cgId         :: Id,
             cgLambdaForm :: LambdaFormInfo,
             cgLocation   :: CgLoc }

splitStaticLoc :: CgLoc -> (Text, Text)
splitStaticLoc (LocStatic ft modClass clName) = (modClass, clName)
splitStaticLoc _ = error $ "splitStaticLoc: Not LocStatic"

getJavaInfo :: CgIdInfo -> (Text, Text, Text)
getJavaInfo CgIdInfo { cgLocation, cgLambdaForm } = (modClass, clName, clClass)
  where (modClass, clName) = splitStaticLoc cgLocation
        -- TODO: Reduce duplication
        clClass = fromMaybe (qualifiedName modClass clName)
                            $ maybeDataConClass cgLambdaForm

maybeDataConClass :: LambdaFormInfo -> Maybe Text
maybeDataConClass lfInfo =
  case lfInfo of
    LFCon dataCon ->
      let dataName = dataConName dataCon
          dataClass = nameDataText dataName
          -- TODO: Most likely this will fail for same module data cons
          -- Maybe externalize the data con name?
          dataModuleClass = moduleJavaClass
                          . fromMaybe (error "Failed")
                          $ nameModule_maybe dataName
      in Just $ qualifiedName dataModuleClass dataClass
    _ -> Nothing


mkCgIdInfo :: Id -> LambdaFormInfo -> CgIdInfo
mkCgIdInfo id lfInfo =
  CgIdInfo { cgId = id
           , cgLambdaForm = lfInfo
           , cgLocation = loc }
  where loc = mkStaticLoc id lfInfo
        mod = nameModule . idName $ id

mkCgIdInfoWithLoc :: Id -> LambdaFormInfo -> CgLoc -> CgIdInfo
mkCgIdInfoWithLoc id lfInfo cgLoc =
  CgIdInfo { cgId = id
           , cgLambdaForm = lfInfo
           , cgLocation = cgLoc }

mkStaticLoc :: Id -> LambdaFormInfo -> CgLoc
mkStaticLoc id lfInfo = LocStatic (obj clClass) modClass clName
  where name = idName id
        mod = nameModule name
        clName = nameText name
        modClass = moduleJavaClass mod
        -- TODO: Reduce duplication
        clClass = fromMaybe (qualifiedName modClass clName)
                            $ maybeDataConClass lfInfo

type Liveness = [Bool]   -- One Bool per word; True  <=> non-ptr or dead

data StandardFormInfo
  = NonStandardThunk
        -- The usual case: not of the standard forms

  | SelectorThunk
        -- A SelectorThunk is of form
        --      case x of
        --           con a1,..,an -> ak
        -- and the constructor is from a single-constr type.
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

-- Use with care; if used inappropriately, it could break invariants.
unsafeStripNV :: NonVoid a -> a
unsafeStripNV (NonVoid a) = a

nonVoidIds :: [Id] -> [NonVoid Id]
nonVoidIds ids = [NonVoid id | id <- ids, not (isVoidJRep (idJPrimRep id))]

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
