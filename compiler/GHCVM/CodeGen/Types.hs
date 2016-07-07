module GHCVM.CodeGen.Types
  (TopLevelFlag(..), RepArity,
   CgIdInfo(..),
   NonVoid(..),
   LambdaFormInfo(..),
   StandardFormInfo(..),
   ArgDescr(..),
   RecFlag(..),
   CgBindings,
   isRec,
   isNonRec,
   mkCgIdInfo,
   unsafeStripNV,
   getNonVoids)
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

type CgBindings = IdEnv CgIdInfo

data CgIdInfo = CgIdInfo {
  cgId :: Id,
  cgLambdaForm :: LambdaFormInfo,
  cgModuleClass :: Text,
  cgClosureName :: Text,
  cgClosureClass :: Text,
  cgFieldType :: FieldType }

mkCgIdInfo :: Module -> Id -> LambdaFormInfo -> CgIdInfo
mkCgIdInfo curMod id lambdaFormInfo = CgIdInfo {
  cgId = id,
  cgLambdaForm = lambdaFormInfo,
  cgModuleClass = modClass,
  cgClosureName = closure closureId,
  cgClosureClass = closureClass,
  cgFieldType = obj closureClass }
  -- TODO: Populate cgClosureClass with Nothing for certain lambda forms
  where name = idName id
        mod = fromMaybe curMod
            $ nameModule_maybe name
        closureId = nameText name
        modClass = moduleJavaClass mod
        closureClass = case lambdaFormInfo of
          LFCon dataCon ->
            let dataName = dataConName dataCon
                dataClass = nameText dataName
                dataModuleClass = moduleJavaClass
                              -- TODO: Most likely this will fail for same module data cons
                              -- Maybe externalize the data con name?
                              . fromMaybe curMod
                              $ nameModule_maybe dataName
            in qualifiedName dataModuleClass dataClass
          _ -> qualifiedName modClass closureId

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
