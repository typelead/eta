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
   unsafe_stripNV)
where

import Id
import VarEnv
import DataCon
import TyCon
import Type

type CgBindings = IdEnv CgIdInfo

data CgIdInfo = CgIdInfo {
  cgId :: Id,
  cgLambdaForm :: LambdaFormInfo }

mkCgIdInfo :: Id -> LambdaFormInfo -> CgIdInfo
mkCgIdInfo id lambdaFormInfo = CgIdInfo {
  cgId = id,
  cgLambdaForm = lambdaFormInfo }

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
unsafe_stripNV :: NonVoid a -> a
unsafe_stripNV (NonVoid a) = a

nonVoidIds :: [Id] -> [NonVoid Id]
nonVoidIds ids = [NonVoid id | id <- ids, not (isVoidRep (idPrimRep id))]

idPrimRep :: Id -> PrimRep
idPrimRep = typePrimRep . idType

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
