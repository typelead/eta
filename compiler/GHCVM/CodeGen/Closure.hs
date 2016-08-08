module GHCVM.CodeGen.Closure where

import GHCVM.Main.DynFlags
import GHCVM.BasicTypes.Id
import GHCVM.StgSyn.StgSyn
import GHCVM.Types.Type
import GHCVM.Types.TyCon
import GHCVM.BasicTypes.DataCon
import GHCVM.Utils.Panic
import GHCVM.Utils.FastString
import GHCVM.BasicTypes.OccName
import GHCVM.BasicTypes.Name
import GHCVM.BasicTypes.Module

import GHCVM.CodeGen.Types
import GHCVM.CodeGen.ArgRep

import Codec.JVM

mkClosureLFInfo :: Id           -- The binder
                -> TopLevelFlag -- True of top level
                -> [NonVoid Id] -- Free vars
                -> UpdateFlag   -- Update flag
                -> [Id]         -- Args
                -> LambdaFormInfo
mkClosureLFInfo binder topLevelFlag freeVars updateFlag args
  | null args =
        mkLFThunk (idType binder) topLevelFlag
          (map unsafeStripNV freeVars) updateFlag
  | otherwise =
        mkLFReEntrant topLevelFlag (map unsafeStripNV freeVars)
          args (mkArgDescr args)

mkConLFInfo :: DataCon -> LambdaFormInfo
mkConLFInfo = LFCon

mkLFReEntrant :: TopLevelFlag -> [Id] -> [Id] -> ArgDescr -> LambdaFormInfo
mkLFReEntrant topLevelFlag freeVars args argDescriptor =
  LFReEntrant {
    lfTopLevelFlag = topLevelFlag,
    lfArity = length args,
    lfNoFreeVars = null freeVars,
    lfArgDescriptor = argDescriptor }

mkLFThunk :: Type -> TopLevelFlag -> [Id] -> UpdateFlag -> LambdaFormInfo
mkLFThunk thunkType topLevelFlag freeVars updateFlag
  = -- Invariant: not updatable or lifted type
    LFThunk {
      lfTopLevelFlag = topLevelFlag,
      lfNoFreeVars = null freeVars,
      lfUpdatable = isUpdatable updateFlag,
      lfStandardFormInfo = NonStandardThunk,
      lfMaybeFunction = maybeFunction thunkType }

maybeFunction :: Type -> Bool
maybeFunction ty
  | UnaryRep rep <- repType ty
  , Just tc <- tyConAppTyCon_maybe rep
  , isDataTyCon tc
  = False
  | otherwise
  = True

mkArgDescr :: [Id] -> ArgDescr
mkArgDescr args
  = let argReps = filter isNonV (map idArgRep args)
           -- Getting rid of voids eases matching of standard patterns
    in case stdPattern argReps of
         Just specId -> ArgSpec specId
         Nothing     -> ArgGen []

stdPattern :: [ArgRep] -> Maybe Int
stdPattern reps = Nothing

mkLFImported :: Id -> LambdaFormInfo
mkLFImported id
  | Just con <- isDataConWorkId_maybe id
  , isNullaryRepDataCon con
  = LFCon { lfDataCon = con }
  | arity > 0
  = LFReEntrant {
      lfTopLevelFlag = TopLevel,
      lfArity = arity,
      lfNoFreeVars = True,
      lfArgDescriptor = panic "arg_descr" }
  | otherwise
  = mkLFArgument id
  where arity = idRepArity id

mkLFArgument :: Id -> LambdaFormInfo
mkLFArgument id
  | isUnLiftedType ty      = LFUnLifted
  | maybeFunction ty       = LFUnknown True
  | otherwise              = LFUnknown False
  where
    ty = idType id

argPrimRep :: StgArg -> PrimRep
argPrimRep = typePrimRep . stgArgType

data CallMethod
  = EnterIt
  | JumpToIt Label [CgLoc]
  | ReturnIt
  | SlowCall
  | DirectEntry Code RepArity

getCallMethod
  :: DynFlags
  -> Name           -- Function being applied
  -> Id             -- Function Id used to chech if it can refer to
                    -- CAF's and whether the function is tail-calling
                    -- itself
  -> LambdaFormInfo -- Its info
  -> RepArity       -- Number of available arguments
  -> CgLoc          -- Passed in from cgIdApp so that we can
                    -- handle let-no-escape bindings and self-recursive
                    -- tail calls using the same data constructor,
                    -- JumpToIt. This saves us one case branch in
                    -- cgIdApp
  -> Maybe SelfLoopInfo -- can we perform a self-recursive tail call?
  -> CallMethod

getCallMethod dflags _ id _ n _ (Just (selfLoopId, label, cgLocs))
  | gopt Opt_Loopification dflags, id == selfLoopId, n == length cgLocs
  = JumpToIt label cgLocs

-- TODO: Enter via node when in parallel
getCallMethod dflags name id (LFReEntrant _ arity _ _) n cgLoc _
  | n == 0         = ReturnIt        -- No args at all
  | n < arity      = SlowCall        -- Not enough args
  | otherwise      = DirectEntry (enterMethod cgLoc) arity

getCallMethod _ _ _ LFUnLifted _ _ _
  = ReturnIt

getCallMethod _ _ _ (LFCon _) _ _ _
  = ReturnIt

getCallMethod dflags name id
              (LFThunk _ _ updatable stdFormInfo isFun) n cgLoc _
  | isFun      -- it *might* be a function, so we must "call" it (which is always safe)
  = SlowCall
  -- Since isFun is False, we are *definitely* looking at a data value
  | updatable
  = EnterIt
  -- TODO: Is the below necessary?
  -- even a non-updatable selector thunk can be updated by the garbage
  -- collector.
  | SelectorThunk {} <- stdFormInfo
  = EnterIt
  | otherwise        -- Jump direct to code for single-entry thunks
  = DirectEntry (enterMethod cgLoc) 0

getCallMethod _ _ _ (LFUnknown True) _ _ _
  = SlowCall -- might be a function

getCallMethod _ _ _ (LFUnknown False) _ _ _
  = EnterIt -- Not a function

getCallMethod _ _ _ LFLetNoEscape _ (LocLne label cgLocs) _
  = JumpToIt label cgLocs

getCallMethod _ _ _ _ _ _ _ = panic "Unknown call method"

mkApLFInfo :: Id -> UpdateFlag -> Int -> LambdaFormInfo
mkApLFInfo id updateFlag arity
  = LFThunk NotTopLevel (arity == 0)
           (isUpdatable updateFlag) (ApThunk arity) (maybeFunction (idType id))

lneIdInfo :: Label -> Id -> [CgLoc] -> CgIdInfo
lneIdInfo label id cgLocs =
  CgIdInfo
  { cgId = id
  , cgLambdaForm =  mkLFLetNoEscape
  , cgLocation = LocLne label cgLocs }

getDataConTag :: DataCon -> Int
getDataConTag = dataConTag

mkLFLetNoEscape :: LambdaFormInfo
mkLFLetNoEscape = LFLetNoEscape
