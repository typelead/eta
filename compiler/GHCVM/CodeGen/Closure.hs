module GHCVM.CodeGen.Closure where

import DynFlags
import Id
import StgSyn
import Type
import TyCon
import DataCon
import Panic
import FastString
import OccName
import Name
import Module

import GHCVM.CodeGen.Types
import GHCVM.CodeGen.ArgRep
import GHCVM.CodeGen.Object
import GHCVM.Primitive

import Data.Maybe

data ClosureRep = IndStatic Id

mkClosureLFInfo :: DynFlags
                -> Id           -- The binder
                -> TopLevelFlag -- True of top level
                -> [NonVoid Id] -- Free vars
                -> UpdateFlag   -- Update flag
                -> [Id]         -- Args
                -> LambdaFormInfo
mkClosureLFInfo dflags binder topLevelFlag freeVars updateFlag args
  | null args =
        mkLFThunk (idType binder) topLevelFlag
          (map unsafe_stripNV freeVars) updateFlag
  | otherwise =
        mkLFReEntrant topLevelFlag (map unsafe_stripNV freeVars)
          args (mkArgDescr dflags args)

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

mkArgDescr :: DynFlags -> [Id] -> ArgDescr
mkArgDescr dflags args
  = let argReps = filter isNonV (map idJArgRep args)
           -- Getting rid of voids eases matching of standard patterns
    in case stdPattern argReps of
         Just specId -> ArgSpec specId
         Nothing     -> ArgGen []

stdPattern :: [JArgRep] -> Maybe Int
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
  where arity = idRepArity id -- TODO: Need to override idRepArity

mkLFArgument :: Id -> LambdaFormInfo
mkLFArgument id
  | isUnLiftedType ty      = LFUnLifted
  | maybeFunction ty       = LFUnknown True
  | otherwise              = LFUnknown False
  where
    ty = idType id

-- newClosureField :: (Generator e g)
--                 => B.ByteString
--                 -> String
--                 -> g e ()
--                 -> g e ()
-- newClosureField fieldName className initCode = newField [ACC_PUBLIC, ACC_STATIC] fieldName (ObjectType className) (Just initCode)

-- emitClosure :: Id -> ClosureRep -> GenerateIO e ()
-- emitClosure closureId (IndStatic id) =
--   newClosureField (idNameBS closureId) (BC.unpack indStaticObj) $ do
--     new indStaticObj
--     dup
--     getStaticField (idClassBS id) (NameType (idNameBS id)
--                                             (ObjectType (BC.unpack closureObj)))
--     invokeSpecial indStaticObj (NameType "<init>" (MethodSignature
--                                                    [obj (BC.unpack closureObj)]
--                                                    ReturnsVoid))
--   where indirectClosureBS = idNameBS id


newClosureInnerClass = undefined
-- newClosureInnnerClass :: (Generator e g)
--                       => Id
--                       -> LambdaFormInfo
--                          = newInnerClass [ACC_PUBLIC, ACC_STATIC]
