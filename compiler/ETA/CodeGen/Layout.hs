{-# LANGUAGE OverloadedStrings #-}
module ETA.CodeGen.Layout where

import ETA.Types.Type
import ETA.Types.TyCon
import ETA.Main.DynFlags
import ETA.StgSyn.StgSyn
import Codec.JVM
import ETA.Util
import ETA.BasicTypes.Var
import ETA.CodeGen.Monad
import ETA.CodeGen.Types
import ETA.CodeGen.ArgRep
import ETA.CodeGen.Rts
import ETA.CodeGen.Env


import Data.Maybe
import Data.Monoid
import Data.Foldable

emitReturn :: [CgLoc] -> CodeGen ()
emitReturn results = do
  sequel <- getSequel
  emit $
    case sequel of
      Return         -> mkReturnExit results <> greturn closureType
      AssignTo slots -> multiAssign slots (map loadLoc results)

emitAssign :: CgLoc -> Code -> CodeGen ()
emitAssign cgLoc code = emit $ storeLoc cgLoc code

-- TODO: Verify that this is valid in all cases,
--       otherwise fall back on the strongly connected components
--       algorithm a la GHC
multiAssign :: [CgLoc] -> [Code] -> Code
multiAssign locs codes = fold $ zipWith storeLoc locs codes

-- TODO: There are a lot of bangs in this function. Verify that they do
--       indeed help.
mkCallEntry :: Bool -> Bool -> [CgLoc] -> Code
mkCallEntry convert store cgLocs = go mempty cgLocs 2 1 1 1 1 1
  where go !code (cgLoc:cgLocs) !r !i !l !f !d !o =
          case argRep of
            P -> loadRec (context r) (r + 1) i l f d o
            N -> loadRec (context i <> maybeConvInt) r (i + 1) l f d o
            L -> loadRec (context l) r i (l + 1) f d o
            F -> loadRec (context f) r i l (f + 1) d o
            D -> loadRec (context d) r i l f (d + 1) o
            O -> loadRec (context o <> maybeConvObj) r i l f d (o + 1)
            _ -> error "contextLoad: V"
          where context = contextLoad argRep
                argRep  = locArgRep cgLoc
                ft      = locFt cgLoc
                maybeStoreLoc nextCode
                  | store     = storeLoc cgLoc nextCode
                  | otherwise = nextCode
                loadRec nextCode = go (code <> maybeStoreLoc nextCode) cgLocs
                maybeConvInt
                  | convert   = gconv jint ft
                  | otherwise = mempty
                maybeConvObj
                  | convert   = gconv jobject ft
                  | otherwise = mempty
        go !code _ _ _ _ _ _ _ = code

mkCallExit :: Bool -> [(ArgRep, Maybe FieldType, Maybe Code)] -> Code
mkCallExit slow args' = storeArgs mempty args' rStart 1 1 1 1 1
  where rStart = if slow then 1 else 2
        storeArgs !code ((argRep, _ft', code'):args) !r !i !l !f !d !o =
          case argRep of
            P -> storeRec (context r) (r + 1) i l f d o
            N -> storeRec (context i) r (i + 1) l f d o
            L -> storeRec (context l) r i (l + 1) f d o
            F -> storeRec (context f) r i l (f + 1) d o
            D -> storeRec (context d) r i l f (d + 1) o
            O -> storeRec (context o) r i l f d (o + 1)
            V -> storeArgs code args r i l f d o
          where loadCode = expectJust "mkCallExit:loadCode" code'
                context = contextStore argRep loadCode
                storeRec nextCode =
                  storeArgs (code <> nextCode) args
        storeArgs !code _ _ _ _ _ _ _ = code

-- Helper function for generating return entry/exit layout
findFirstR :: [CgLoc] -> (Maybe CgLoc, [CgLoc])
findFirstR = go []
  where go locs []              = (Nothing, reverse locs)
        go locs (cgLoc:cgLocs)
          | locArgRep cgLoc == P = (Just cgLoc, reverse locs ++ cgLocs)
          | otherwise            = go (cgLoc:locs) cgLocs

-- This method is extremely sensitive. It assumes that the returned closure is
-- on the top of the stack
mkReturnEntry :: [CgLoc] -> Code
mkReturnEntry cgLocs' =
     maybe (pop closureType) (flip storeLoc mempty) mR1
  <> loadVals mempty cgLocs'' 2 1 1 1 1 1
  where (mR1, cgLocs'') = findFirstR cgLocs'
        loadVals !code (cgLoc:cgLocs) !r !i !l !f !d !o =
          case argRep of
            P -> loadRec (context r) (r + 1) i l f d o
            N -> loadRec (context i <> gconv jint ft) r (i + 1) l f d o
            L -> loadRec (context l) r i (l + 1) f d o
            F -> loadRec (context f) r i l (f + 1) d o
            D -> loadRec (context d) r i l f (d + 1) o
            O -> loadRec (context o <> gconv jobject ft) r i l f d (o + 1)
            _ -> error "contextLoad: V"
          where ft = locFt cgLoc
                argRep = locArgRep cgLoc
                context = contextLoad argRep
                loadRec nextCode =
                  loadVals (code <> storeLoc cgLoc nextCode) cgLocs
        loadVals !code _ _ _ _ _ _ _ = code

-- This method assumes that the bytecode after this code expects a closure at the
-- top of the stack
mkReturnExit :: [CgLoc] -> Code
mkReturnExit cgLocs' = storeVals mempty cgLocs'' 2 1 1 1 1 1
                    <> maybe (aconst_null closureType) loadLoc mR1
  where (mR1, cgLocs'') = findFirstR cgLocs'
        storeVals !code (cgLoc:cgLocs) !r !i !l !f !d !o =
          case argRep of
            P -> storeRec (context r) (r + 1) i l f d o
            N -> storeRec (context i) r (i + 1) l f d o
            L -> storeRec (context l) r i (l + 1) f d o
            F -> storeRec (context f) r i l (f + 1) d o
            D -> storeRec (context d) r i l f (d + 1) o
            O -> storeRec (context o) r i l f d (o + 1)
            _ -> error "contextLoad: V"
          where loadCode = loadLoc cgLoc
                argRep = locArgRep cgLoc
                context = contextStore argRep loadCode
                storeRec nextCode =
                  storeVals (code <> nextCode) cgLocs
        storeVals !code _ _ _ _ _ _ _ = code

slowCall :: DynFlags -> CgLoc -> [(ArgRep, Maybe FieldType, Maybe Code)] -> Code
slowCall dflags fun argFtCodes
    -- TODO: Implement optimization
    --       effectively an evaluation test + fast call
  | n > arity && optLevel dflags >= 2 = slowCode
  | otherwise = slowCode
  where n            = length argFtCodes
        ft           = locFt fun
        code         = loadLoc fun
        (arity, fts) = slowCallPattern $ map (\(a,_,_) -> a) argFtCodes
        slowCode     = directCall' True True (mkApFast arity (contextType:fts)) arity
                                   ((P, Just ft, Just code):argFtCodes)

directCall :: Bool -> CgLoc -> RepArity -> [(ArgRep, Maybe FieldType, Maybe Code)] -> Code
directCall slow fun arity argFtCodes
  | arity' == arity =
    directCall' True True (mkApFast arity' (contextType:fts)) arity'
      ((P, Just ft, Just code):argFtCodes)
  | otherwise = directCall' slow False entryCode arity argFtCodes
  where (arity', fts) = slowCallPattern $ map (\(a,_,_) -> a) argFtCodes
        code         = loadLoc fun
        ft           = locFt fun
        entryCode    = enterMethod fun

directCall' :: Bool -> Bool -> Code -> RepArity -> [(ArgRep, Maybe FieldType, Maybe Code)] -> Code
directCall' slow directLoad entryCode arity args =
     node
  <> loadArgs
  <> entryCode
  <> applyCode
  where (callArgs, restArgs) = splitAt realArity args
        realArity
          | slow      = arity + 1
          | otherwise = arity
        realCallArgs
          | slow      = drop 1 callArgs
          | otherwise = callArgs
        third (_,_,a) = a
        (node, loadArgs)
          | directLoad = (fromMaybe mempty . third $ head callArgs,
                          loadContext <> fold (catMaybes $ map third realCallArgs))
          | otherwise  = (mempty, mkCallExit slow callArgs)
        applyCalls = genApplyCalls restArgs
        applyCode
          | null applyCalls = mempty
          | otherwise       = fold applyCalls

genApplyCalls :: [(ArgRep, Maybe FieldType, Maybe Code)] -> [Code]
genApplyCalls []   = []
genApplyCalls args = applyCall : genApplyCalls restArgs
  where (n, fts)     = slowCallPattern $ map (\(a,_,_) -> a) args
        (callArgs, restArgs) = splitAt n args
        applyCall            = genApplyCall n fts callArgs

genApplyCall :: Int -> [FieldType] -> [(ArgRep, Maybe FieldType, Maybe Code)] -> Code
genApplyCall arity fts args =
     loadContext
  <> fold loadCodes
  <> mkApFast arity (contextType:fts)
  where loadCodes = mapMaybe (\(_, _, a) -> a) args

getRepFtCodes :: [StgArg] -> CodeGen [(ArgRep, Maybe FieldType, Maybe Code)]
getRepFtCodes = mapM getFtAmode
  where getFtAmode arg
          | Nothing <- ft = return (V, Nothing, Nothing)
          | otherwise = do code <- getArgLoadCode (NonVoid arg)
                           return (rep, ft, Just code)
          where ty = stgArgType arg
                ft = repFieldType_maybe ty
                rep = typeArgRep ty

newUnboxedTupleLocs :: Type -> CodeGen [CgLoc]
newUnboxedTupleLocs resType = getSequel >>= chooseLocs
  where chooseLocs (AssignTo regs) = return regs
        chooseLocs _               = mapM (\rep -> newTemp (isGcPtrRep rep)
                                                           (primRepFieldType rep))
                                   $ getUnboxedResultReps resType

getUnboxedResultReps :: Type -> [PrimRep]
getUnboxedResultReps resType = [ rep
                               | ty <- tyArgs
                               , let rep = typePrimRep ty
                               , not (isVoidRep rep) ]
  where tyArgs = case repType resType of
          UbxTupleRep tys -> tys
          UnaryRep    ty  -> [ty]

withContinuation :: Code -> CodeGen ()
withContinuation code = do
  sequel <- getSequel
  emit code
  emit $ case sequel of
           AssignTo cgLocs -> mkReturnEntry cgLocs
           _               -> greturn closureType

argLocsFrom :: Int -> [NonVoid Id] -> [CgLoc]
argLocsFrom startLocal args =
  reverse $ snd $ foldl' (\(!n, rest) arg ->
                            let argLoc = mkLocArg arg n
                            in (n + fieldSize (locFt argLoc), argLoc : rest))
                         (startLocal, []) args
