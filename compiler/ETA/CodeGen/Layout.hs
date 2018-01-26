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
import ETA.Utils.Digraph
import ETA.Utils.Panic

import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Text (Text)

emitReturn :: [CgLoc] -> CodeGen ()
emitReturn results = do
  sequel <- getSequel
  loadContext <- getContextLoc
  case sequel of
      Return         -> emit $ mkReturnExit loadContext results <> greturn closureType
      AssignTo slots -> emitMultiAssign slots (map Right results)

emitAssign :: CgLoc -> Code -> CodeGen ()
emitAssign cgLoc code = emit $ storeLoc cgLoc code

-- this code is called after locs and codes are sorted topologically
multiAssign :: [CgLoc] -> [Code] -> Code
multiAssign locs codes = fold $ zipWith storeLoc locs codes

-- generic way - might be used fod "any" code or known vars
emitMultiAssign  :: [CgLoc] -> [Either Code CgLoc] -> CodeGen ()
emitMultiAssign stores exprs =  (emit $ multiAssign (fst  codes) (snd codes) ) >> emitMultiAssignVars (fst  locs)  (snd locs)
--emitMultiAssign stores exprs =  (emit $ multiAssign stores exprs ) >> emitMultiAssignVars []  []
    where
        codes = codesOnly stores exprs
        locs = cgLocsOnly stores exprs
-- emitMultiAssign (storeHead:stores) (Left exprHead:exprs) = multiAssign storeHead
-- emitMultiAssign locs codes = fold $ zipWith storeLoc locs codes

emitMultiAssignVars  :: [CgLoc] -> [CgLoc] -> CodeGen ()
emitMultiAssignVars stores load = do
    unscramble $ makeStatements stores load

type Key  = Int

data Statement = Statement { from::CgLoc, to::CgLoc}

type Vrtx = (Key, Statement)

makeStatements::[CgLoc]->[CgLoc]->[Statement]
makeStatements [] [] = []
makeStatements (toHead:toTail) (fromHead:fromTail) =
    Statement { from = fromHead, to = toHead} : makeStatements toTail fromTail
makeStatements _ _ = panic "not matching stmts"

--  this is more or less close copy of algorithm used in GHC
unscramble ::[Statement] -> CodeGen ()
unscramble vertices = mapM_ do_component components
  where
        edges :: [ Node Key Vrtx ]
        edges = [ (vertex, key1, (edges_from stmt1))
                | vertex@(key1, stmt1) <- to_vertices vertices ]

        to_vertices::[Statement]->[Vrtx]
        to_vertices stmts = (\x -> (edge_to x, x)) <$> stmts

        edges_from :: Statement -> [Key]
        edges_from stmt1 = findVarId  $ from stmt1

        edge_to:: Statement -> Key
        edge_to stmt1 = head ( findVarId $ to stmt1 )

        components :: [SCC Vrtx]
        components = reverse $ stronglyConnCompFromEdgedVertices edges

        do_component :: SCC Vrtx -> CodeGen ()
        do_component (AcyclicSCC (_,stmt))  = mk_graph stmt
        do_component (CyclicSCC [])         = panic "do_component"
        do_component (CyclicSCC [(_,stmt)]) = mk_graph stmt

        do_component (CyclicSCC ((_,first_stmt) : rest)) = do
            u <- emitTemp $ to first_stmt
            let (to_tmp, from_tmp) = split u first_stmt
            mk_graph to_tmp
            unscramble $ snd <$> rest
            mk_graph from_tmp

        split :: CgLoc -> Statement -> (Statement, Statement)
        split uniq Statement{ from = f, to = t}
          = (Statement{from=f, to = uniq}, Statement{from = uniq, to =t})

        mk_graph :: Statement -> CodeGen ()
        mk_graph stmt = emitAssign (to stmt) code
            where code = loadLoc $ from stmt


emitTemp::CgLoc -> CodeGen CgLoc
emitTemp (LocLocal isClosure ft _) = newTemp isClosure ft
emitTemp _ = panic "not implemented"

codesOnly:: [CgLoc] -> [Either Code CgLoc] -> ([CgLoc], [Code])
codesOnly [] [] = ([], [])
codesOnly (storeHead:stores) (Left exprHead:exprs) = ( storeHead : fst tail, exprHead : snd tail)
    where tail = codesOnly stores exprs
codesOnly  (_:stores) (Right _:exprs) = codesOnly stores exprs
codesOnly _ _ = panic "unequal assigmnents"

-- so bad copy paste almost
cgLocsOnly:: [CgLoc] -> [Either Code CgLoc] -> ([CgLoc], [CgLoc])
cgLocsOnly [] [] = ([], [])
cgLocsOnly (storeHead:stores) (Right exprHead:exprs) = ( storeHead : fst tail, exprHead : snd tail)
    where tail = cgLocsOnly stores exprs
cgLocsOnly  (_:stores) (Left _:exprs) = cgLocsOnly stores exprs
cgLocsOnly _ _ = panic "unequal assigmnents"

findVarId::CgLoc->[Int]
findVarId (LocLocal _ _ x) = [x]
findVarId _ = []

-- TODO: There are a lot of bangs in this function. Verify that they do
--       indeed help.
mkCallEntry :: Code -> Bool -> Bool -> [CgLoc] -> Code
mkCallEntry loadContext convert store cgLocs = go mempty cgLocs 1 1 1 1 1 1
  where go !code (cgLoc:cgLocs) !r !i !l !f !d !o =
          case argRep of
            P -> loadRec (context r) (r + 1) i l f d o
            N -> loadRec (context i <> maybeConvInt) r (i + 1) l f d o
            L -> loadRec (context l) r i (l + 1) f d o
            F -> loadRec (context f) r i l (f + 1) d o
            D -> loadRec (context d) r i l f (d + 1) o
            O -> loadRec (context o <> maybeConvObj) r i l f d (o + 1)
            _ -> error "contextLoad: V"
          where context = contextLoad loadContext argRep
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

mkCallExit :: Code -> [(ArgRep, Maybe FieldType, Maybe Code)] -> Code
mkCallExit loadContext args' = storeArgs mempty args' 1 1 1 1 1 1
  where storeArgs !code ((argRep, _ft', code'):args) !r !i !l !f !d !o =
          case argRep of
            P -> storeRec (context r) (r + 1) i l f d o
            N -> storeRec (context i) r (i + 1) l f d o
            L -> storeRec (context l) r i (l + 1) f d o
            F -> storeRec (context f) r i l (f + 1) d o
            D -> storeRec (context d) r i l f (d + 1) o
            O -> storeRec (context o) r i l f d (o + 1)
            V -> storeArgs code args r i l f d o
          where loadCode = expectJust "mkCallExit:loadCode" code'
                context = contextStore loadContext argRep loadCode
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
mkReturnEntry :: Code -> [CgLoc] -> Code
mkReturnEntry loadContext cgLocs' =
     maybe (pop closureType) (flip storeLoc mempty) mR1
  <> loadVals mempty cgLocs'' 1 1 1 1 1 1
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
                context = contextLoad loadContext argRep
                loadRec nextCode =
                  loadVals (code <> storeLoc cgLoc nextCode) cgLocs
        loadVals !code _ _ _ _ _ _ _ = code

-- This method assumes that the bytecode after this code expects a closure at the
-- top of the stack
mkReturnExit :: Code -> [CgLoc] -> Code
mkReturnExit loadContext cgLocs' =
     storeVals mempty cgLocs'' 1 1 1 1 1 1
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
                context = contextStore loadContext argRep loadCode
                storeRec nextCode =
                  storeVals (code <> nextCode) cgLocs
        storeVals !code _ _ _ _ _ _ _ = code

slowCall :: DynFlags -> Code -> CgLoc -> [(ArgRep, Maybe FieldType, Maybe Code)] -> (Code, Maybe Code)
slowCall dflags loadContext fun argFtCodes
    -- TODO: Implement optimization
    --       effectively an evaluation test + fast call
  | n > arity && optLevel dflags >= 2 = slowCode
  | otherwise = slowCode
  where n            = length argFtCodes
        ft           = locFt fun
        code         = loadLoc fun
        realCls      = fromMaybe stgClosure $ locClass fun
        (arity, fts) = slowCallPattern $ map (\(a,_,_) -> a) argFtCodes
        slowCode     = directCall' loadContext True True realCls
                         (mkApFast arity realCls fts)
                         arity ((P, Just ft, Just code):argFtCodes)

directCall :: Code -> Type -> CgLoc -> RepArity -> [(ArgRep, Maybe FieldType, Maybe Code)] -> (Code, Maybe Code)
directCall loadContext funType fun arity argFtCodes
  | Just staticCode <- loadStaticMethod fun argFts
  = directCall' loadContext True True stgClosure
      staticCode arity ((P, Nothing, Nothing):argFtCodes)
  | arity' == arity =
    directCall' loadContext True True realCls (mkApFast arity' realCls fts) arity'
      ((P, Just ft, Just code):argFtCodes)
  | otherwise = directCall' loadContext False False realCls entryCode arity argFtCodes
  where (arity', fts) = slowCallPattern $ map (\(a,_,_) -> a) argFtCodes
        code         = loadLoc fun
        ft           = locFt fun
        entryCode    = enterMethod loadContext fun
        realCls      = fromMaybe stgClosure $ locClass fun
        argFts'      = catMaybes . take arity $ map (\(_,ft,_) -> ft) argFtCodes
        -- All this logic is to get around some weird issues with
        -- the java monad type inlining. Ideally, argFts = staticFts
        -- in all cases.
        argFts
          | numStaticFts == length argFts' = staticFts
          | otherwise = staticFts ++ drop numStaticFts argFts'
        numStaticFts = length staticFts
        staticFts = staticMethodFts funType arity

staticMethodFts :: Type -> Int -> [FieldType]
staticMethodFts funType n =
  catMaybes . map repFieldType_maybe . take n $ deepSplitFunTys funType
  where deepSplitFunTys ty
          | null args = []
          | otherwise = args ++ deepSplitFunTys res
          where rest = dropForAlls ty
                (args, res) = splitFunTys rest

directCall' :: Code -> Bool -> Bool -> Text -> Code -> RepArity -> [(ArgRep, Maybe FieldType, Maybe Code)] -> (Code, Maybe Code)
directCall' loadContext slow directLoad realCls entryCode arity args =
  (  node
  <> loadArgs
  <> entryCode
  <> applyCode
  , lastApply)
  where (callArgs, restArgs) = splitAt realArity args
        realArity
          | slow      = arity + 1
          | otherwise = arity
        realCallArgs
          | slow      = drop 1 callArgs
          | otherwise = callArgs
        third (_,_,a) = a
        (node, loadArgs)
          | directLoad = ((fromMaybe mempty . third $ head callArgs)
                         <> gconv closureType (obj realCls),
                          loadContext <> fold (catMaybes $ map third realCallArgs))
          | otherwise  = (mempty, mkCallExit loadContext callArgs)
        applyCalls = genApplyCalls loadContext restArgs
        applyCode
          | null applyCalls = mempty
          | otherwise       = fold $ init applyCalls
        lastApply
          | null applyCalls = Nothing
          | otherwise       = Just $ last applyCalls

genApplyCalls :: Code -> [(ArgRep, Maybe FieldType, Maybe Code)] -> [Code]
genApplyCalls _ []   = []
genApplyCalls loadContext args = applyCall : genApplyCalls loadContext restArgs
  where (n, fts)             = slowCallPattern $ map (\(a,_,_) -> a) args
        (callArgs, restArgs) = splitAt n args
        applyCall            = genApplyCall loadContext n fts callArgs

genApplyCall :: Code -> Int -> [FieldType] -> [(ArgRep, Maybe FieldType, Maybe Code)] -> Code
genApplyCall loadContext arity fts args =
     loadContext
  <> fold loadCodes
  <> mkApFast arity stgClosure fts
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

withContinuation :: Code -> Maybe Code -> CodeGen ()
withContinuation contCode mLastCode = do
  sequel      <- getSequel
  loadContext <- getContextLoc
  let shouldGenSavePoint
        | Return <- sequel = isJust mLastCode
        | otherwise = True
      genContLoc
        | Return <- sequel
        , isJust mLastCode
        = fmap Just $ newTemp True closureType
        | otherwise = return Nothing
      genTrampLoc
        | shouldGenSavePoint = fmap Just $ newTemp False jbool
        | otherwise          = return Nothing
      afterTrampoline mTrampLoc
        | shouldGenSavePoint = loadContext <> loadLoc (fromJust mTrampLoc)
                                           <> putTrampolineField
        | otherwise          = mempty
      beforeCode mTrampLoc mContLoc
        =  maybe mempty
             (flip storeLoc (loadContext <> getAndSetTrampolineMethod)) mTrampLoc
        <> maybe contCode (flip storeLoc contCode) mContLoc
      afterCode mTrampLoc mContLoc
        | AssignTo cgLocs <- sequel =
            fromMaybe mempty mLastCode
            <> mkReturnEntry loadContext cgLocs <> afterTrampoline mTrampLoc
        | otherwise =
            afterTrampoline mTrampLoc
         <> maybe mempty (loadLoc (fromJust mContLoc) <>) mLastCode
         <> greturn closureType
        | otherwise = greturn closureType
  mContLoc  <- genContLoc
  mTrampLoc <- genTrampLoc
  emit $ beforeCode mTrampLoc mContLoc <> afterCode mTrampLoc mContLoc

withPrimContinuation :: Type -> (Maybe FieldType -> Code) -> CodeGen ()
withPrimContinuation resultType retCode = do
  sequel <- getSequel
  loadContext <- getContextLoc
  emit (code sequel loadContext)
  where code sequel loadContext
          | numResults == 0
          = retCode Nothing
         <> (case sequel of
               AssignTo _ -> mempty
               _ -> mkReturnExit loadContext [] <> greturn closureType)
          | numResults == 1
          , let resFt   = primRepFieldType (head reps)
                resCode = retCode (Just resFt)
          = case sequel of
              AssignTo cgLocs
                | let cgLoc = head cgLocs -> storeLoc cgLoc resCode
              _ -> mkReturnExit loadContext
                     [mkLocDirect (isClosureFt resFt) (resFt, resCode)]
                <> greturn closureType
          | otherwise
          = retCode (Just closureType)
         <> (case sequel of
               AssignTo cgLocs -> mkReturnEntry loadContext cgLocs
               _               -> greturn closureType)
        reps = getUnboxedResultReps resultType
        numResults = length reps

argLocsFrom :: Bool -> Int -> [NonVoid Id] -> [CgLoc]
argLocsFrom mask startLocal args =
  reverse $ snd $ foldl' (\(!n, rest) arg ->
                            let argLoc = mkLocArg mask arg n
                            in (n + fieldSize (locFt argLoc), argLoc : rest))
                         (startLocal, []) args
