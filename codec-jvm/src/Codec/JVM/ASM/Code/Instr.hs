{-# LANGUAGE GeneralizedNewtypeDeriving, UnboxedTuples, RecordWildCards, MultiParamTypeClasses, FlexibleContexts #-}
module Codec.JVM.ASM.Code.Instr where

import Control.Monad.State
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Monoid ((<>))
import Data.List(scanl')
import Data.Maybe(fromMaybe)
import Control.Monad(forM_)

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap

import Codec.JVM.ASM.Code.CtrlFlow (CtrlFlow, Stack)
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..), LabelTable(..))
import Codec.JVM.Const (Const)
import Codec.JVM.Internal (packI16, packI32)
import Codec.JVM.Opcode (Opcode, opcode)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Types (ReturnType, jint, Label(..), FieldType)

import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

data InstrState =
  InstrState { isByteCode      :: !ByteString
             , isStackMapTable :: StackMapTable
             , isOffset        :: !Offset
             , isCtrlFlow      :: CtrlFlow
             , isLabelTable    :: LabelTable }

newtype InstrM a = InstrM { runInstrM :: ConstPool -> InstrState -> (# a, InstrState #) }

newtype Instr = Instr { unInstr :: InstrM () }

instance Functor InstrM where
  fmap = liftM

instance Applicative InstrM where
  pure = return
  (<*>) = ap

instance Monad InstrM where
  return x = InstrM $ \_ s -> (# x, s #)
  (InstrM m) >>= f =
    InstrM $ \e s ->
      case m e s of
        (# x, s' #) ->
          case runInstrM (f x) e s' of
            (# x', s'' #) -> (# x', s'' #)

instance MonadState InstrState InstrM where
  get = InstrM $ \_ s -> (# s, s #)
  put s' = InstrM $ \_ s -> (# (), s' #)

instance MonadReader ConstPool InstrM where
  ask = InstrM $ \e s -> (# e, s #)

instance Monoid Instr where
  mempty = Instr $ return ()
  mappend (Instr rws0) (Instr rws1) = Instr $ do
    rws0
    rws1

instance Show Instr where
  show insr = "Instructions"

withOffset :: (Int -> Instr) -> Instr
withOffset f = Instr $ do
  InstrState { isOffset = Offset offset } <- get
  unInstr $ f offset

emptyInstrState :: InstrState
emptyInstrState =
  InstrState { isByteCode = mempty
             , isStackMapTable = mempty
             , isOffset = 0
             , isCtrlFlow = CF.empty
             , isLabelTable = mempty }

runInstr :: Instr -> ConstPool -> (ByteString, CtrlFlow, StackMapTable)
runInstr instr cp = runInstr' instr cp $ emptyInstrState

runInstr' :: Instr -> ConstPool -> InstrState -> (ByteString, CtrlFlow, StackMapTable)
runInstr' (Instr m) e s = case runInstrM m e s of
  (# (), InstrState{..} #) -> (isByteCode, isCtrlFlow, isStackMapTable)

runInstrWithLabels' :: Instr -> ConstPool -> Offset -> CtrlFlow -> LabelTable -> (ByteString, CtrlFlow, StackMapTable)
runInstrWithLabels' instr cp offset cf lt = runInstr' instr cp s
  where s = emptyInstrState { isOffset = offset
                            , isCtrlFlow = cf
                            , isLabelTable = lt }

modifyStack' :: (Stack -> Stack) -> InstrM ()
modifyStack' f = --do
  ctrlFlow' $ CF.mapStack f
  -- (a, cf, c) <- get
  -- traceShow ("modifyStack", cf) $ put (a, cf, c)

modifyStack :: (Stack -> Stack) -> Instr
modifyStack = Instr . modifyStack'

gbranch :: (FieldType -> Stack -> Stack)
        -> FieldType -> Opcode -> Instr -> Instr -> Instr
gbranch f ft oc ok ko = Instr $ do
  lengthOp <- writeInstr ifop
  InstrState { isCtrlFlow = cf } <- get
  branches cf lengthOp ok ko
  where ifop = op oc <> modifyStack (f ft)

-- TODO: This function fails for huge methods, must make it safe
--       when goto offset is outside of −32,768 to 32,767
--       which isn't likely to happen.
branches :: CtrlFlow -> Int -> Instr -> Instr -> InstrM ()
branches cf lengthOp ok ko = do
  (koBytes, koCF, koFrames) <- pad 2 ko -- packI16
  let hasGoto = ifLastGoto koBytes
      lengthJumpOK = if hasGoto then 0 else 3
  writeBytes . packI16 $ BS.length koBytes + lengthJumpOK + lengthOp + 2 -- packI16
  write koBytes koFrames
  (okBytes, okCF, okFrames) <- pad lengthJumpOK ok
  unless hasGoto $ do
    op' OP.goto
    writeBytes . packI16 $ BS.length okBytes + 3 -- op goto <> packI16 $ length ok
    -- TODO: Omit stackframes accordingly?
  writeStackMapFrame
  write okBytes okFrames
  putCtrlFlow' $ CF.merge cf [okCF, koCF]
  writeStackMapFrame
    where
      pad padding instr = do
        cp <- ask
        InstrState { isOffset = Offset offset
                   , isCtrlFlow = cf
                   , isLabelTable = lt } <- get
        return $ runInstrWithLabels' instr cp (Offset $ offset + padding) cf lt

bytes :: ByteString -> Instr
bytes = Instr . writeBytes

ix :: Const -> Instr
ix c = Instr $ do
  cp <- ask
  writeBytes . packI16 $ CP.ix $ CP.unsafeIndex c cp

op :: Opcode -> Instr
op = Instr . op'

op' :: Opcode -> InstrM ()
op' = writeBytes . BS.singleton . opcode

ctrlFlow' :: (CtrlFlow -> CtrlFlow) -> InstrM ()
ctrlFlow' f = modify' $ \s@InstrState { isCtrlFlow = cf }  -> s { isCtrlFlow = f cf }

ctrlFlow :: (CtrlFlow -> CtrlFlow) -> Instr
ctrlFlow = Instr . ctrlFlow'

initCtrl :: (CtrlFlow -> CtrlFlow) -> Instr
initCtrl f = Instr $ do
  unInstr $ ctrlFlow f
  writeStackMapFrame

putCtrlFlow :: CtrlFlow -> Instr
putCtrlFlow = Instr . putCtrlFlow'

putCtrlFlow' :: CtrlFlow -> InstrM ()
putCtrlFlow' cf = modify' $ \s -> s { isCtrlFlow = cf }

incOffset :: Int -> Instr
incOffset = Instr . incOffset'

incOffset' :: Int -> InstrM ()
incOffset' i =
  modify' $ \s@InstrState { isOffset = Offset off } ->
              s { isOffset = Offset $ off + i}

write :: ByteString -> StackMapTable -> InstrM ()
write bs smfs = do
  incOffset' $ BS.length bs
  modify' $ \s@InstrState { isByteCode = bs'
                          , isStackMapTable = smfs' } ->
    s { isByteCode = bs' <> bs
      , isStackMapTable = smfs' <> smfs }

writeBytes :: ByteString -> InstrM ()
writeBytes bs = write bs mempty

writeInstr :: Instr -> InstrM Int
writeInstr (Instr action) = do
  off0 <- getOffset
  action
  off1 <- getOffset
  return $ off1 - off0

writeStackMapFrame :: InstrM ()
writeStackMapFrame = do
  modify' $ \s@InstrState { isOffset = Offset offset
                          , isCtrlFlow = cf
                          , isStackMapTable = StackMapTable smfs } ->
    s { isStackMapTable = StackMapTable $ IntMap.insert offset cf smfs}

getOffset :: InstrM Int
getOffset = do
  Offset offset <- gets isOffset
  return offset

type BranchMap = IntMap.IntMap Instr

tableswitch :: Int -> Int -> BranchMap -> Maybe Instr -> Instr
tableswitch low high branchMap deflt = Instr $ do
  cp <- ask
  baseOffset <- getOffset
  writeInstr $ op OP.tableswitch
  modifyStack' $ CF.pop jint
  InstrState { isOffset = Offset offset
             , isCtrlFlow = cf
             , isLabelTable = lt } <- get
  --(Offset offset, cf, lt) <- get
  -- Align to 4-byte boundary
  let padding = 4 - (offset `mod` 4)
  writeBytes . BS.pack . replicate padding $ 0
  offset' <- getOffset
  let firstOffset = offset' + 4 * (3 + numBranches)
      (offsets, codeInfos) = unzip . tail $ scanl' (computeOffsets cf cp lt) (firstOffset, undefined) [low..high]
      defOffset = last offsets
      defInstr = fromMaybe mempty deflt
      (defBytes, defCF, defFrames)
        = runInstrWithLabels' defInstr cp (Offset defOffset) cf lt
      breakOffset = defOffset + BS.length defBytes
      relOffset x = x - baseOffset
  writeBytes . packI32 $ relOffset defOffset
  writeBytes . packI32 $ low
  writeBytes . packI32 $ high
  forM_ codeInfos $ \(offset, _, _, _, _) ->
    writeBytes . packI32 $ relOffset offset
  forM_ codeInfos $ \(offset, len, bytes, cf', frames) -> do
    writeStackMapFrame
    if len == 0 then do
      op' OP.goto
      writeBytes . packI16 $ (defOffset - offset)
    else do
      write bytes frames
      op' OP.goto
      writeBytes . packI16 $ (breakOffset - (offset + len))
  writeStackMapFrame
  write defBytes defFrames
  putCtrlFlow' $ CF.merge cf (defCF : map (\(_, _, _, cf', _) -> cf') codeInfos)
  writeStackMapFrame
  where computeOffsets cf cp lt (offset, _) i =
          ( offset + bytesLength + lengthJump
          , (offset, bytesLength, bytes, cf', frames) )
          where (bytes, cf', frames) = runInstrWithLabels' instr cp (Offset offset) cf lt
                instr = IntMap.findWithDefault mempty i branchMap
                bytesLength = BS.length bytes
        lengthJump = 3 -- op goto <> pack16 $ length ko
        numBranches = high - low + 1

lookupswitch :: BranchMap -> Maybe Instr -> Instr
lookupswitch branchMap deflt = Instr $ do
  cp <- ask
  baseOffset <- getOffset
  writeInstr $ op OP.lookupswitch
  modifyStack' $ CF.pop jint
  InstrState { isOffset = Offset offset
             , isCtrlFlow = cf
             , isLabelTable = lt } <- get
  --(Offset offset, cf, lt) <- get
  -- Align to 4-byte boundary
  let padding = 4 - (offset `mod` 4)
  writeBytes . BS.pack . replicate padding $ 0
  offset' <- getOffset
  let firstOffset = offset' + 4 * (2 + 2 * numBranches)
      (offsets, codeInfos) = unzip . tail $ scanl' (computeOffsets cf cp lt) (firstOffset, undefined) $ IntMap.toAscList branchMap
      defOffset = last offsets
      defInstr = fromMaybe mempty deflt
      (defBytes, defCF, defFrames) = runInstrWithLabels' defInstr cp (Offset defOffset) cf lt
      breakOffset = defOffset + BS.length defBytes
      relOffset x = x - baseOffset
  writeBytes . packI32 $ relOffset defOffset
  writeBytes . packI32 $ length codeInfos
  forM_ codeInfos $ \(offset, _, val, _, _, _) -> do
    writeBytes . packI32 $ val
    writeBytes . packI32 $ relOffset offset
  forM_ codeInfos $ \(offset, len, _, bytes, cf', frames) -> do
    writeStackMapFrame
    write bytes frames
    op' OP.goto
    writeBytes . packI16 $ (breakOffset - (offset + len))
  writeStackMapFrame
  write defBytes defFrames
  putCtrlFlow' $
    CF.merge cf (defCF : map (\(_, _, _, _, cf', _) -> cf') codeInfos)
  writeStackMapFrame
  where computeOffsets cf cp lt (offset, _) (val, instr) =
          ( offset + bytesLength + lengthJump
          , (offset, bytesLength, val, bytes, cf', frames) )
          where (bytes, cf', frames) = runInstrWithLabels' instr cp (Offset offset) cf lt
                bytesLength = BS.length bytes
        lengthJump = 3 -- op goto <> pack16 $ length ko
        numBranches = IntMap.size branchMap


lookupLabel :: Label -> InstrM Offset
lookupLabel (Label id)= do
  InstrState { isLabelTable = LabelTable table } <- get
  -- TODO: Find a better default.
  return $ IntMap.findWithDefault (Offset 0) id table

gotoLabel :: Label -> Instr
gotoLabel label = Instr $ do
  offset <- getOffset
  Offset labelOffset <- lookupLabel label
  op' OP.goto
  writeBytes . packI16 $ labelOffset - offset

putLabel :: Label -> Instr
putLabel (Label id) = Instr $
  modify' $ \s@InstrState { isLabelTable = LabelTable table
                          , isOffset = off } ->
              s { isLabelTable = LabelTable $ IntMap.insert id off table }

addLabels :: [(Label, Offset)] -> InstrM ()
addLabels labelOffsets = modify' f
  where f s@InstrState { isLabelTable = LabelTable table } =
          s { isLabelTable = LabelTable table' }
          where table' = IntMap.union (IntMap.fromList labels) table
        labels = map (\(Label l, o) -> (l, o)) labelOffsets

-- TODO: Account for goto_w
ifLastGoto :: ByteString -> Bool
ifLastGoto bs =
  if index >= 0
  then unsafeIndex bs index == opcode OP.goto
  else False
  where index = (BS.length bs - 1) - 2
