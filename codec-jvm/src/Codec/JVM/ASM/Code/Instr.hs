{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codec.JVM.ASM.Code.Instr where

import Control.Monad.Trans.RWS
import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.Monoid ((<>))

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap

import Codec.JVM.ASM.Code.CtrlFlow (CtrlFlow, Stack)
import Codec.JVM.ASM.Code.Types (Offset(..), StackMapTable(..))
import Codec.JVM.Attr (StackMapFrame(..), VerifType(..))
import Codec.JVM.Cond (Cond)
import Codec.JVM.Const (Const)
import Codec.JVM.Internal (packI16)
import Codec.JVM.Opcode (Opcode, opcode)
import Codec.JVM.ConstPool (ConstPool)
import Codec.JVM.Types (ReturnType, jInt)

import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.Cond as CD
import qualified Codec.JVM.ConstPool as CP
import qualified Codec.JVM.Opcode as OP

type InstrRWS a = (RWS ConstPool (ByteString, StackMapTable) (Offset, CtrlFlow) a)

newtype Instr = Instr (InstrRWS ())

instrRWS :: Instr -> InstrRWS ()
instrRWS (Instr irws) = irws

instance Monoid Instr where
  mempty = Instr $ return mempty
  mappend (Instr rws0) (Instr rws1) = Instr $ do
    rws0
    rws1

runInstr :: Instr -> ConstPool -> (ByteString, CtrlFlow, StackMapTable)
runInstr instr cp = runInstr' instr cp 0 CF.empty

runInstr' :: Instr -> ConstPool -> Offset -> CtrlFlow -> (ByteString, CtrlFlow, StackMapTable)
runInstr' (Instr instr) cp offset cf = f $ runRWS instr cp (offset, cf) where
  f (_, (_, cf'), (bs, smfs)) = (bs, cf', smfs)

iif :: Cond -> Instr -> Instr -> Instr
iif cond ok ko = Instr $ do
  lengthOp <- writeInstr ifop
  branches lengthOp
    where
      ifop = op oc <> (ctrlFlow $ CF.mapStack $ CF.pop jInt) where
        oc = case cond of
          CD.EQ -> OP.ifeq
          CD.NE -> OP.ifne
      branches :: Int -> InstrRWS ()
      branches lengthOp = do
        (_, cf) <- get
        (koBytes, koCF, koFrames) <- pad 2 ko -- packI16
        writeBytes . packI16 $ BS.length koBytes + lengthJumpOK + lengthOp + 2 -- packI16
        write koBytes koFrames
        (okBytes, okCF, okFrames) <- pad lengthJumpOK ok
        op' OP.goto
        writeBytes . packI16 $ BS.length okBytes + 3 -- op goto <> packI16 $ length ok
        writeStackMapFrame
        write okBytes okFrames
        putCtrlFlow $ okCF
          { CF.locals = IntMap.union (CF.locals okCF) (CF.locals koCF)
          , CF.stack  = (CF.stack okCF) { CF.stackMax = max (CF.stackMax $ CF.stack okCF) (CF.stackMax $ CF.stack koCF)} }
        writeStackMapFrame
          where
            pad padding instr = do
              cp <- ask
              (Offset offset, cf) <- get
              return $ runInstr' instr cp (Offset $ offset + padding) cf
            lengthKO = 0
            lengthJumpOK = 3 -- op goto <> pack16 $ length ko

bytes :: ByteString -> Instr
bytes = Instr . writeBytes

ix :: Const -> Instr
ix c = Instr $ do
  cp <- ask
  writeBytes . packI16 $ CP.ix $ CP.unsafeIndex c cp

op :: Opcode -> Instr
op = Instr . op'

op' :: Opcode -> InstrRWS ()
op' = writeBytes . BS.singleton . opcode

ctrlFlow :: (CtrlFlow -> CtrlFlow) -> Instr
ctrlFlow f = Instr $ state s where s (off, cf) = (mempty, (off, f cf))

putCtrlFlow :: CtrlFlow -> InstrRWS ()
putCtrlFlow cf = do
  (off, _) <- get
  put (off, cf)

incOffset :: Int -> Instr
incOffset = Instr . incOffset'

incOffset' :: Int -> InstrRWS ()
incOffset' i = state s where s (Offset off, cf) = (mempty, (Offset $ off + i, cf))

write :: ByteString -> StackMapTable-> InstrRWS ()
write bs smfs = do
  incOffset' $ BS.length bs
  tell (bs, smfs)

writeBytes :: ByteString -> InstrRWS ()
writeBytes bs = write bs mempty

writeInstr :: Instr -> InstrRWS Int
writeInstr (Instr action) = do
  (Offset off0, _) <- get
  action
  (Offset off1, _) <- get
  return (off1 - off0)

writeStackMapFrame :: InstrRWS ()
writeStackMapFrame = get >>= f where
  f (Offset offset, cf) = tell (mempty, StackMapTable $ IntMap.singleton offset cf)
