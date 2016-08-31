module GHCVM.CodeGen.LetNoEscape where

import Codec.JVM hiding (op)
import Codec.JVM.ASM.Code
import Codec.JVM.ASM.Code.Instr
import Codec.JVM.ASM.Code.Types
import Codec.JVM.Internal
import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.Opcode as OP

import Control.Monad.RWS
import Control.Arrow(second)
import Data.List(scanl')
import qualified Data.ByteString as BS

letNoEscapeCodeBlocks :: [(Label, Code)] -> Code -> Code
letNoEscapeCodeBlocks lneBinds expr
  = mkCode cs
  $ letNoEscapeBlocks (map (second instr) lneBinds) (instr expr)
  where cs = concatMap (consts . snd) lneBinds
          ++ consts expr

{-
This will generate code like:
goto expr
lne1:
   ..
   return
lne2:
   ..
   return
...
expr:
   ...
Current implemention runs the bytecodes in the Instr monad twice since
proper population of the LabelTable is required. Maybe a simpler implementation
with knot-tying semantics can be pursued in the future? Only if it helps
increase performance.
-}
letNoEscapeBlocks :: [(Label, Instr)] -> Instr -> Instr
letNoEscapeBlocks lneBinds expr = Instr $ do
  cp <- ask
  InstrState { isOffset = Offset baseOffset
             , isCtrlFlow = cf
             , isLabelTable = lt } <- get
  let firstOffset = baseOffset + lengthJump
      (offsets, labelOffsets) = unzip . tail $ scanl' (computeOffsets cf cp) (firstOffset, undefined) lneBinds
      defOffset = last offsets
      defInstr = expr
      (defBytes, _, _)
        = runInstrWithLabels' defInstr cp (Offset defOffset) cf lt
      breakOffset = defOffset + BS.length defBytes
      (_, instrs) = unzip lneBinds
  addLabels labelOffsets
  InstrState { isLabelTable = lt' } <- get
  writeGoto $ defOffset - baseOffset
  cfs <- forM (zip labelOffsets instrs) $ \((_, offset), instr) -> do
    writeStackMapFrame
    let (bytes', cf', frames') = runInstrWithLabels' instr cp offset cf lt'
    write bytes' frames'
    curOffset <- getOffset
    writeGoto $ breakOffset - curOffset
    return cf'

  let (defBytes', defCf', defFrames')
        = runInstrWithLabels' defInstr cp (Offset defOffset) cf lt'
  writeStackMapFrame
  write defBytes' defFrames'
  putCtrlFlow' $ CF.merge cf (defCf' : cfs)
  writeStackMapFrame
  where computeOffsets cf cp (offset, _) (label, instr) =
          ( offset + bytesLength + lengthJump
          , (label, Offset offset) )
          where (bytes, _, _) = runInstr' instr cp
                              $ emptyInstrState
                                { isOffset = Offset offset
                                , isCtrlFlow = cf }
                bytesLength = BS.length bytes
        lengthJump = 3 -- op goto <> pack16 $ length ko
        writeGoto offset = do
          op' OP.goto
          writeBytes . packI16 $ offset
