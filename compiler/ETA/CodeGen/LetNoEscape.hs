module ETA.CodeGen.LetNoEscape where

import Codec.JVM hiding (op)
import Codec.JVM.ASM.Code
import Codec.JVM.ASM.Code.Instr
import Codec.JVM.ASM.Code.Types
import Codec.JVM.Internal
import qualified Codec.JVM.ASM.Code.CtrlFlow as CF
import qualified Codec.JVM.Opcode as OP

import Control.Monad.RWS
import Control.Monad.IO.Class
import Control.Arrow(second)
import Data.List(scanl')
import Data.Foldable(fold)
import qualified Data.ByteString as BS

letNoEscapeCodeBlocks :: Label -> [(Label, Code)] -> (Label, Code) -> Code
letNoEscapeCodeBlocks defaultLabel lneBinds exp@(exprLabel, expr)
  = mkCode cs
  $ letNoEscapeBlocks defaultLabel (map (second instr) lneBinds) (exprLabel, instr expr)
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
letNoEscapeBlocks :: Label -> [(Label, Instr)] -> (Label, Instr) -> Instr
letNoEscapeBlocks defaultLabel lneBinds (exprLabel, expr) = Instr $ do
  cp <- ask
  InstrState { isOffset = Offset baseOffset
             , isCtrlFlow = cf
             , isLabelTable = lt } <- get
  unInstr $ gotoLabel exprLabel
  cfsAndLts <- forM lneBinds $ \(l, i) -> do
    withCtrlFlowAndLabels cf lt (unInstr (putLabel l <> i <> condGoto defaultLabel))
  cfAndLt <- withCtrlFlowAndLabels cf lt (unInstr (putLabel exprLabel <> expr))
  unInstr $ putLabel defaultLabel
  let (cfs, lts) = unzip (cfAndLt : cfsAndLts)
  putCtrlFlow' $ CF.merge cf cfs
  mergeLabels lts
