module GHCVM.Iface.TcIface where

import GHCVM.Iface.IfaceSyn    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule, IfaceAnnotation )
import GHCVM.Types.TypeRep     ( TyThing )
import GHCVM.TypeCheck.TcRnTypes   ( IfL )
import GHCVM.Types.InstEnv     ( ClsInst )
import GHCVM.Types.FamInstEnv  ( FamInst )
import GHCVM.Core.CoreSyn     ( CoreRule )
import GHCVM.Main.HscTypes    ( TypeEnv, VectInfo, IfaceVectInfo )
import GHCVM.BasicTypes.Module      ( Module )
import GHCVM.Main.Annotations ( Annotation )

tcIfaceDecl        :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules       :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo    :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst        :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst     :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations :: [IfaceAnnotation] -> IfL [Annotation]