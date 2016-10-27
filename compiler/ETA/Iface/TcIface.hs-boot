module ETA.Iface.TcIface where

import ETA.Iface.IfaceSyn    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule, IfaceAnnotation )
import ETA.Types.TypeRep     ( TyThing )
import ETA.TypeCheck.TcRnTypes   ( IfL )
import ETA.Types.InstEnv     ( ClsInst )
import ETA.Types.FamInstEnv  ( FamInst )
import ETA.Core.CoreSyn     ( CoreRule )
import ETA.Main.HscTypes    ( TypeEnv, VectInfo, IfaceVectInfo )
import ETA.BasicTypes.Module      ( Module )
import ETA.Main.Annotations ( Annotation )

tcIfaceDecl        :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules       :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo    :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst        :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst     :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations :: [IfaceAnnotation] -> IfL [Annotation]