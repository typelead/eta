module Eta.Iface.TcIface where

import Eta.Iface.IfaceSyn    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule, IfaceAnnotation )
import Eta.Types.TypeRep     ( TyThing )
import Eta.TypeCheck.TcRnTypes   ( IfL )
import Eta.Types.InstEnv     ( ClsInst )
import Eta.Types.FamInstEnv  ( FamInst )
import Eta.Core.CoreSyn     ( CoreRule )
import Eta.Main.HscTypes    ( TypeEnv, VectInfo, IfaceVectInfo )
import Eta.BasicTypes.Module      ( Module )
import Eta.Main.Annotations ( Annotation )

tcIfaceDecl        :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules       :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo    :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst        :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst     :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations :: [IfaceAnnotation] -> IfL [Annotation]