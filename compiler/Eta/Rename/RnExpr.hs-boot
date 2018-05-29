module Eta.Rename.RnExpr where
import Eta.HsSyn.HsSyn
import Eta.BasicTypes.Name       ( Name )
import Eta.BasicTypes.NameSet    ( FreeVars )
import Eta.BasicTypes.RdrName    ( RdrName )
import Eta.TypeCheck.TcRnTypes
import Eta.BasicTypes.SrcLoc     ( Located )
import Eta.Utils.Outputable ( Outputable )

rnLExpr :: LHsExpr RdrName
        -> RnM (LHsExpr Name, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body RdrName) => HsStmtContext Name
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> [LStmt RdrName (Located (body RdrName))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt Name (Located (body Name))], thing), FreeVars)
