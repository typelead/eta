module ETA.Rename.RnExpr where
import ETA.HsSyn.HsSyn
import ETA.BasicTypes.Name       ( Name )
import ETA.BasicTypes.NameSet    ( FreeVars )
import ETA.BasicTypes.RdrName    ( RdrName )
import ETA.TypeCheck.TcRnTypes
import ETA.BasicTypes.SrcLoc     ( Located )
import ETA.Utils.Outputable ( Outputable )

rnLExpr :: LHsExpr RdrName
        -> RnM (LHsExpr Name, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body RdrName) => HsStmtContext Name
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> [LStmt RdrName (Located (body RdrName))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt Name (Located (body Name))], thing), FreeVars)
