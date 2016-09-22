module GHCVM.Rename.RnExpr where
import GHCVM.HsSyn.HsSyn
import GHCVM.BasicTypes.Name       ( Name )
import GHCVM.BasicTypes.NameSet    ( FreeVars )
import GHCVM.BasicTypes.RdrName    ( RdrName )
import GHCVM.TypeCheck.TcRnTypes
import GHCVM.BasicTypes.SrcLoc     ( Located )
import GHCVM.Utils.Outputable ( Outputable )

rnLExpr :: LHsExpr RdrName
        -> RnM (LHsExpr Name, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body RdrName) => HsStmtContext Name
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> [LStmt RdrName (Located (body RdrName))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt Name (Located (body Name))], thing), FreeVars)
