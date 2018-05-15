module ETA.Rename.RnSplice where

import ETA.HsSyn.HsSyn
import ETA.TypeCheck.TcRnMonad
import ETA.BasicTypes.NameSet
import ETA.BasicTypes.RdrName
import ETA.BasicTypes.Name


rnSpliceType :: HsSplice RdrName   -> RnM (HsType Name, FreeVars)
rnSplicePat  :: HsSplice RdrName   -> RnM ( Either (Pat RdrName) (Pat Name)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl RdrName -> RnM (SpliceDecl Name, FreeVars)

rnTopSpliceDecls :: HsSplice RdrName -> RnM ([LHsDecl RdrName], FreeVars)
