module Eta.Rename.RnSplice where

import Eta.HsSyn.HsSyn
import Eta.TypeCheck.TcRnMonad
import Eta.BasicTypes.NameSet
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.Name


rnSpliceType :: HsSplice RdrName   -> RnM (HsType Name, FreeVars)
rnSplicePat  :: HsSplice RdrName   -> RnM ( Either (Pat RdrName) (Pat Name)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl RdrName -> RnM (SpliceDecl Name, FreeVars)

rnTopSpliceDecls :: HsSplice RdrName -> RnM ([LHsDecl RdrName], FreeVars)
