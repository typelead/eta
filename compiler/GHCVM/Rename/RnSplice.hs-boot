module GHCVM.Rename.RnSplice where

import GHCVM.HsSyn.HsSyn
import GHCVM.TypeCheck.TcRnMonad
import GHCVM.BasicTypes.RdrName
import GHCVM.BasicTypes.Name
import GHCVM.BasicTypes.NameSet
import GHCVM.Types.Kind


rnSpliceType :: HsSplice RdrName   -> PostTc Name Kind
             -> RnM (HsType Name, FreeVars)
rnSplicePat  :: HsSplice RdrName   -> RnM ( Either (Pat RdrName) (Pat Name)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl RdrName -> RnM (SpliceDecl Name, FreeVars)
