module Eta.Iface.MkJavaIface (mkJavaIface) where

import Eta.Main.HscTypes (ClassIndex, ModIface)
import Eta.TypeCheck.TcRnTypes (TcRnIf)

mkJavaIface :: ClassIndex -> String -> TcRnIf gbl lcl ModIface
