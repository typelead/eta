module Eta.Iface.MkJavaIface (mkJavaIface) where

import Eta.Main.HscTypes (ModIface)
import Eta.TypeCheck.TcRnTypes (TcRnIf)

mkJavaIface :: String -> TcRnIf gbl lcl ModIface
