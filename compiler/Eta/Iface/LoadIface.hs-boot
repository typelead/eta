module Eta.Iface.LoadIface where
import Eta.BasicTypes.Module (Module)
import Eta.TypeCheck.TcRnMonad (IfM)
import Eta.Main.HscTypes (ModIface)
import Eta.Utils.Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
