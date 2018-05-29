module Eta.TypeCheck.TcEnv where
import Eta.TypeCheck.TcRnTypes

tcExtendIdEnv :: [TcId] -> TcM a -> TcM a