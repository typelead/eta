module GHCVM.TypeCheck.TcEnv where
import GHCVM.TypeCheck.TcRnTypes

tcExtendIdEnv :: [TcId] -> TcM a -> TcM a