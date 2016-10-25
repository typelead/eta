module ETA.TypeCheck.TcEnv where
import ETA.TypeCheck.TcRnTypes

tcExtendIdEnv :: [TcId] -> TcM a -> TcM a