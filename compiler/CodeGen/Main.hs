module CodeGen.Main where

import JVM.Builder

import Module
import HscTypes
import TyCon
import StgSyn
import DynFlags

data CgEnv = CgEnv

data CgState = CgState

newtype CodeGen e a = CG { unCG :: CgEnv -> CgState -> GenerateIO e (CgState, a)}

doCodeGen :: HscEnv -> Module -> [TyCon] -> [StgBinding] -> HpcInfo -> IO ()
doCodeGen hsc_env this_mod data_tycons stg_binds hpc_info = return ()

codeGen :: DynFlags -> Module -> [TyCon] -> [StgBinding] -> HpcInfo -> IO ()
codeGen _ _ _ _ _ = return ()
  
mkModuleInit :: Module -> GenerateIO e ()
mkModuleInit _ = return ()
