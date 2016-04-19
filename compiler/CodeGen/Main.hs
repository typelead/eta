module CodeGen.Main where

import JVM.Builder

import Module
import HscTypes
import TyCon
import StgSyn

data CgEnv = CgEnv

data CgState = CgState

newtype CodeGen e a = CG { unCG :: CgEnv -> CgState -> GenerateIO e a }

doCodeGen :: HscEnv -> Module -> [TyCon] -> [StgBinding] -> HpcInfo -> IO ()
doCodeGen hsc_env this_mod data_tycons stg_binds hpc_info = return ()
