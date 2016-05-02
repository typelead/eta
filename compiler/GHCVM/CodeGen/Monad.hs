{-# LANGUAGE MultiParamTypeClasses #-}
module GHCVM.CodeGen.Monad (
  CgEnv(..),
  CgState(..),
  CodeGen(..),
  code)
where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import JVM.Builder

data CgEnv = CgEnv {
  cgPackagePrefix :: B.ByteString,
  cgFileName :: B.ByteString,
  cgClassName :: B.ByteString }

data CgState = CgState

newtype CodeGen e a = CG {
  unCG :: CgEnv -> CgState -> GenerateIO e (CgState, a) }

instance Functor (CodeGen e) where
  fmap = liftM

instance Applicative (CodeGen e) where
  pure = return
  (<*>) = ap

instance Monad (CodeGen e) where
  return x = CG $ \_ s -> return (s, x)
  m >>= f = CG $ \e s -> do
      (s0, x) <- unCG m e s
      unCG (f x) e s0

instance MonadState CgState (CodeGen e) where
  state action = CG $ \_ s -> do
    let (a, s') = action s
    return (s', a)

instance MonadReader CgEnv (CodeGen e) where
  ask = CG $ \env s -> return (s, env)
  local f action = CG $ \env s -> unCG action (f env) s

code :: GenerateIO e a -> CodeGen e a
code fc = CG $ \_ s -> do
                a <- fc
                return (s, a)
