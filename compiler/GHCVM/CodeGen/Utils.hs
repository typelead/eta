module GHCVM.CodeGen.Utils where

import Outputable
import Literal
import Codec.JVM

cgLit :: Literal -> Code
cgLit (MachStr s)           = mempty --
cgLit (MachChar   c)        = mempty -- CmmInt (fromIntegral (ord c)) (wordWidth dflags)
cgLit MachNullAddr          = aconst_null -- TODO: Is this correct?
cgLit (MachInt i)           = mempty -- CmmInt i (wordWidth dflags)
cgLit (MachWord i)          = mempty -- CmmInt i (wordWidth dflags)
cgLit (MachInt64 i)         = mempty -- CmmInt i W64
cgLit (MachWord64 i)        = mempty -- CmmInt i W64
cgLit (MachFloat r)         = mempty -- CmmFloat r W32
cgLit (MachDouble r)        = mempty -- CmmFloat r W64
-- TODO: Handle this later
cgLit (MachLabel fs ms fod) = error $ "cgLit: MachLabel"
cgLit other                 = pprPanic "mkSimpleLit" (ppr other)

