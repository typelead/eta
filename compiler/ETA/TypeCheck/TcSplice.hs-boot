{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module ETA.TypeCheck.TcSplice where

import ETA.BasicTypes.Name
import ETA.HsSyn.HsExpr   ( PendingRnSplice )
import ETA.TypeCheck.TcRnTypes( TcM , SpliceType, TcId )
import ETA.TypeCheck.TcType   ( TcRhoType )
import ETA.Main.Annotations ( Annotation, CoreAnnTarget )
import ETA.BasicTypes.RdrName  ( RdrName )
import ETA.HsSyn.HsSyn      ( HsSplice, HsBracket, HsExpr, LHsExpr, LHsType, LPat,
                    LHsDecl, ThModFinalizers )
import qualified Language.Haskell.TH as TH

tcSpliceExpr :: HsSplice Name
             -> TcRhoType
             -> TcM (HsExpr TcId)

tcUntypedBracket :: HsExpr Name
                 -> HsBracket Name
                 -> [PendingRnSplice]
                 -> TcRhoType
                 -> TcM (HsExpr TcId)
tcTypedBracket :: HsExpr Name
               -> HsBracket Name
               -> TcRhoType
               -> TcM (HsExpr TcId)

runAnnotation     :: CoreAnnTarget -> LHsExpr Name -> TcM Annotation

tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr TcId) -> TcM (LHsExpr TcId)

runMetaE :: LHsExpr TcId -> TcM (LHsExpr RdrName)
runMetaP :: LHsExpr TcId -> TcM (LPat RdrName)
runMetaT :: LHsExpr TcId -> TcM (LHsType RdrName)
runMetaD :: LHsExpr TcId -> TcM [LHsDecl RdrName]

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
runQuasi :: TH.Q a -> TcM a
runRemoteModFinalizers :: ThModFinalizers -> TcM ()
finishTH :: TcM ()
