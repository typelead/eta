{-# LANGUAGE CPP #-}
module GHCVM.TypeCheck.TcSplice where
import GHCVM.HsSyn.HsSyn    ( HsSplice, HsBracket, HsQuasiQuote,
                  HsExpr, LHsType, LHsExpr, LPat, LHsDecl )
import GHCVM.HsSyn.HsExpr   ( PendingRnSplice )
import GHCVM.BasicTypes.Name     ( Name )
import GHCVM.BasicTypes.RdrName  ( RdrName )
import GHCVM.TypeCheck.TcRnTypes( TcM, TcId )
import GHCVM.TypeCheck.TcType   ( TcRhoType )
import GHCVM.Main.Annotations ( Annotation, CoreAnnTarget )

#ifdef GHCI
import GHCVM.BasicTypes.Id         ( Id )
import qualified Language.Haskell.TH as TH
import GHCVM.Utils.Outputable (SDoc)
import GHCVM.BasicTypes.SrcLoc     (SrcSpan)
#endif

tcSpliceExpr :: HsSplice Name
             -> TcRhoType
             -> TcM (HsExpr TcId)

tcUntypedBracket :: HsBracket Name
                 -> [PendingRnSplice]
                 -> TcRhoType
                 -> TcM (HsExpr TcId)
tcTypedBracket :: HsBracket Name
               -> TcRhoType
               -> TcM (HsExpr TcId)

runQuasiQuoteDecl :: HsQuasiQuote RdrName -> TcM [LHsDecl RdrName]
runQuasiQuoteExpr :: HsQuasiQuote RdrName -> TcM (LHsExpr RdrName)
runQuasiQuoteType :: HsQuasiQuote RdrName -> TcM (LHsType RdrName)
runQuasiQuotePat  :: HsQuasiQuote RdrName -> TcM (LPat RdrName)
runAnnotation     :: CoreAnnTarget -> LHsExpr Name -> TcM Annotation

#ifdef GHCI
tcTopSpliceExpr :: Bool -> TcM (LHsExpr Id) -> TcM (LHsExpr Id)

runMetaE :: LHsExpr Id -> TcM (LHsExpr RdrName)
runMetaP :: LHsExpr Id -> TcM (LPat RdrName)
runMetaT :: LHsExpr Id  -> TcM (LHsType RdrName)
runMetaD :: LHsExpr Id -> TcM [LHsDecl RdrName]

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
runQuasi :: TH.Q a -> TcM a

data SpliceInfo
  = SpliceInfo
    { spliceIsDeclaration :: Bool
    , spliceDescription   :: String
    , spliceLocation      :: Maybe SrcSpan
    , spliceSource        :: Maybe SDoc
    , spliceGenerated     :: SDoc
    }
traceSplice :: SpliceInfo -> TcM ()
#endif
