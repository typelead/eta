module Eta.Rename.RnHsDoc ( rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import Eta.TypeCheck.TcRnTypes
import Eta.HsSyn.HsSyn
import Eta.BasicTypes.SrcLoc

rnMbLHsDoc :: Maybe LHsDocString -> RnM (Maybe LHsDocString)
rnMbLHsDoc mb_doc = case mb_doc of
  Just doc -> do
    doc' <- rnLHsDoc doc
    return (Just doc')
  Nothing -> return Nothing

rnLHsDoc :: LHsDocString -> RnM LHsDocString
rnLHsDoc (L pos doc) = do
  doc' <- rnHsDoc doc
  return (L pos doc')

rnHsDoc :: HsDocString -> RnM HsDocString
rnHsDoc (HsDocString s) = return (HsDocString s)

