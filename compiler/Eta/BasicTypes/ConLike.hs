{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998

\section[ConLike]{@ConLike@: Constructor-like things}
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Eta.BasicTypes.ConLike (
        ConLike(..), conLikeIsInfix
    ) where

import {-# SOURCE #-} Eta.BasicTypes.DataCon (DataCon, dataConIsInfix)
import {-# SOURCE #-} Eta.BasicTypes.PatSyn  (PatSyn, patSynIsInfix)
import Eta.Utils.Outputable
import Eta.BasicTypes.Unique
import Eta.Utils.Util
import Eta.BasicTypes.Name
import Data.Function (on)
import qualified Data.Data as Data
import qualified Data.Typeable

{-
************************************************************************
*                                                                      *
\subsection{Constructor-like things}
*                                                                      *
************************************************************************
-}

-- | A constructor-like thing
data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn
  deriving Data.Typeable.Typeable

{-
************************************************************************
*                                                                      *
\subsection{Instances}
*                                                                      *
************************************************************************
-}

instance Eq ConLike where
    (==) = (==) `on` getUnique
    (/=) = (/=) `on` getUnique

instance Ord ConLike where
    (<=) = (<=) `on` getUnique
    (<) = (<) `on` getUnique
    (>=) = (>=) `on` getUnique
    (>) = (>) `on` getUnique
    compare = compare `on` getUnique

instance Uniquable ConLike where
    getUnique (RealDataCon dc) = getUnique dc
    getUnique (PatSynCon ps)   = getUnique ps

instance NamedThing ConLike where
    getName (RealDataCon dc) = getName dc
    getName (PatSynCon ps)   = getName ps

instance Outputable ConLike where
    ppr (RealDataCon dc) = ppr dc
    ppr (PatSynCon ps) = ppr ps

instance OutputableBndr ConLike where
    pprInfixOcc (RealDataCon dc) = pprInfixOcc dc
    pprInfixOcc (PatSynCon ps) = pprInfixOcc ps
    pprPrefixOcc (RealDataCon dc) = pprPrefixOcc dc
    pprPrefixOcc (PatSynCon ps) = pprPrefixOcc ps

instance Data.Data ConLike where
    -- don't traverse?
    toConstr _   = abstractConstr "ConLike"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "ConLike"

conLikeIsInfix :: ConLike -> Bool
conLikeIsInfix (RealDataCon dc) = dataConIsInfix dc
conLikeIsInfix (PatSynCon ps)   = patSynIsInfix  ps

-- -- | Returns the type of the whole pattern
-- conLikeResTy :: ConLike -> [Type] -> Type
-- conLikeResTy (RealDataCon con) tys = mkTyConApp (dataConTyCon con) tys
-- conLikeResTy (PatSynCon ps)    tys = patSynInstResTy ps tys
