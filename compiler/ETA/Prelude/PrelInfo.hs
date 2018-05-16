{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}
-}

{-# LANGUAGE CPP #-}
module ETA.Prelude.PrelInfo (
        wiredInIds, ghcPrimIds,
        primOpRules, builtinRules, lookupKnownNameInfo,

        ghcPrimExports,
        wiredInThings, basicKnownKeyNames,
        primOpId,

        -- Random other things
        maybeCharLikeCon, maybeIntLikeCon,

        -- Class categories
        isNumericClass, isStandardClass

    ) where

#include "HsVersions.h"

import ETA.Prelude.PrelNames
import ETA.Prelude.PrelRules
import ETA.BasicTypes.Avail
import ETA.Prelude.PrimOp
import ETA.BasicTypes.DataCon
import ETA.BasicTypes.Id
import ETA.BasicTypes.MkId
import ETA.Prelude.TysPrim
import ETA.Prelude.TysWiredIn
import ETA.Main.HscTypes
import ETA.Types.Class
import ETA.Types.TyCon
import ETA.Utils.Util
import ETA.BasicTypes.NameEnv
import ETA.Utils.Outputable
-- import {-# SOURCE #-} ETA.TypeCheck.TcTypeNats ( typeNatTyCons )
import ETA.TypeCheck.TcTypeNats ( typeNatTyCons )
import ETA.BasicTypes.Name
import Data.Array

{-
************************************************************************
*                                                                      *
\subsection[builtinNameInfo]{Lookup built-in names}
*                                                                      *
************************************************************************

Notes about wired in things
~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Wired-in things are Ids\/TyCons that are completely known to the compiler.
  They are global values in GHC, (e.g.  listTyCon :: TyCon).

* A wired in Name contains the thing itself inside the Name:
        see Name.wiredInNameTyThing_maybe
  (E.g. listTyConName contains listTyCon.

* The name cache is initialised with (the names of) all wired-in things

* The type checker sees if the Name is wired in before looking up
  the name in the type environment.  So the type envt itself contains
  no wired in things.

* MkIface prunes out wired-in things before putting them in an interface file.
  So interface files never contain wired-in things.
-}

wiredInThings :: [TyThing]
-- This list is used only to initialise HscMain.knownKeyNames
-- to ensure that when you say "Prelude.map" in your source code, you
-- get a Name with the correct known key (See Note [Known-key names])
wiredInThings
  = concat
    [           -- Wired in TyCons and their implicit Ids
          tycon_things
        , concatMap implicitTyThings tycon_things

                -- Wired in Ids
        , map AnId wiredInIds

                -- PrimOps
        , map (AnId . primOpId) allThePrimOps
    ]
  where
    tycon_things = map ATyCon ([funTyCon] ++ primTyCons ++ wiredInTyCons
                                    ++ typeNatTyCons)

-- | Given a 'Unique' lookup any associated arbitrary SDoc's to be displayed by
-- GHCi's ':info' command.
lookupKnownNameInfo :: Name -> SDoc
lookupKnownNameInfo name = case lookupNameEnv knownNamesInfo name of
    -- If we do find a doc, we add comment delimeters to make the output
    -- of ':info' valid Haskell.
    Nothing  -> empty
    Just doc -> vcat [text "{-", doc, text "-}"]

-- A map from Uniques to SDocs, used in GHCi's ':info' command. (#12390)
knownNamesInfo :: NameEnv SDoc
knownNamesInfo = unitNameEnv coercibleTyConName $
    vcat [ text "Coercible is a special constraint with custom solving rules."
         , text "It is not a class."
         , text "Please see section 9.14.4 of the user's guide for details." ]


{-
We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.

************************************************************************
*                                                                      *
                PrimOpIds
*                                                                      *
************************************************************************
-}

primOpIds :: Array Int Id
-- A cache of the PrimOp Ids, indexed by PrimOp tag
primOpIds = array (1,maxPrimOpTag) [ (primOpTag op, mkPrimOpId op)
                                   | op <- allThePrimOps ]

primOpId :: PrimOp -> Id
primOpId op = primOpIds ! primOpTag op

{-
************************************************************************
*                                                                      *
\subsection{Export lists for pseudo-modules (GHC.Prim)}
*                                                                      *
************************************************************************

GHC.Prim "exports" all the primops and primitive types, some
wired-in Ids.
-}

ghcPrimExports :: [IfaceExport]
ghcPrimExports
 = map (Avail . idName) ghcPrimIds ++
   map (Avail . idName . primOpId) allThePrimOps ++
   [ AvailTC n [n]
   | tc <- funTyCon : primTyCons, let n = tyConName tc  ]

{-
************************************************************************
*                                                                      *
\subsection{Built-in keys}
*                                                                      *
************************************************************************

ToDo: make it do the ``like'' part properly (as in 0.26 and before).
-}

maybeCharLikeCon, maybeIntLikeCon :: DataCon -> Bool
maybeCharLikeCon con = con `hasKey` charDataConKey
maybeIntLikeCon  con = con `hasKey` intDataConKey

{-
************************************************************************
*                                                                      *
\subsection{Class predicates}
*                                                                      *
************************************************************************
-}

isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys

is_elem :: Eq a => a -> [a] -> Bool
is_elem = isIn "is_X_Class"
