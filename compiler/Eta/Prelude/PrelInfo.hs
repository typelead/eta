{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}
-}

{-# LANGUAGE CPP #-}
module Eta.Prelude.PrelInfo (
        -- * Known-key names
        isKnownKeyName,
        lookupKnownKeyName,
        -- ** Internal use
        -- | 'knownKeyNames' is exported to seed the original name cache only;
        -- if you find yourself wanting to look at it you might consider using
        -- 'lookupKnownKeyName' or 'isKnownKeyName'.
        knownKeyNames,

        -- * Miscellaneous
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

import Eta.Prelude.KnownUniques
import Eta.Prelude.THNames ( templateHaskellNames )
import Eta.Prelude.PrelNames
import Eta.Prelude.PrelRules
import Eta.BasicTypes.Avail
import Eta.BasicTypes.Unique
import Eta.Prelude.PrimOp
import Eta.BasicTypes.DataCon
import Eta.BasicTypes.Id
import Eta.BasicTypes.MkId
import Eta.Prelude.TysPrim
import Eta.Prelude.TysWiredIn
import Eta.Main.HscTypes
import Eta.Types.Class
import Eta.Types.TyCon
import Eta.Utils.Util
import Eta.Utils.UniqFM
import Eta.BasicTypes.NameEnv
import Eta.Utils.Outputable
-- import {-# SOURCE #-} Eta.TypeCheck.TcTypeNats ( typeNatTyCons )
import Eta.TypeCheck.TcTypeNats ( typeNatTyCons )
import Eta.BasicTypes.Name
import Data.Array
import Data.List
import Data.Maybe
import Control.Applicative ( (<|>) )

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
-- | This list is used to ensure that when you say "Prelude.map" in your source
-- code, or in an interface file, you get a Name with the correct known key (See
-- Note [Known-key names] in PrelNames)
knownKeyNames :: [Name]
knownKeyNames
  | debugIsOn
  , Just badNamesStr <- knownKeyNamesOkay all_names
  = panic ("badAllKnownKeyNames:\n" ++ badNamesStr)
       -- NB: We can't use ppr here, because this is sometimes evaluated in a
       -- context where there are no DynFlags available, leading to a cryptic
       -- "<<details unavailable>>" error. (This seems to happen only in the
       -- stage 2 compiler, for reasons I [Richard] have no clue of.)
  | otherwise
  = all_names
  where
    all_names =
      concat [ map getName wiredInThings
             , basicKnownKeyNames
             , templateHaskellNames
             ]
    -- All of the names associated with a wired-in TyCon.
    -- This includes the TyCon itself, its DataCons and promoted TyCons.

-- | Check the known-key names list of consistency.
knownKeyNamesOkay :: [Name] -> Maybe String
knownKeyNamesOkay all_names
  | ns@(_:_) <- filter (not . isValidKnownKeyUnique . getUnique) all_names
  = Just $ "    Out-of-range known-key uniques: ["
        ++ intercalate ", " (map (occNameString . nameOccName) ns) ++
         "]"
  | null badNamesPairs
  = Nothing
  | otherwise
  = Just badNamesStr
  where
    namesEnv      = foldl (\m n -> extendNameEnv_Acc (:) singleton m n n)
                          emptyUFM all_names
    badNamesEnv   = filterNameEnv (\ns -> ns `lengthExceeds` 1) namesEnv
    badNamesPairs = nonDetUFMToList badNamesEnv
      -- It's OK to use nonDetUFMToList here because the ordering only affects
      -- the message when we get a panic
    badNamesStrs  = map pairToStr badNamesPairs
    badNamesStr   = unlines badNamesStrs

    pairToStr (uniq, ns) = "        " ++
                           show uniq ++
                           ": [" ++
                           intercalate ", " (map (occNameString . nameOccName) ns) ++
                           "]"

-- | Given a 'Unique' lookup its associated 'Name' if it corresponds to a
-- known-key thing.
lookupKnownKeyName :: Unique -> Maybe Name
lookupKnownKeyName u =
    knownUniqueName u <|> lookupUFM knownKeysMap u

-- | Is a 'Name' known-key?
isKnownKeyName :: Name -> Bool
isKnownKeyName n =
    isJust (knownUniqueName $ nameUnique n) || elemUFM n knownKeysMap

knownKeysMap :: UniqFM Name
knownKeysMap = listToUFM [ (nameUnique n, n) | n <- knownKeyNames ]

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
             Export lists for pseudo-modules (GHC.Prim)
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
                          Built-in keys
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
                       Class predicates
*                                                                      *
************************************************************************
-}

isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys

is_elem :: Eq a => a -> [a] -> Bool
is_elem = isIn "is_X_Class"
