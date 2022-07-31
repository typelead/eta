{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PrelNames]{Definitions of prelude modules and names}


Nota Bene: all Names defined in here should come from the base package

 - ModuleNames for prelude modules,
        e.g.    pREL_BASE_Name :: ModuleName

 - Modules for prelude modules
        e.g.    pREL_Base :: Module

 - Uniques for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    intTyConKey :: Unique
                minusClassOpKey :: Unique

 - Names for Ids, DataCons, TyCons and Classes that the compiler
   "knows about" in some way
        e.g.    intTyConName :: Name
                minusName    :: Name
   One of these Names contains
        (a) the module and occurrence name of the thing
        (b) its Unique
   The may way the compiler "knows about" one of these things is
   where the type checker or desugarer needs to look it up. For
   example, when desugaring list comprehensions the desugarer
   needs to conjure up 'foldr'.  It does this by looking up
   foldrName in the environment.

 - RdrNames for Ids, DataCons etc that the compiler may emit into
   generated code (e.g. for deriving).  It's not necessary to know
   the uniques for these guys, only their names


Note [Known-key names]
~~~~~~~~~~~~~~~~~~~~~~
It is *very* important that the compiler gives wired-in things and
things with "known-key" names the correct Uniques wherever they
occur. We have to be careful about this in exactly two places:

  1. When we parse some source code, renaming the AST better yield an
     AST whose Names have the correct uniques

  2. When we read an interface file, the read-in gubbins better have
     the right uniques

This is accomplished through a combination of mechanisms:

  1. When parsing source code, the RdrName-decorated AST has some
     RdrNames which are Exact. These are wired-in RdrNames where the
     we could directly tell from the parsed syntax what Name to
     use. For example, when we parse a [] in a type we can just insert
     an Exact RdrName Name with the listTyConKey.

     Currently, I believe this is just an optimisation: it would be
     equally valid to just output Orig RdrNames that correctly record
     the module etc we expect the final Name to come from. However,
     were we to eliminate isBuiltInOcc_maybe it would become essential
     (see point 3).

  2. The knownKeyNames (which consist of the basicKnownKeyNames from
     the module, and those names reachable via the wired-in stuff from
     TysWiredIn) are used to initialise the "OrigNameCache" in
     IfaceEnv.  This initialization ensures that when the type checker
     or renamer (both of which use IfaceEnv) look up an original name
     (i.e. a pair of a Module and an OccName) for a known-key name
     they get the correct Unique.

     This is the most important mechanism for ensuring that known-key
     stuff gets the right Unique, and is why it is so important to
     place your known-key names in the appropriate lists.

 3. For "infinite families" of known-key names (i.e. tuples and sums), we
    have to be extra careful. Because there are an infinite number of
    these things, we cannot add them to the list of known-key names
    used to initialise the OrigNameCache. Instead, we have to
    rely on never having to look them up in that cache. See
    Note [Infinite families of known-key names] for details.

    Note [Infinite families of known-key names]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Infinite families of known-key things (e.g. tuples and sums) pose a tricky
  problem: we can't add them to the knownKeyNames finite map which we use to
  ensure that, e.g., a reference to (,) gets assigned the right unique (if this
  doesn't sound familiar see Note [Known-key names] above).

  We instead handle tuples and sums separately from the "vanilla" known-key
  things,
  a) The parser recognises them specially and generates an Exact Name (hence not
     looked up in the orig-name cache)


  b) The known infinite families of names are specially serialised by
     BinIface.putName, with that special treatment detected when we read back to
     ensure that we get back to the correct uniques. See Note [Symbol table
     representation of names] in BinIface and Note [How tuples work] in
     TysWiredIn.

Most of the infinite families cannot occur in source code, so mechanisms (a) and (b)
suffice to ensure that they always have the right Unique. In particular,
implicit param TyCon names, constraint tuples and Any TyCons cannot be mentioned
by the user. For those things that *can* appear in source programs,

  c) IfaceEnv.lookupOrigNameCache uses isBuiltInOcc_maybe to map built-in syntax
     directly onto the corresponding name, rather than trying to find it in the
     original-name cache.

     See also Note [Built-in syntax and the OrigNameCache]
-}

module Eta.Prelude.PrelNames (
        Unique, Uniquable(..), hasKey,  -- Re-exported for convenience

        -----------------------------------------------------------
        module Eta.Prelude.PrelNames,
                                -- A huge bunch of (a) Names,  e.g. intTyConName
                                --                 (b) Uniques e.g. intTyConKey
                                --                 (c) Groups of classes and types
                                --                 (d) miscellaneous things
                                -- So many that we export them all
    ) where

import Eta.BasicTypes.Module
import Eta.BasicTypes.OccName
import Eta.BasicTypes.RdrName
import Eta.BasicTypes.Unique
import Eta.BasicTypes.BasicTypes
import Eta.BasicTypes.Name
import Eta.BasicTypes.SrcLoc
import Eta.Utils.FastString
import {-# SOURCE #-} Eta.Prelude.KnownUniques ( mkTupleTyConUnique )
-- import Eta.Utils.Panic ( panic )

{-
************************************************************************
*                                                                      *
     allNameStrings
*                                                                      *
************************************************************************
-}

allNameStrings :: [String]
-- Infinite list of a,b,c...z, aa, ab, ac, ... etc
allNameStrings = [ c:cs | cs <- "" : allNameStrings, c <- ['a'..'z'] ]

{-
************************************************************************
*                                                                      *
\subsection{Local Names}
*                                                                      *
************************************************************************

This *local* name is used by the interactive stuff
-}

itName :: Integer -> Unique -> SrcSpan -> Name
itName no uniq loc = mkInternalName uniq (mkOccNameFS varName (fsLit ("it" ++ show no))) loc

-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: RdrName -> Name
mkUnboundName rdr_name = mkInternalName unboundKey (rdrNameOcc rdr_name) noSrcSpan

isUnboundName :: Name -> Bool
isUnboundName name = name `hasKey` unboundKey

{-
************************************************************************
*                                                                      *
\subsection{Known key Names}
*                                                                      *
************************************************************************

This section tells what the compiler knows about the association of
names with uniques.  These ones are the *non* wired-in ones.  The
wired in ones are defined in TysWiredIn etc.

The names for DPH can come from one of multiple backend packages. At the point where
'basicKnownKeyNames' is used, we don't know which backend it will be.  Hence, we list
the names for multiple backends.  That works out fine, although they use the same uniques,
as we are guaranteed to only load one backend; hence, only one of the different names
sharing a unique will be used.
-}

basicKnownKeyNames :: [Name]
basicKnownKeyNames
 = genericTyConNames
 ++ [   -- Type constructors (synonyms especially)
        ioTyConName, ioDataConName,
        runMainIOName,
        rationalTyConName,
        stringTyConName,
        ratioDataConName,
        ratioTyConName,
        integerTyConName,

        --  Classes.  *Must* include:
        --      classes that are grabbed by key (e.g., eqClassKey)
        --      classes in "Class.standardClassKeys" (quite a few)
        eqClassName,                    -- mentioned, derivable
        ordClassName,                   -- derivable
        boundedClassName,               -- derivable
        numClassName,                   -- mentioned, numeric
        enumClassName,                  -- derivable
        monadClassName,
        functorClassName,
        realClassName,                  -- numeric
        integralClassName,              -- numeric
        fractionalClassName,            -- numeric
        floatingClassName,              -- numeric
        realFracClassName,              -- numeric
        realFloatClassName,             -- numeric
        dataClassName,
        isStringClassName,
        applicativeClassName,
        alternativeClassName,
        foldableClassName,
        traversableClassName,

        -- Typeable
        typeableClassName,
        typeRepTyConName,
        mkTyConName,
        mkPolyTyConAppName,
        mkAppTyName,
        typeLitTypeRepName,


        -- Numeric stuff
        negateName, minusName, geName, eqName,

        -- Conversion functions
        fromRationalName, fromIntegerName,
        toIntegerName, toRationalName,
        fromIntegralName, realToFracName,

        -- String stuff
        fromStringName,

        -- Enum stuff
        enumFromName, enumFromThenName,
        enumFromThenToName, enumFromToName,

        -- Applicative/Alternative stuff
        pureAName,
        apAName,

        -- Monad stuff
        thenIOName, bindIOName, returnIOName, failIOName,
        failMName, bindMName, thenMName, returnMName,
        fmapName,
        joinMName,

        -- MonadRec stuff
        mfixName,

        -- Arrow stuff
        arrAName, composeAName, firstAName,
        appAName, choiceAName, loopAName,

        -- Ix stuff
        ixClassName,

        -- Show stuff
        showClassName,

        -- Read stuff
        readClassName,

        -- Stable pointers
        newStablePtrName,

        -- GHC Extensions
        groupWithName,

        -- Strings and lists
        unpackCStringName,
        unpackCStringFoldrName, unpackCStringUtf8Name,

        -- Overloaded lists
        isListClassName,
        fromListName,
        fromListNName,
        toListName,

        -- List operations
        concatName, filterName, mapName,
        zipName, foldrName, buildName, augmentName, appendName,

        -- FFI primitive types that are not wired-in.
        stablePtrTyConName, ptrTyConName, funPtrTyConName,
        int8TyConName, int16TyConName, int32TyConName, int64TyConName,
        word16TyConName, word32TyConName, word64TyConName,

        -- Others
        otherwiseIdName, inlineIdName,
        eqStringName, assertName, breakpointName, breakpointCondName,
        breakpointAutoName,  opaqueTyConName,
        assertErrorName, runSTRepName,
        printName, printRawName, fstName, sndName,

        -- Integer
        integerTyConName, mkIntegerName,
        integerToWord64Name, integerToInt64Name,
        word64ToIntegerName, int64ToIntegerName,
        plusIntegerName, timesIntegerName, smallIntegerName,
        wordToIntegerName,
        integerToWordName, integerToIntName, minusIntegerName,
        negateIntegerName, eqIntegerPrimName, neqIntegerPrimName,
        absIntegerName, signumIntegerName,
        leIntegerPrimName, gtIntegerPrimName, ltIntegerPrimName, geIntegerPrimName,
        compareIntegerName, quotRemIntegerName, divModIntegerName,
        quotIntegerName, remIntegerName, divIntegerName, modIntegerName,
        floatFromIntegerName, doubleFromIntegerName,
        encodeFloatIntegerName, encodeDoubleIntegerName,
        decodeDoubleIntegerName,
        gcdIntegerName, lcmIntegerName,
        andIntegerName, orIntegerName, xorIntegerName, complementIntegerName,
        shiftLIntegerName, shiftRIntegerName,

        -- Float/Double
        rationalToFloatName,
        rationalToDoubleName,

        -- MonadFix
        monadFixClassName, mfixName,

        -- Other classes
        randomClassName, randomGenClassName, monadPlusClassName,

        -- Type-level naturals
        knownNatClassName, knownSymbolClassName,

        -- Overloaded labels
       isLabelClassName,

        -- Source locations
        callStackDataConName, callStackTyConName, hasCallStackTyConName,
        emptyCallStackName, pushCallStackName,
        srcLocDataConName,

        -- Annotation type checking
        toAnnotationWrapperName

        -- The Ordering type
        , orderingTyConName, ltDataConName, eqDataConName, gtDataConName

        -- The SPEC type for SpecConstr
        , specTyConName

        -- The Either type
        , eitherTyConName, leftDataConName, rightDataConName

        -- Plugins
        , pluginTyConName

        -- Generics
        , genClassName, gen1ClassName
        , datatypeClassName, constructorClassName, selectorClassName

        -- Monad comprehensions
        , guardMName
        , liftMName
        , mzipName

        -- GHCi Sandbox
        , ghciIoClassName, ghciStepIoMName

        -- StaticPtr
        , staticPtrTyConName
        , staticPtrDataConName, staticPtrInfoDataConName

        -- Fingerprint
        , fingerprintDataConName

        -- Custom type errors
       , errorMessageTypeErrorFamName
       , typeErrorTextDataConName
       , typeErrorAppendDataConName
       , typeErrorVAppendDataConName
       , typeErrorShowTypeDataConName

        -- Integer (S#)
        , integerSDataConName
        -- ETA
        , javaTyConName
        , extendsFamTyConName
        , inheritsFamTyConName
        , javaDataConName
        , extendsClassName
        , superCastName
        , unsafeCastName
        , fmapJavaName
        , classClassName
        , objName
        , unobjName
        , fromJStringName
        , toJStringName
        , classIdentifierName
        , overloadableClassName
        , sobjectTyConName
        , sobjectDataConName
        , byteTyConName
        , shortTyConName
        , jcharTyConName
    ]

genericTyConNames :: [Name]
genericTyConNames = [
    v1TyConName, u1TyConName, par1TyConName, rec1TyConName,
    k1TyConName, m1TyConName, sumTyConName, prodTyConName,
    compTyConName, rTyConName, pTyConName, dTyConName,
    cTyConName, sTyConName, rec0TyConName, par0TyConName,
    d1TyConName, c1TyConName, s1TyConName, noSelTyConName,
    repTyConName, rep1TyConName
  ]

{-
************************************************************************
*                                                                      *
\subsection{Module names}
*                                                                      *
************************************************************************


--MetaHaskell Extension Add a new module here
-}

pRELUDE :: Module
pRELUDE         = mkBaseModule_ pRELUDE_NAME

gHC_PRIM, gHC_TYPES, gHC_GENERICS, gHC_MAGIC,
    gHC_CLASSES, gHC_BASE, gHC_ENUM, gHC_GHCI, gHC_CSTRING,
    gHC_SHOW, gHC_READ, gHC_NUM, gHC_MAYBE, gHC_INTEGER_TYPE, gHC_LIST,
    gHC_TUPLE, dATA_TUPLE, dATA_EITHER, dATA_STRING, dATA_FOLDABLE, dATA_TRAVERSABLE, dATA_MONOID,
    gHC_CONC, gHC_IO, gHC_IO_Exception,
    gHC_ST, gHC_ARR, gHC_STABLE, gHC_PTR, gHC_ERR, gHC_REAL,
    gHC_FLOAT, gHC_TOP_HANDLER, sYSTEM_IO, eTA_RTS, dYNAMIC,
    tYPEABLE, tYPEABLE_INTERNAL, gENERICS,
    rEAD_PREC, lEX, gHC_INT, gHC_WORD, mONAD, mONAD_FIX, mONAD_ZIP,
    aRROW, cONTROL_APPLICATIVE, gHC_DESUGAR, rANDOM, gHC_EXTS,
    cONTROL_EXCEPTION_BASE, gHC_TYPELITS, gHC_TYPENATS :: Module

gHC_PRIM        = mkPrimModule (fsLit "GHC.Prim")   -- Primitive types and values
gHC_TYPES       = mkPrimModule (fsLit "GHC.Types")
gHC_MAGIC       = mkPrimModule (fsLit "GHC.Magic")
gHC_CSTRING     = mkPrimModule (fsLit "GHC.CString")
gHC_CLASSES     = mkPrimModule (fsLit "GHC.Classes")

gHC_BASE        = mkBaseModule (fsLit "GHC.Base")
gHC_ENUM        = mkBaseModule (fsLit "GHC.Enum")
gHC_GHCI        = mkBaseModule (fsLit "GHC.GHCi")
gHC_SHOW        = mkBaseModule (fsLit "GHC.Show")
gHC_READ        = mkBaseModule (fsLit "GHC.Read")
gHC_NUM         = mkBaseModule (fsLit "GHC.Num")
gHC_MAYBE       = mkBaseModule (fsLit "GHC.Maybe")
gHC_INTEGER_TYPE= mkIntegerModule (fsLit "GHC.Integer.Type")
gHC_LIST        = mkBaseModule (fsLit "GHC.List")
gHC_TUPLE       = mkPrimModule (fsLit "GHC.Tuple")
dATA_TUPLE      = mkBaseModule (fsLit "Data.Tuple")
dATA_EITHER     = mkBaseModule (fsLit "Data.Either")
dATA_STRING     = mkBaseModule (fsLit "Data.String")
dATA_FOLDABLE   = mkBaseModule (fsLit "Data.Foldable")
dATA_TRAVERSABLE= mkBaseModule (fsLit "Data.Traversable")
dATA_MONOID     = mkBaseModule (fsLit "Data.Monoid")
gHC_CONC        = mkBaseModule (fsLit "GHC.Conc")
gHC_IO          = mkBaseModule (fsLit "GHC.IO")
gHC_IO_Exception = mkBaseModule (fsLit "GHC.IO.Exception")
gHC_ST          = mkBaseModule (fsLit "GHC.ST")
gHC_ARR         = mkBaseModule (fsLit "GHC.Arr")
gHC_STABLE      = mkBaseModule (fsLit "GHC.Stable")
gHC_PTR         = mkBaseModule (fsLit "GHC.Ptr")
gHC_ERR         = mkBaseModule (fsLit "GHC.Err")
gHC_REAL        = mkBaseModule (fsLit "GHC.Real")
gHC_FLOAT       = mkBaseModule (fsLit "GHC.Float")
gHC_TOP_HANDLER = mkBaseModule (fsLit "GHC.TopHandler")
sYSTEM_IO       = mkBaseModule (fsLit "System.IO")
eTA_RTS         = mkBaseModule (fsLit "Eta.RTS")
dYNAMIC         = mkBaseModule (fsLit "Data.Dynamic")
tYPEABLE        = mkBaseModule (fsLit "Data.Typeable")
tYPEABLE_INTERNAL = mkBaseModule (fsLit "Data.Typeable.Internal")
gENERICS        = mkBaseModule (fsLit "Data.Data")
rEAD_PREC       = mkBaseModule (fsLit "Text.ParserCombinators.ReadPrec")
lEX             = mkBaseModule (fsLit "Text.Read.Lex")
gHC_INT         = mkBaseModule (fsLit "GHC.Int")
gHC_WORD        = mkBaseModule (fsLit "GHC.Word")
mONAD           = mkBaseModule (fsLit "Control.Monad")
mONAD_FIX       = mkBaseModule (fsLit "Control.Monad.Fix")
mONAD_ZIP       = mkBaseModule (fsLit "Control.Monad.Zip")
aRROW           = mkBaseModule (fsLit "Control.Arrow")
cONTROL_APPLICATIVE = mkBaseModule (fsLit "Control.Applicative")
gHC_DESUGAR = mkBaseModule (fsLit "GHC.Desugar")
rANDOM          = mkBaseModule (fsLit "System.Random")
gHC_EXTS        = mkBaseModule (fsLit "GHC.Exts")
cONTROL_EXCEPTION_BASE = mkBaseModule (fsLit "Control.Exception.Base")
gHC_GENERICS    = mkBaseModule (fsLit "GHC.Generics")
gHC_TYPELITS    = mkBaseModule (fsLit "GHC.TypeLits")
gHC_TYPENATS    = mkBaseModule (fsLit "GHC.TypeNats")

gHC_PARR' :: Module
gHC_PARR' = mkBaseModule (fsLit "GHC.PArr")

gHC_SRCLOC :: Module
gHC_SRCLOC = mkBaseModule (fsLit "GHC.SrcLoc")

gHC_STACK :: Module
gHC_STACK = mkBaseModule (fsLit "GHC.Stack")

gHC_STACK_TYPES :: Module
gHC_STACK_TYPES = mkBaseModule (fsLit "GHC.Stack.Types")

gHC_STATICPTR :: Module
gHC_STATICPTR = mkBaseModule (fsLit "GHC.StaticPtr")

gHC_FINGERPRINT_TYPE :: Module
gHC_FINGERPRINT_TYPE = mkBaseModule (fsLit "GHC.Fingerprint.Type")

gHC_OVER_LABELS :: Module
gHC_OVER_LABELS = mkBaseModule (fsLit "GHC.OverloadedLabels")

jAVA_STRING, jAVA_UTILS, jAVA_PRIMITIVEBASE, eTA_INTEROP :: Module
jAVA_STRING = mkBaseModule (fsLit "Java.StringBase")
jAVA_UTILS  = mkBaseModule (fsLit "Java.Utils")
jAVA_PRIMITIVEBASE  = mkBaseModule (fsLit "Java.PrimitiveBase")
eTA_INTEROP = mkBaseModule (fsLit "Eta.Interop")

mAIN, rOOT_MAIN :: Module
mAIN            = mkMainModule_ mAIN_NAME
rOOT_MAIN       = mkMainModule (fsLit ":Main") -- Root module for initialisation

mkInteractiveModule :: Int -> Module
-- (mkInteractiveModule 9) makes module 'interactive:M9'
mkInteractiveModule n = mkModule interactiveUnitId (mkModuleName ("EtaRepl" ++ show n))

pRELUDE_NAME, mAIN_NAME :: ModuleName
pRELUDE_NAME   = mkModuleNameFS (fsLit "Prelude")
mAIN_NAME      = mkModuleNameFS (fsLit "Main")

dATA_ARRAY_PARALLEL_NAME, dATA_ARRAY_PARALLEL_PRIM_NAME :: ModuleName
dATA_ARRAY_PARALLEL_NAME      = mkModuleNameFS (fsLit "Data.Array.Parallel")
dATA_ARRAY_PARALLEL_PRIM_NAME = mkModuleNameFS (fsLit "Data.Array.Parallel.Prim")

mkPrimModule :: FastString -> Module
mkPrimModule m = mkModule primUnitId (mkModuleNameFS m)

mkIntegerModule :: FastString -> Module
mkIntegerModule m = mkModule integerUnitId (mkModuleNameFS m)

mkBaseModule :: FastString -> Module
mkBaseModule m = mkModule baseUnitId (mkModuleNameFS m)

mkBaseModule_ :: ModuleName -> Module
mkBaseModule_ m = mkModule baseUnitId m

mkThisGhcModule :: FastString -> Module
mkThisGhcModule m = mkModule thisGhcUnitId (mkModuleNameFS m)

mkThisGhcModule_ :: ModuleName -> Module
mkThisGhcModule_ m = mkModule thisGhcUnitId m

mkMainModule :: FastString -> Module
mkMainModule m = mkModule mainUnitId (mkModuleNameFS m)

mkMainModule_ :: ModuleName -> Module
mkMainModule_ m = mkModule mainUnitId m

{-
************************************************************************
*                                                                      *
\subsection{Constructing the names of tuples
*                                                                      *
************************************************************************
-}

mkTupleModule :: TupleSort -> Module
mkTupleModule BoxedTuple      = gHC_TUPLE
mkTupleModule ConstraintTuple = gHC_TUPLE
mkTupleModule UnboxedTuple    = gHC_PRIM

{-
************************************************************************
*                                                                      *
                        RdrNames
*                                                                      *
************************************************************************
-}

main_RDR_Unqual    :: RdrName
main_RDR_Unqual = mkUnqual varName (fsLit "main")
        -- We definitely don't want an Orig RdrName, because
        -- main might, in principle, be imported into module Main

forall_tv_RDR, dot_tv_RDR :: RdrName
forall_tv_RDR = mkUnqual tvName (fsLit "forall")
dot_tv_RDR    = mkUnqual tvName (fsLit ".")

eq_RDR, ge_RDR, ne_RDR, le_RDR, lt_RDR, gt_RDR, compare_RDR,
    ltTag_RDR, eqTag_RDR, gtTag_RDR :: RdrName
eq_RDR                  = nameRdrName eqName
ge_RDR                  = nameRdrName geName
ne_RDR                  = varQual_RDR  gHC_CLASSES (fsLit "/=")
le_RDR                  = varQual_RDR  gHC_CLASSES (fsLit "<=")
lt_RDR                  = varQual_RDR  gHC_CLASSES (fsLit "<")
gt_RDR                  = varQual_RDR  gHC_CLASSES (fsLit ">")
compare_RDR             = varQual_RDR  gHC_CLASSES (fsLit "compare")
ltTag_RDR               = dataQual_RDR gHC_TYPES (fsLit "LT")
eqTag_RDR               = dataQual_RDR gHC_TYPES (fsLit "EQ")
gtTag_RDR               = dataQual_RDR gHC_TYPES (fsLit "GT")

eqClass_RDR, numClass_RDR, ordClass_RDR, enumClass_RDR, monadClass_RDR
    :: RdrName
eqClass_RDR             = nameRdrName eqClassName
numClass_RDR            = nameRdrName numClassName
ordClass_RDR            = nameRdrName ordClassName
enumClass_RDR           = nameRdrName enumClassName
monadClass_RDR          = nameRdrName monadClassName

map_RDR, append_RDR :: RdrName
map_RDR                 = varQual_RDR gHC_BASE (fsLit "map")
append_RDR              = varQual_RDR gHC_BASE (fsLit "++")

foldr_RDR, build_RDR, returnM_RDR, bindM_RDR, failM_RDR :: RdrName
foldr_RDR               = nameRdrName foldrName
build_RDR               = nameRdrName buildName
returnM_RDR             = nameRdrName returnMName
bindM_RDR               = nameRdrName bindMName
failM_RDR               = nameRdrName failMName

left_RDR, right_RDR :: RdrName
left_RDR                = nameRdrName leftDataConName
right_RDR               = nameRdrName rightDataConName

fromEnum_RDR, toEnum_RDR :: RdrName
fromEnum_RDR            = varQual_RDR gHC_ENUM (fsLit "fromEnum")
toEnum_RDR              = varQual_RDR gHC_ENUM (fsLit "toEnum")

enumFrom_RDR, enumFromTo_RDR, enumFromThen_RDR, enumFromThenTo_RDR :: RdrName
enumFrom_RDR            = nameRdrName enumFromName
enumFromTo_RDR          = nameRdrName enumFromToName
enumFromThen_RDR        = nameRdrName enumFromThenName
enumFromThenTo_RDR      = nameRdrName enumFromThenToName

ratioDataCon_RDR, plusInteger_RDR, timesInteger_RDR :: RdrName
ratioDataCon_RDR        = nameRdrName ratioDataConName
plusInteger_RDR         = nameRdrName plusIntegerName
timesInteger_RDR        = nameRdrName timesIntegerName

ioDataCon_RDR :: RdrName
ioDataCon_RDR           = nameRdrName ioDataConName

eqString_RDR, unpackCString_RDR, unpackCStringFoldr_RDR,
    unpackCStringUtf8_RDR :: RdrName
eqString_RDR            = nameRdrName eqStringName
unpackCString_RDR       = nameRdrName unpackCStringName
unpackCStringFoldr_RDR  = nameRdrName unpackCStringFoldrName
unpackCStringUtf8_RDR   = nameRdrName unpackCStringUtf8Name

newStablePtr_RDR :: RdrName
newStablePtr_RDR        = nameRdrName newStablePtrName

bindIO_RDR, returnIO_RDR :: RdrName
bindIO_RDR              = nameRdrName bindIOName
returnIO_RDR            = nameRdrName returnIOName

fromInteger_RDR, fromRational_RDR, minus_RDR, times_RDR, plus_RDR :: RdrName
fromInteger_RDR         = nameRdrName fromIntegerName
fromRational_RDR        = nameRdrName fromRationalName
minus_RDR               = nameRdrName minusName
times_RDR               = varQual_RDR  gHC_NUM (fsLit "*")
plus_RDR                = varQual_RDR gHC_NUM (fsLit "+")

stringTy_RDR, fromString_RDR :: RdrName
stringTy_RDR            = tcQual_RDR gHC_BASE (fsLit "String")
fromString_RDR          = nameRdrName fromStringName

fromList_RDR, fromListN_RDR, toList_RDR :: RdrName
fromList_RDR = nameRdrName fromListName
fromListN_RDR = nameRdrName fromListNName
toList_RDR = nameRdrName toListName

compose_RDR :: RdrName
compose_RDR             = varQual_RDR gHC_BASE (fsLit ".")

not_RDR, getTag_RDR, succ_RDR, pred_RDR, minBound_RDR, maxBound_RDR,
    and_RDR, range_RDR, inRange_RDR, index_RDR,
    unsafeIndex_RDR, unsafeRangeSize_RDR :: RdrName
and_RDR                 = varQual_RDR gHC_CLASSES (fsLit "&&")
not_RDR                 = varQual_RDR gHC_CLASSES (fsLit "not")
getTag_RDR              = varQual_RDR gHC_BASE (fsLit "getTag")
succ_RDR                = varQual_RDR gHC_ENUM (fsLit "succ")
pred_RDR                = varQual_RDR gHC_ENUM (fsLit "pred")
minBound_RDR            = varQual_RDR gHC_ENUM (fsLit "minBound")
maxBound_RDR            = varQual_RDR gHC_ENUM (fsLit "maxBound")
range_RDR               = varQual_RDR gHC_ARR (fsLit "range")
inRange_RDR             = varQual_RDR gHC_ARR (fsLit "inRange")
index_RDR               = varQual_RDR gHC_ARR (fsLit "index")
unsafeIndex_RDR         = varQual_RDR gHC_ARR (fsLit "unsafeIndex")
unsafeRangeSize_RDR     = varQual_RDR gHC_ARR (fsLit "unsafeRangeSize")

readList_RDR, readListDefault_RDR, readListPrec_RDR, readListPrecDefault_RDR,
    readPrec_RDR, parens_RDR, choose_RDR, lexP_RDR, expectP_RDR :: RdrName
readList_RDR            = varQual_RDR gHC_READ (fsLit "readList")
readListDefault_RDR     = varQual_RDR gHC_READ (fsLit "readListDefault")
readListPrec_RDR        = varQual_RDR gHC_READ (fsLit "readListPrec")
readListPrecDefault_RDR = varQual_RDR gHC_READ (fsLit "readListPrecDefault")
readPrec_RDR            = varQual_RDR gHC_READ (fsLit "readPrec")
parens_RDR              = varQual_RDR gHC_READ (fsLit "parens")
choose_RDR              = varQual_RDR gHC_READ (fsLit "choose")
lexP_RDR                = varQual_RDR gHC_READ (fsLit "lexP")
expectP_RDR             = varQual_RDR gHC_READ (fsLit "expectP")

punc_RDR, ident_RDR, symbol_RDR :: RdrName
punc_RDR                = dataQual_RDR lEX (fsLit "Punc")
ident_RDR               = dataQual_RDR lEX (fsLit "Ident")
symbol_RDR              = dataQual_RDR lEX (fsLit "Symbol")

step_RDR, alt_RDR, reset_RDR, prec_RDR, pfail_RDR :: RdrName
step_RDR                = varQual_RDR  rEAD_PREC (fsLit "step")
alt_RDR                 = varQual_RDR  rEAD_PREC (fsLit "+++")
reset_RDR               = varQual_RDR  rEAD_PREC (fsLit "reset")
prec_RDR                = varQual_RDR  rEAD_PREC (fsLit "prec")
pfail_RDR               = varQual_RDR  rEAD_PREC (fsLit "pfail")

showList_RDR, showList___RDR, showsPrec_RDR, showString_RDR,
    showSpace_RDR, showParen_RDR :: RdrName
showList_RDR            = varQual_RDR gHC_SHOW (fsLit "showList")
showList___RDR          = varQual_RDR gHC_SHOW (fsLit "showList__")
showsPrec_RDR           = varQual_RDR gHC_SHOW (fsLit "showsPrec")
showString_RDR          = varQual_RDR gHC_SHOW (fsLit "showString")
showSpace_RDR           = varQual_RDR gHC_SHOW (fsLit "showSpace")
showParen_RDR           = varQual_RDR gHC_SHOW (fsLit "showParen")

typeRep_RDR, mkTyCon_RDR, mkTyConApp_RDR :: RdrName
typeRep_RDR       = varQual_RDR tYPEABLE_INTERNAL    (fsLit "typeRep#")
mkTyCon_RDR       = varQual_RDR tYPEABLE_INTERNAL    (fsLit "mkTyCon")
mkTyConApp_RDR    = varQual_RDR tYPEABLE_INTERNAL    (fsLit "mkTyConApp")

undefined_RDR :: RdrName
undefined_RDR = varQual_RDR gHC_ERR (fsLit "undefined")

error_RDR :: RdrName
error_RDR = varQual_RDR gHC_ERR (fsLit "error")

-- Generics (constructors and functions)
u1DataCon_RDR, par1DataCon_RDR, rec1DataCon_RDR,
  k1DataCon_RDR, m1DataCon_RDR, l1DataCon_RDR, r1DataCon_RDR,
  prodDataCon_RDR, comp1DataCon_RDR,
  unPar1_RDR, unRec1_RDR, unK1_RDR, unComp1_RDR,
  from_RDR, from1_RDR, to_RDR, to1_RDR,
  datatypeName_RDR, moduleName_RDR, isNewtypeName_RDR,
  conName_RDR, conFixity_RDR, conIsRecord_RDR,
  noArityDataCon_RDR, arityDataCon_RDR, selName_RDR,
  prefixDataCon_RDR, infixDataCon_RDR, leftAssocDataCon_RDR,
  rightAssocDataCon_RDR, notAssocDataCon_RDR :: RdrName

u1DataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "U1")
par1DataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "Par1")
rec1DataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "Rec1")
k1DataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "K1")
m1DataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "M1")

l1DataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "L1")
r1DataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "R1")

prodDataCon_RDR   = dataQual_RDR gHC_GENERICS (fsLit ":*:")
comp1DataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "Comp1")

unPar1_RDR  = varQual_RDR gHC_GENERICS (fsLit "unPar1")
unRec1_RDR  = varQual_RDR gHC_GENERICS (fsLit "unRec1")
unK1_RDR    = varQual_RDR gHC_GENERICS (fsLit "unK1")
unComp1_RDR = varQual_RDR gHC_GENERICS (fsLit "unComp1")

from_RDR  = varQual_RDR gHC_GENERICS (fsLit "from")
from1_RDR = varQual_RDR gHC_GENERICS (fsLit "from1")
to_RDR    = varQual_RDR gHC_GENERICS (fsLit "to")
to1_RDR   = varQual_RDR gHC_GENERICS (fsLit "to1")

datatypeName_RDR  = varQual_RDR gHC_GENERICS (fsLit "datatypeName")
moduleName_RDR    = varQual_RDR gHC_GENERICS (fsLit "moduleName")
isNewtypeName_RDR = varQual_RDR gHC_GENERICS (fsLit "isNewtype")
selName_RDR       = varQual_RDR gHC_GENERICS (fsLit "selName")
conName_RDR       = varQual_RDR gHC_GENERICS (fsLit "conName")
conFixity_RDR     = varQual_RDR gHC_GENERICS (fsLit "conFixity")
conIsRecord_RDR   = varQual_RDR gHC_GENERICS (fsLit "conIsRecord")

noArityDataCon_RDR    = dataQual_RDR gHC_GENERICS (fsLit "NoArity")
arityDataCon_RDR      = dataQual_RDR gHC_GENERICS (fsLit "Arity")
prefixDataCon_RDR     = dataQual_RDR gHC_GENERICS (fsLit "Prefix")
infixDataCon_RDR      = dataQual_RDR gHC_GENERICS (fsLit "Infix")
leftAssocDataCon_RDR  = dataQual_RDR gHC_GENERICS (fsLit "LeftAssociative")
rightAssocDataCon_RDR = dataQual_RDR gHC_GENERICS (fsLit "RightAssociative")
notAssocDataCon_RDR   = dataQual_RDR gHC_GENERICS (fsLit "NotAssociative")


fmap_RDR, pure_RDR, ap_RDR, foldable_foldr_RDR, foldMap_RDR,
    traverse_RDR, mempty_RDR, mappend_RDR :: RdrName
fmap_RDR                = varQual_RDR gHC_BASE (fsLit "fmap")
pure_RDR                = nameRdrName pureAName
ap_RDR                  = nameRdrName apAName
foldable_foldr_RDR      = varQual_RDR dATA_FOLDABLE       (fsLit "foldr")
foldMap_RDR             = varQual_RDR dATA_FOLDABLE       (fsLit "foldMap")
traverse_RDR            = varQual_RDR dATA_TRAVERSABLE    (fsLit "traverse")
mempty_RDR              = varQual_RDR gHC_BASE            (fsLit "mempty")
mappend_RDR             = varQual_RDR gHC_BASE            (fsLit "mappend")

----------------------
varQual_RDR, tcQual_RDR, clsQual_RDR, dataQual_RDR
    :: Module -> FastString -> RdrName
varQual_RDR  mod str = mkOrig mod (mkOccNameFS varName str)
tcQual_RDR   mod str = mkOrig mod (mkOccNameFS tcName str)
clsQual_RDR  mod str = mkOrig mod (mkOccNameFS clsName str)
dataQual_RDR mod str = mkOrig mod (mkOccNameFS dataName str)

{-
************************************************************************
*                                                                      *
\subsection{Known-key names}
*                                                                      *
************************************************************************

Many of these Names are not really "built in", but some parts of the
compiler (notably the deriving mechanism) need to mention their names,
and it's convenient to write them all down in one place.

--MetaHaskell Extension  add the constrs and the lower case case
-- guys as well (perhaps) e.g. see  trueDataConName     below
-}

wildCardName :: Name
wildCardName = mkSystemVarName wildCardKey (fsLit "wild")

runMainIOName :: Name
runMainIOName = varQual gHC_TOP_HANDLER (fsLit "runMainIO") runMainKey

orderingTyConName, ltDataConName, eqDataConName, gtDataConName :: Name
orderingTyConName = tcQual   gHC_TYPES (fsLit "Ordering") orderingTyConKey
ltDataConName     = conName gHC_TYPES (fsLit "LT") ltDataConKey
eqDataConName     = conName gHC_TYPES (fsLit "EQ") eqDataConKey
gtDataConName     = conName gHC_TYPES (fsLit "GT") gtDataConKey

specTyConName :: Name
specTyConName     = tcQual gHC_TYPES (fsLit "SPEC") specTyConKey

eitherTyConName, leftDataConName, rightDataConName :: Name
eitherTyConName   = tcQual  dATA_EITHER (fsLit "Either") eitherTyConKey
leftDataConName   = conName dATA_EITHER (fsLit "Left")   leftDataConKey
rightDataConName  = conName dATA_EITHER (fsLit "Right")  rightDataConKey

-- Generics (types)
v1TyConName, u1TyConName, par1TyConName, rec1TyConName,
  k1TyConName, m1TyConName, sumTyConName, prodTyConName,
  compTyConName, rTyConName, pTyConName, dTyConName,
  cTyConName, sTyConName, rec0TyConName, par0TyConName,
  d1TyConName, c1TyConName, s1TyConName, noSelTyConName,
  repTyConName, rep1TyConName :: Name

v1TyConName  = tcQual gHC_GENERICS (fsLit "V1") v1TyConKey
u1TyConName  = tcQual gHC_GENERICS (fsLit "U1") u1TyConKey
par1TyConName  = tcQual gHC_GENERICS (fsLit "Par1") par1TyConKey
rec1TyConName  = tcQual gHC_GENERICS (fsLit "Rec1") rec1TyConKey
k1TyConName  = tcQual gHC_GENERICS (fsLit "K1") k1TyConKey
m1TyConName  = tcQual gHC_GENERICS (fsLit "M1") m1TyConKey

sumTyConName    = tcQual gHC_GENERICS (fsLit ":+:") sumTyConKey
prodTyConName   = tcQual gHC_GENERICS (fsLit ":*:") prodTyConKey
compTyConName   = tcQual gHC_GENERICS (fsLit ":.:") compTyConKey

rTyConName  = tcQual gHC_GENERICS (fsLit "R") rTyConKey
pTyConName  = tcQual gHC_GENERICS (fsLit "P") pTyConKey
dTyConName  = tcQual gHC_GENERICS (fsLit "D") dTyConKey
cTyConName  = tcQual gHC_GENERICS (fsLit "C") cTyConKey
sTyConName  = tcQual gHC_GENERICS (fsLit "S") sTyConKey

rec0TyConName  = tcQual gHC_GENERICS (fsLit "Rec0") rec0TyConKey
par0TyConName  = tcQual gHC_GENERICS (fsLit "Par0") par0TyConKey
d1TyConName  = tcQual gHC_GENERICS (fsLit "D1") d1TyConKey
c1TyConName  = tcQual gHC_GENERICS (fsLit "C1") c1TyConKey
s1TyConName  = tcQual gHC_GENERICS (fsLit "S1") s1TyConKey
noSelTyConName = tcQual gHC_GENERICS (fsLit "NoSelector") noSelTyConKey

repTyConName  = tcQual gHC_GENERICS (fsLit "Rep")  repTyConKey
rep1TyConName = tcQual gHC_GENERICS (fsLit "Rep1") rep1TyConKey

-- Base strings Strings
unpackCStringName, unpackCStringFoldrName,
    unpackCStringUtf8Name, eqStringName, stringTyConName :: Name
unpackCStringName       = varQual gHC_CSTRING (fsLit "unpackCString#") unpackCStringIdKey
unpackCStringFoldrName  = varQual gHC_CSTRING (fsLit "unpackFoldrCString#") unpackCStringFoldrIdKey
unpackCStringUtf8Name   = varQual gHC_CSTRING (fsLit "unpackCStringUtf8#") unpackCStringUtf8IdKey
eqStringName            = varQual gHC_BASE (fsLit "eqString")  eqStringIdKey
stringTyConName         = tcQual  gHC_BASE (fsLit "String") stringTyConKey

-- The 'inline' function
inlineIdName :: Name
inlineIdName            = varQual gHC_MAGIC (fsLit "inline") inlineIdKey

-- Base classes (Eq, Ord, Functor)
fmapName, eqClassName, eqName, ordClassName, geName, functorClassName :: Name
eqClassName       = clsQual gHC_CLASSES (fsLit "Eq")      eqClassKey
eqName            = varQual gHC_CLASSES (fsLit "==")      eqClassOpKey
ordClassName      = clsQual gHC_CLASSES (fsLit "Ord")     ordClassKey
geName            = varQual gHC_CLASSES (fsLit ">=")      geClassOpKey
functorClassName  = clsQual gHC_BASE    (fsLit "Functor") functorClassKey
fmapName          = varQual gHC_BASE    (fsLit "fmap")    fmapClassOpKey

-- Class Monad
monadClassName, thenMName, bindMName, returnMName, failMName :: Name
monadClassName     = clsQual gHC_BASE (fsLit "Monad")  monadClassKey
thenMName          = varQual gHC_BASE (fsLit ">>")     thenMClassOpKey
bindMName          = varQual gHC_BASE (fsLit ">>=")    bindMClassOpKey
returnMName        = varQual gHC_BASE (fsLit "return") returnMClassOpKey
failMName          = varQual gHC_BASE (fsLit "fail")   failMClassOpKey

-- Classes (Applicative, Foldable, Traversable)
applicativeClassName, foldableClassName, traversableClassName :: Name
applicativeClassName  = clsQual  gHC_BASE            (fsLit "Applicative") applicativeClassKey
foldableClassName     = clsQual  dATA_FOLDABLE       (fsLit "Foldable")    foldableClassKey
traversableClassName  = clsQual  dATA_TRAVERSABLE    (fsLit "Traversable") traversableClassKey



-- AMP additions

joinMName,  apAName, pureAName, alternativeClassName :: Name
joinMName            = varQual gHC_BASE (fsLit "join")        joinMIdKey
apAName              = varQual gHC_BASE (fsLit "<*>")         apAClassOpKey
pureAName            = varQual gHC_BASE (fsLit "pure")        pureAClassOpKey
alternativeClassName = clsQual mONAD (fsLit "Alternative") alternativeClassKey

joinMIdKey, apAClassOpKey, pureAClassOpKey, alternativeClassKey :: Unique
joinMIdKey          = mkPreludeMiscIdUnique 750
apAClassOpKey       = mkPreludeMiscIdUnique 751 -- <*>
pureAClassOpKey     = mkPreludeMiscIdUnique 752
alternativeClassKey = mkPreludeMiscIdUnique 753


-- Functions for GHC extensions
groupWithName :: Name
groupWithName = varQual gHC_EXTS (fsLit "groupWith") groupWithIdKey

-- Random PrelBase functions
fromStringName, otherwiseIdName, foldrName, buildName, augmentName,
    mapName, appendName, assertName,
    breakpointName, breakpointCondName, breakpointAutoName,
    opaqueTyConName :: Name
fromStringName = varQual dATA_STRING (fsLit "fromString") fromStringClassOpKey
otherwiseIdName   = varQual gHC_BASE (fsLit "otherwise")  otherwiseIdKey
foldrName         = varQual gHC_BASE (fsLit "foldr")      foldrIdKey
buildName         = varQual gHC_BASE (fsLit "build")      buildIdKey
augmentName       = varQual gHC_BASE (fsLit "augment")    augmentIdKey
mapName           = varQual gHC_BASE (fsLit "map")        mapIdKey
appendName        = varQual gHC_BASE (fsLit "++")         appendIdKey
assertName        = varQual gHC_BASE (fsLit "assert")     assertIdKey
breakpointName    = varQual gHC_BASE (fsLit "breakpoint") breakpointIdKey
breakpointCondName= varQual gHC_BASE (fsLit "breakpointCond") breakpointCondIdKey
breakpointAutoName= varQual gHC_BASE (fsLit "breakpointAuto") breakpointAutoIdKey
opaqueTyConName   = tcQual  gHC_BASE (fsLit "Opaque")     opaqueTyConKey

breakpointJumpName :: Name
breakpointJumpName
    = mkInternalName
        breakpointJumpIdKey
        (mkOccNameFS varName (fsLit "breakpointJump"))
        noSrcSpan
breakpointCondJumpName :: Name
breakpointCondJumpName
    = mkInternalName
        breakpointCondJumpIdKey
        (mkOccNameFS varName (fsLit "breakpointCondJump"))
        noSrcSpan
breakpointAutoJumpName :: Name
breakpointAutoJumpName
    = mkInternalName
        breakpointAutoJumpIdKey
        (mkOccNameFS varName (fsLit "breakpointAutoJump"))
        noSrcSpan

-- PrelTup
fstName, sndName :: Name
fstName           = varQual dATA_TUPLE (fsLit "fst") fstIdKey
sndName           = varQual dATA_TUPLE (fsLit "snd") sndIdKey

-- Module GHC.Num
numClassName, fromIntegerName, minusName, negateName :: Name
numClassName      = clsQual gHC_NUM (fsLit "Num")         numClassKey
fromIntegerName   = varQual gHC_NUM (fsLit "fromInteger") fromIntegerClassOpKey
minusName         = varQual gHC_NUM (fsLit "-")           minusClassOpKey
negateName        = varQual gHC_NUM (fsLit "negate")      negateClassOpKey

integerTyConName, mkIntegerName, integerSDataConName,
    integerToWord64Name, integerToInt64Name,
    word64ToIntegerName, int64ToIntegerName,
    plusIntegerName, timesIntegerName, smallIntegerName,
    wordToIntegerName,
    integerToWordName, integerToIntName, minusIntegerName,
    negateIntegerName, eqIntegerPrimName, neqIntegerPrimName,
    absIntegerName, signumIntegerName,
    leIntegerPrimName, gtIntegerPrimName, ltIntegerPrimName, geIntegerPrimName,
    compareIntegerName, quotRemIntegerName, divModIntegerName,
    quotIntegerName, remIntegerName, divIntegerName, modIntegerName,
    floatFromIntegerName, doubleFromIntegerName,
    encodeFloatIntegerName, encodeDoubleIntegerName,
    decodeDoubleIntegerName,
    gcdIntegerName, lcmIntegerName,
    andIntegerName, orIntegerName, xorIntegerName, complementIntegerName,
    shiftLIntegerName, shiftRIntegerName :: Name
integerTyConName      = tcQual  gHC_INTEGER_TYPE (fsLit "Integer")           integerTyConKey
integerSDataConName   = conName gHC_INTEGER_TYPE (fsLit "S#")                   integerSDataConKey
mkIntegerName         = varQual gHC_INTEGER_TYPE (fsLit "mkInteger")         mkIntegerIdKey
integerToWord64Name   = varQual gHC_INTEGER_TYPE (fsLit "integerToWord64")   integerToWord64IdKey
integerToInt64Name    = varQual gHC_INTEGER_TYPE (fsLit "integerToInt64")    integerToInt64IdKey
word64ToIntegerName   = varQual gHC_INTEGER_TYPE (fsLit "word64ToInteger")   word64ToIntegerIdKey
int64ToIntegerName    = varQual gHC_INTEGER_TYPE (fsLit "int64ToInteger")    int64ToIntegerIdKey
plusIntegerName       = varQual gHC_INTEGER_TYPE (fsLit "plusInteger")       plusIntegerIdKey
timesIntegerName      = varQual gHC_INTEGER_TYPE (fsLit "timesInteger")      timesIntegerIdKey
smallIntegerName      = varQual gHC_INTEGER_TYPE (fsLit "smallInteger")      smallIntegerIdKey
wordToIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "wordToInteger")     wordToIntegerIdKey
integerToWordName     = varQual gHC_INTEGER_TYPE (fsLit "integerToWord")     integerToWordIdKey
integerToIntName      = varQual gHC_INTEGER_TYPE (fsLit "integerToInt")      integerToIntIdKey
minusIntegerName      = varQual gHC_INTEGER_TYPE (fsLit "minusInteger")      minusIntegerIdKey
negateIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "negateInteger")     negateIntegerIdKey
eqIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "eqInteger#")        eqIntegerPrimIdKey
neqIntegerPrimName    = varQual gHC_INTEGER_TYPE (fsLit "neqInteger#")       neqIntegerPrimIdKey
absIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "absInteger")        absIntegerIdKey
signumIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "signumInteger")     signumIntegerIdKey
leIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "leInteger#")        leIntegerPrimIdKey
gtIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "gtInteger#")        gtIntegerPrimIdKey
ltIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "ltInteger#")        ltIntegerPrimIdKey
geIntegerPrimName     = varQual gHC_INTEGER_TYPE (fsLit "geInteger#")        geIntegerPrimIdKey
compareIntegerName    = varQual gHC_INTEGER_TYPE (fsLit "compareInteger")    compareIntegerIdKey
quotRemIntegerName    = varQual gHC_INTEGER_TYPE (fsLit "quotRemInteger")    quotRemIntegerIdKey
divModIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "divModInteger")     divModIntegerIdKey
quotIntegerName       = varQual gHC_INTEGER_TYPE (fsLit "quotInteger")       quotIntegerIdKey
remIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "remInteger")        remIntegerIdKey
divIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "divInteger")        divIntegerIdKey
modIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "modInteger")        modIntegerIdKey
floatFromIntegerName  = varQual gHC_INTEGER_TYPE (fsLit "floatFromInteger")      floatFromIntegerIdKey
doubleFromIntegerName = varQual gHC_INTEGER_TYPE (fsLit "doubleFromInteger")     doubleFromIntegerIdKey
encodeFloatIntegerName  = varQual gHC_INTEGER_TYPE (fsLit "encodeFloatInteger")  encodeFloatIntegerIdKey
encodeDoubleIntegerName = varQual gHC_INTEGER_TYPE (fsLit "encodeDoubleInteger") encodeDoubleIntegerIdKey
decodeDoubleIntegerName = varQual gHC_INTEGER_TYPE (fsLit "decodeDoubleInteger") decodeDoubleIntegerIdKey
gcdIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "gcdInteger")        gcdIntegerIdKey
lcmIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "lcmInteger")        lcmIntegerIdKey
andIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "andInteger")        andIntegerIdKey
orIntegerName         = varQual gHC_INTEGER_TYPE (fsLit "orInteger")         orIntegerIdKey
xorIntegerName        = varQual gHC_INTEGER_TYPE (fsLit "xorInteger")        xorIntegerIdKey
complementIntegerName = varQual gHC_INTEGER_TYPE (fsLit "complementInteger") complementIntegerIdKey
shiftLIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "shiftLInteger")     shiftLIntegerIdKey
shiftRIntegerName     = varQual gHC_INTEGER_TYPE (fsLit "shiftRInteger")     shiftRIntegerIdKey

-- GHC.Real types and classes
rationalTyConName, ratioTyConName, ratioDataConName, realClassName,
    integralClassName, realFracClassName, fractionalClassName,
    fromRationalName, toIntegerName, toRationalName, fromIntegralName,
    realToFracName :: Name
rationalTyConName   = tcQual  gHC_REAL (fsLit "Rational")     rationalTyConKey
ratioTyConName      = tcQual  gHC_REAL (fsLit "Ratio")        ratioTyConKey
ratioDataConName    = conName gHC_REAL (fsLit ":%")           ratioDataConKey
realClassName       = clsQual gHC_REAL (fsLit "Real")         realClassKey
integralClassName   = clsQual gHC_REAL (fsLit "Integral")     integralClassKey
realFracClassName   = clsQual gHC_REAL (fsLit "RealFrac")     realFracClassKey
fractionalClassName = clsQual gHC_REAL (fsLit "Fractional")   fractionalClassKey
fromRationalName    = varQual gHC_REAL (fsLit "fromRational") fromRationalClassOpKey
toIntegerName       = varQual gHC_REAL (fsLit "toInteger")    toIntegerClassOpKey
toRationalName      = varQual gHC_REAL (fsLit "toRational")   toRationalClassOpKey
fromIntegralName    = varQual  gHC_REAL (fsLit "fromIntegral")fromIntegralIdKey
realToFracName      = varQual  gHC_REAL (fsLit "realToFrac")  realToFracIdKey

-- PrelFloat classes
floatingClassName, realFloatClassName :: Name
floatingClassName  = clsQual gHC_FLOAT (fsLit "Floating")  floatingClassKey
realFloatClassName = clsQual gHC_FLOAT (fsLit "RealFloat") realFloatClassKey

-- other GHC.Float functions
rationalToFloatName, rationalToDoubleName :: Name
rationalToFloatName  = varQual gHC_FLOAT (fsLit "rationalToFloat") rationalToFloatIdKey
rationalToDoubleName = varQual gHC_FLOAT (fsLit "rationalToDouble") rationalToDoubleIdKey

-- Class Ix
ixClassName :: Name
ixClassName = clsQual gHC_ARR (fsLit "Ix") ixClassKey

-- Class Typeable, and functions for constructing `Typeable` dictionaries
typeableClassName
  , typeRepTyConName
  , mkTyConName
  , mkPolyTyConAppName
  , mkAppTyName
  , typeLitTypeRepName
  :: Name
typeableClassName     = clsQual tYPEABLE_INTERNAL (fsLit "Typeable")       typeableClassKey
typeRepTyConName      = tcQual  tYPEABLE_INTERNAL (fsLit "TypeRep")        typeRepTyConKey
mkTyConName           = varQual tYPEABLE_INTERNAL (fsLit "mkTyCon")        mkTyConKey
mkPolyTyConAppName    = varQual tYPEABLE_INTERNAL (fsLit "mkPolyTyConApp") mkPolyTyConAppKey
mkAppTyName           = varQual tYPEABLE_INTERNAL (fsLit "mkAppTy")        mkAppTyKey
typeLitTypeRepName    = varQual tYPEABLE_INTERNAL (fsLit "typeLitTypeRep") typeLitTypeRepKey

-- Custom type errors
errorMessageTypeErrorFamName
  , typeErrorTextDataConName
  , typeErrorAppendDataConName
  , typeErrorVAppendDataConName
  , typeErrorShowTypeDataConName
  :: Name

errorMessageTypeErrorFamName =
  tcQual gHC_TYPELITS (fsLit "TypeError") errorMessageTypeErrorFamKey

typeErrorTextDataConName =
  dcQual gHC_TYPELITS (fsLit "Text") typeErrorTextDataConKey

typeErrorAppendDataConName =
  dcQual gHC_TYPELITS (fsLit ":<>:") typeErrorAppendDataConKey

typeErrorVAppendDataConName =
  dcQual gHC_TYPELITS (fsLit ":$$:") typeErrorVAppendDataConKey

typeErrorShowTypeDataConName =
  dcQual gHC_TYPELITS (fsLit "ShowType") typeErrorShowTypeDataConKey

-- Dynamic
toDynName :: Name
toDynName = varQual dYNAMIC (fsLit "toDyn") toDynIdKey

-- Class Data
dataClassName :: Name
dataClassName = clsQual gENERICS (fsLit "Data") dataClassKey

-- Error module
assertErrorName    :: Name
assertErrorName   = varQual gHC_IO_Exception (fsLit "assertError") assertErrorIdKey

-- Enum module (Enum, Bounded)
enumClassName, enumFromName, enumFromToName, enumFromThenName,
    enumFromThenToName, boundedClassName :: Name
enumClassName      = clsQual gHC_ENUM (fsLit "Enum")           enumClassKey
enumFromName       = varQual gHC_ENUM (fsLit "enumFrom")       enumFromClassOpKey
enumFromToName     = varQual gHC_ENUM (fsLit "enumFromTo")     enumFromToClassOpKey
enumFromThenName   = varQual gHC_ENUM (fsLit "enumFromThen")   enumFromThenClassOpKey
enumFromThenToName = varQual gHC_ENUM (fsLit "enumFromThenTo") enumFromThenToClassOpKey
boundedClassName   = clsQual gHC_ENUM (fsLit "Bounded")        boundedClassKey

-- List functions
concatName, filterName, zipName :: Name
concatName        = varQual gHC_LIST (fsLit "concat") concatIdKey
filterName        = varQual gHC_LIST (fsLit "filter") filterIdKey
zipName           = varQual gHC_LIST (fsLit "zip")    zipIdKey

-- Overloaded lists
isListClassName, fromListName, fromListNName, toListName :: Name
isListClassName = clsQual gHC_EXTS (fsLit "IsList")    isListClassKey
fromListName    = varQual gHC_EXTS (fsLit "fromList")  fromListClassOpKey
fromListNName   = varQual gHC_EXTS (fsLit "fromListN") fromListNClassOpKey
toListName      = varQual gHC_EXTS (fsLit "toList")    toListClassOpKey

-- Class Show
showClassName :: Name
showClassName   = clsQual gHC_SHOW (fsLit "Show")      showClassKey

-- Class Read
readClassName :: Name
readClassName   = clsQual gHC_READ (fsLit "Read")      readClassKey

-- Classes Generic and Generic1, Datatype, Constructor and Selector
genClassName, gen1ClassName, datatypeClassName, constructorClassName,
  selectorClassName :: Name
genClassName  = clsQual gHC_GENERICS (fsLit "Generic")  genClassKey
gen1ClassName = clsQual gHC_GENERICS (fsLit "Generic1") gen1ClassKey

datatypeClassName    = clsQual gHC_GENERICS (fsLit "Datatype")    datatypeClassKey
constructorClassName = clsQual gHC_GENERICS (fsLit "Constructor") constructorClassKey
selectorClassName    = clsQual gHC_GENERICS (fsLit "Selector")    selectorClassKey

genericClassNames :: [Name]
genericClassNames = [genClassName, gen1ClassName]

-- GHCi things
ghciIoClassName, ghciStepIoMName :: Name
ghciIoClassName = clsQual gHC_GHCI (fsLit "GHCiSandboxIO") ghciIoClassKey
ghciStepIoMName = varQual gHC_GHCI (fsLit "ghciStepIO") ghciStepIoMClassOpKey

-- IO things
ioTyConName, ioDataConName, thenIOName, bindIOName, returnIOName,
    failIOName :: Name
ioTyConName       = tcQual  gHC_TYPES (fsLit "IO")       ioTyConKey
ioDataConName     = conName gHC_TYPES (fsLit "IO")       ioDataConKey
thenIOName        = varQual gHC_BASE  (fsLit "thenIO")   thenIOIdKey
bindIOName        = varQual gHC_BASE  (fsLit "bindIO")   bindIOIdKey
returnIOName      = varQual gHC_BASE  (fsLit "returnIO") returnIOIdKey
failIOName        = varQual gHC_IO    (fsLit "failIO")   failIOIdKey

-- IO things
printName :: Name
printName         = varQual sYSTEM_IO (fsLit "print") printIdKey

printRawName :: Name
printRawName         = varQual eTA_RTS (fsLit "printRaw") printRawIdKey

-- Int, Word, and Addr things
int8TyConName, int16TyConName, int32TyConName, int64TyConName :: Name
int8TyConName     = tcQual gHC_INT  (fsLit "Int8")  int8TyConKey
int16TyConName    = tcQual gHC_INT  (fsLit "Int16") int16TyConKey
int32TyConName    = tcQual gHC_INT  (fsLit "Int32") int32TyConKey
int64TyConName    = tcQual gHC_INT  (fsLit "Int64") int64TyConKey

-- Word module
word16TyConName, word32TyConName, word64TyConName :: Name
word16TyConName   = tcQual  gHC_WORD (fsLit "Word16") word16TyConKey
word32TyConName   = tcQual  gHC_WORD (fsLit "Word32") word32TyConKey
word64TyConName   = tcQual  gHC_WORD (fsLit "Word64") word64TyConKey

-- PrelPtr module
ptrTyConName, funPtrTyConName :: Name
ptrTyConName      = tcQual   gHC_PTR (fsLit "Ptr")    ptrTyConKey
funPtrTyConName   = tcQual   gHC_PTR (fsLit "FunPtr") funPtrTyConKey

-- Foreign objects and weak pointers
stablePtrTyConName, newStablePtrName :: Name
stablePtrTyConName    = tcQual   gHC_STABLE (fsLit "StablePtr")    stablePtrTyConKey
newStablePtrName      = varQual  gHC_STABLE (fsLit "newStablePtr") newStablePtrIdKey

-- PrelST module
runSTRepName :: Name
runSTRepName       = varQual gHC_ST  (fsLit "runSTRep") runSTRepIdKey

-- Recursive-do notation
monadFixClassName, mfixName :: Name
monadFixClassName  = clsQual mONAD_FIX (fsLit "MonadFix") monadFixClassKey
mfixName           = varQual mONAD_FIX (fsLit "mfix")     mfixIdKey

-- Arrow notation
arrAName, composeAName, firstAName, appAName, choiceAName, loopAName :: Name
arrAName           = varQual aRROW (fsLit "arr")       arrAIdKey
composeAName       = varQual gHC_DESUGAR (fsLit ">>>") composeAIdKey
firstAName         = varQual aRROW (fsLit "first")     firstAIdKey
appAName           = varQual aRROW (fsLit "app")       appAIdKey
choiceAName        = varQual aRROW (fsLit "|||")       choiceAIdKey
loopAName          = varQual aRROW (fsLit "loop")      loopAIdKey

-- Monad comprehensions
guardMName, liftMName, mzipName :: Name
guardMName         = varQual mONAD (fsLit "guard")    guardMIdKey
liftMName          = varQual mONAD (fsLit "liftM")    liftMIdKey
mzipName           = varQual mONAD_ZIP (fsLit "mzip") mzipIdKey


-- Annotation type checking
toAnnotationWrapperName :: Name
toAnnotationWrapperName = varQual gHC_DESUGAR (fsLit "toAnnotationWrapper") toAnnotationWrapperIdKey

-- Other classes, needed for type defaulting
monadPlusClassName, randomClassName, randomGenClassName,
    isStringClassName :: Name
monadPlusClassName  = clsQual mONAD (fsLit "MonadPlus")      monadPlusClassKey
randomClassName     = clsQual rANDOM (fsLit "Random")        randomClassKey
randomGenClassName  = clsQual rANDOM (fsLit "RandomGen")     randomGenClassKey
isStringClassName   = clsQual dATA_STRING (fsLit "IsString") isStringClassKey

-- Type-level naturals
knownNatClassName :: Name
knownNatClassName     = clsQual gHC_TYPENATS (fsLit "KnownNat") knownNatClassNameKey
knownSymbolClassName :: Name
knownSymbolClassName  = clsQual gHC_TYPELITS (fsLit "KnownSymbol") knownSymbolClassNameKey

-- Overloaded labels
isLabelClassName :: Name
isLabelClassName
 = clsQual gHC_OVER_LABELS (fsLit "IsLabel") isLabelClassNameKey

-- Source Locations
callStackDataConName, callStackTyConName, emptyCallStackName, pushCallStackName,
 srcLocDataConName, hasCallStackTyConName :: Name
callStackDataConName
  = conName gHC_STACK_TYPES (fsLit "CallStack") callStackDataConKey
callStackTyConName
  = tcQual  gHC_STACK_TYPES (fsLit "CallStack") callStackTyConKey
emptyCallStackName
  = varQual gHC_STACK_TYPES (fsLit "emptyCallStack") emptyCallStackKey
pushCallStackName
  = varQual gHC_STACK_TYPES (fsLit "pushCallStack") pushCallStackKey
srcLocDataConName
  = conName gHC_STACK_TYPES (fsLit "SrcLoc")   srcLocDataConKey
hasCallStackTyConName
  = tcQual  gHC_STACK_TYPES (fsLit "HasCallStack") hasCallStackTyConKey

-- plugins
pLUGINS :: Module
pLUGINS = mkThisGhcModule (fsLit "Plugins")
pluginTyConName :: Name
pluginTyConName = tcQual pLUGINS (fsLit "Plugin") pluginTyConKey

-- Static pointers
staticPtrInfoTyConName :: Name
staticPtrInfoTyConName =
    tcQual gHC_STATICPTR (fsLit "StaticPtrInfo") staticPtrInfoTyConKey

staticPtrInfoDataConName :: Name
staticPtrInfoDataConName =
    conName gHC_STATICPTR (fsLit "StaticPtrInfo") staticPtrInfoDataConKey

staticPtrTyConName :: Name
staticPtrTyConName =
    tcQual gHC_STATICPTR (fsLit "StaticPtr") staticPtrTyConKey

staticPtrDataConName :: Name
staticPtrDataConName =
    conName gHC_STATICPTR (fsLit "StaticPtr") staticPtrDataConKey

fingerprintDataConName :: Name
fingerprintDataConName =
    conName gHC_FINGERPRINT_TYPE (fsLit "Fingerprint") fingerprintDataConKey

-- ETA-specific names
javaTyConName, extendsFamTyConName, javaDataConName, extendsClassName,
  superCastName, unsafeCastName, fmapJavaName, classClassName, objName, unobjName,
  classIdentifierName, fromJStringName, toJStringName, inheritsFamTyConName,
  overloadableClassName, sobjectTyConName, sobjectDataConName, byteTyConName,
  shortTyConName, jcharTyConName :: Name
javaTyConName         = tcQual  gHC_TYPES   (fsLit "Java") javaTyConKey
extendsFamTyConName   = tcQual  gHC_CLASSES (fsLit "Extends'") extendsFamTyConKey
javaDataConName       = conName gHC_TYPES   (fsLit "Java") javaDataConKey
extendsClassName      = clsQual gHC_CLASSES (fsLit "Extends") extendsClassKey
superCastName         = varQual gHC_CLASSES (fsLit "superCast") superCastClassOpKey
unsafeCastName        = varQual gHC_CLASSES (fsLit "unsafeCast") unsafeCastClassOpKey
fmapJavaName          = varQual gHC_BASE    (fsLit "fmapJava") fmapJavaIdKey
classClassName        = clsQual gHC_CLASSES (fsLit "Class") classClassKey
objName               = varQual gHC_CLASSES (fsLit "obj") objClassOpKey
unobjName             = varQual gHC_CLASSES (fsLit "unobj") unobjClassOpKey
classIdentifierName   = varQual gHC_CLASSES (fsLit "classIdentifier") classIdentifierClassOpKey
fromJStringName       = varQual jAVA_STRING (fsLit "fromJString") fromJStringIdKey
toJStringName         = varQual jAVA_STRING (fsLit "toJString") toJStringIdKey
inheritsFamTyConName  = tcQual  gHC_CLASSES  (fsLit "Inherits") inheritsFamTyConKey
overloadableClassName = clsQual eTA_INTEROP (fsLit "Overloadable") overloadableClassKey
sobjectTyConName      = tcQual  eTA_INTEROP (fsLit "SObject") sobjectTyConKey
sobjectDataConName    = conName eTA_INTEROP (fsLit "SObject") sobjectDataConKey
byteTyConName         = tcQual  jAVA_PRIMITIVEBASE (fsLit "Byte") byteTyConKey
shortTyConName        = tcQual  jAVA_PRIMITIVEBASE (fsLit "Short") shortTyConKey
jcharTyConName        = tcQual  jAVA_PRIMITIVEBASE (fsLit "JChar") jcharTyConKey

{-
************************************************************************
*                                                                      *
\subsection{Local helpers}
*                                                                      *
************************************************************************

All these are original names; hence mkOrig
-}

varQual, tcQual, clsQual, dcQual :: Module -> FastString -> Unique -> Name
varQual  = mk_known_key_name varName
tcQual   = mk_known_key_name tcName
clsQual  = mk_known_key_name clsName
dcQual   = mk_known_key_name dataName

mk_known_key_name :: NameSpace -> Module -> FastString -> Unique -> Name
mk_known_key_name space modu str unique
  = mkExternalName unique modu (mkOccNameFS space str) noSrcSpan

conName :: Module -> FastString -> Unique -> Name
conName modu occ unique
  = mkExternalName unique modu (mkOccNameFS dataName occ) noSrcSpan

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-Classes]{@Uniques@ for wired-in @Classes@}
*                                                                      *
************************************************************************
--MetaHaskell extension hand allocate keys here
-}

boundedClassKey, enumClassKey, eqClassKey, floatingClassKey,
    fractionalClassKey, integralClassKey, monadClassKey, dataClassKey,
    functorClassKey, numClassKey, ordClassKey, readClassKey, realClassKey,
    realFloatClassKey, realFracClassKey, showClassKey, ixClassKey :: Unique
boundedClassKey         = mkPreludeClassUnique 1
enumClassKey            = mkPreludeClassUnique 2
eqClassKey              = mkPreludeClassUnique 3
floatingClassKey        = mkPreludeClassUnique 5
fractionalClassKey      = mkPreludeClassUnique 6
integralClassKey        = mkPreludeClassUnique 7
monadClassKey           = mkPreludeClassUnique 8
dataClassKey            = mkPreludeClassUnique 9
functorClassKey         = mkPreludeClassUnique 10
numClassKey             = mkPreludeClassUnique 11
ordClassKey             = mkPreludeClassUnique 12
readClassKey            = mkPreludeClassUnique 13
realClassKey            = mkPreludeClassUnique 14
realFloatClassKey       = mkPreludeClassUnique 15
realFracClassKey        = mkPreludeClassUnique 16
showClassKey            = mkPreludeClassUnique 17
ixClassKey              = mkPreludeClassUnique 18

typeableClassKey, typeable1ClassKey, typeable2ClassKey, typeable3ClassKey,
    typeable4ClassKey, typeable5ClassKey, typeable6ClassKey, typeable7ClassKey
    :: Unique
typeableClassKey        = mkPreludeClassUnique 20
typeable1ClassKey       = mkPreludeClassUnique 21
typeable2ClassKey       = mkPreludeClassUnique 22
typeable3ClassKey       = mkPreludeClassUnique 23
typeable4ClassKey       = mkPreludeClassUnique 24
typeable5ClassKey       = mkPreludeClassUnique 25
typeable6ClassKey       = mkPreludeClassUnique 26
typeable7ClassKey       = mkPreludeClassUnique 27

monadFixClassKey :: Unique
monadFixClassKey        = mkPreludeClassUnique 28

monadPlusClassKey, randomClassKey, randomGenClassKey :: Unique
monadPlusClassKey       = mkPreludeClassUnique 30
randomClassKey          = mkPreludeClassUnique 31
randomGenClassKey       = mkPreludeClassUnique 32

isStringClassKey :: Unique
isStringClassKey        = mkPreludeClassUnique 33

applicativeClassKey, foldableClassKey, traversableClassKey :: Unique
applicativeClassKey     = mkPreludeClassUnique 34
foldableClassKey        = mkPreludeClassUnique 35
traversableClassKey     = mkPreludeClassUnique 36

genClassKey, gen1ClassKey, datatypeClassKey, constructorClassKey,
  selectorClassKey :: Unique
genClassKey   = mkPreludeClassUnique 37
gen1ClassKey  = mkPreludeClassUnique 38

datatypeClassKey    = mkPreludeClassUnique 39
constructorClassKey = mkPreludeClassUnique 40
selectorClassKey    = mkPreludeClassUnique 41

-- KnownNat: see Note [KnowNat & KnownSymbol and EvLit] in TcEvidence
knownNatClassNameKey :: Unique
knownNatClassNameKey = mkPreludeClassUnique 42

-- KnownSymbol: see Note [KnownNat & KnownSymbol and EvLit] in TcEvidence
knownSymbolClassNameKey :: Unique
knownSymbolClassNameKey = mkPreludeClassUnique 43

ghciIoClassKey :: Unique
ghciIoClassKey = mkPreludeClassUnique 44

isLabelClassNameKey :: Unique
isLabelClassNameKey = mkPreludeClassUnique 49

extendsClassKey, classClassKey, overloadableClassKey :: Unique
extendsClassKey = mkPreludeClassUnique 46
classClassKey   = mkPreludeClassUnique 47
overloadableClassKey = mkPreludeClassUnique 48

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-TyCons]{@Uniques@ for wired-in @TyCons@}
*                                                                      *
************************************************************************
-}

addrPrimTyConKey, arrayPrimTyConKey, arrayArrayPrimTyConKey, boolTyConKey, byteArrayPrimTyConKey,
    charPrimTyConKey, charTyConKey, doublePrimTyConKey, doubleTyConKey,
    floatPrimTyConKey, floatTyConKey, funTyConKey, intPrimTyConKey,
    intTyConKey, int8TyConKey, int16TyConKey, int32PrimTyConKey,
    int32TyConKey, int64PrimTyConKey, int64TyConKey,
    integerTyConKey,
    listTyConKey, foreignObjPrimTyConKey, maybeTyConKey, weakPrimTyConKey,
    mutableArrayPrimTyConKey, mutableArrayArrayPrimTyConKey, mutableByteArrayPrimTyConKey,
    orderingTyConKey, mVarPrimTyConKey, ratioTyConKey, rationalTyConKey,
    realWorldTyConKey, stablePtrPrimTyConKey, stablePtrTyConKey,
    anyTyConKey, eqTyConKey, smallArrayPrimTyConKey,
    smallMutableArrayPrimTyConKey :: Unique
addrPrimTyConKey                        = mkPreludeTyConUnique  1
arrayPrimTyConKey                       = mkPreludeTyConUnique  3
boolTyConKey                            = mkPreludeTyConUnique  4
byteArrayPrimTyConKey                   = mkPreludeTyConUnique  5
charPrimTyConKey                        = mkPreludeTyConUnique  7
charTyConKey                            = mkPreludeTyConUnique  8
doublePrimTyConKey                      = mkPreludeTyConUnique  9
doubleTyConKey                          = mkPreludeTyConUnique 10
floatPrimTyConKey                       = mkPreludeTyConUnique 11
floatTyConKey                           = mkPreludeTyConUnique 12
funTyConKey                             = mkPreludeTyConUnique 13
intPrimTyConKey                         = mkPreludeTyConUnique 14
intTyConKey                             = mkPreludeTyConUnique 15
int8TyConKey                            = mkPreludeTyConUnique 16
int16TyConKey                           = mkPreludeTyConUnique 17
int32PrimTyConKey                       = mkPreludeTyConUnique 18
int32TyConKey                           = mkPreludeTyConUnique 19
int64PrimTyConKey                       = mkPreludeTyConUnique 20
int64TyConKey                           = mkPreludeTyConUnique 21
integerTyConKey                         = mkPreludeTyConUnique 22

listTyConKey                            = mkPreludeTyConUnique 24
foreignObjPrimTyConKey                  = mkPreludeTyConUnique 25
maybeTyConKey                           = mkPreludeTyConUnique 26
weakPrimTyConKey                        = mkPreludeTyConUnique 27
mutableArrayPrimTyConKey                = mkPreludeTyConUnique 28
mutableByteArrayPrimTyConKey            = mkPreludeTyConUnique 29
orderingTyConKey                        = mkPreludeTyConUnique 30
mVarPrimTyConKey                        = mkPreludeTyConUnique 31
ratioTyConKey                           = mkPreludeTyConUnique 32
rationalTyConKey                        = mkPreludeTyConUnique 33
realWorldTyConKey                       = mkPreludeTyConUnique 34
stablePtrPrimTyConKey                   = mkPreludeTyConUnique 35
stablePtrTyConKey                       = mkPreludeTyConUnique 36
anyTyConKey                             = mkPreludeTyConUnique 37
eqTyConKey                              = mkPreludeTyConUnique 38
arrayArrayPrimTyConKey                  = mkPreludeTyConUnique 39
mutableArrayArrayPrimTyConKey           = mkPreludeTyConUnique 40

statePrimTyConKey, stableNamePrimTyConKey, stableNameTyConKey,
    mutVarPrimTyConKey, ioTyConKey,
    wordPrimTyConKey, wordTyConKey, word8TyConKey, word16TyConKey,
    word32PrimTyConKey, word32TyConKey, word64PrimTyConKey, word64TyConKey,
    liftedConKey, unliftedConKey, anyBoxConKey, kindConKey, boxityConKey,
    typeConKey, threadIdPrimTyConKey, bcoPrimTyConKey, ptrTyConKey,
    funPtrTyConKey, tVarPrimTyConKey, eqPrimTyConKey,
    eqReprPrimTyConKey, voidPrimTyConKey :: Unique
statePrimTyConKey                       = mkPreludeTyConUnique 50
stableNamePrimTyConKey                  = mkPreludeTyConUnique 51
stableNameTyConKey                      = mkPreludeTyConUnique 52
eqPrimTyConKey                          = mkPreludeTyConUnique 53
eqReprPrimTyConKey                      = mkPreludeTyConUnique 54
mutVarPrimTyConKey                      = mkPreludeTyConUnique 55
ioTyConKey                              = mkPreludeTyConUnique 56
voidPrimTyConKey                        = mkPreludeTyConUnique 57
wordPrimTyConKey                        = mkPreludeTyConUnique 58
wordTyConKey                            = mkPreludeTyConUnique 59
word8TyConKey                           = mkPreludeTyConUnique 60
word16TyConKey                          = mkPreludeTyConUnique 61
word32PrimTyConKey                      = mkPreludeTyConUnique 62
word32TyConKey                          = mkPreludeTyConUnique 63
word64PrimTyConKey                      = mkPreludeTyConUnique 64
word64TyConKey                          = mkPreludeTyConUnique 65
liftedConKey                            = mkPreludeTyConUnique 66
unliftedConKey                          = mkPreludeTyConUnique 67
anyBoxConKey                            = mkPreludeTyConUnique 68
kindConKey                              = mkPreludeTyConUnique 69
boxityConKey                            = mkPreludeTyConUnique 70
typeConKey                              = mkPreludeTyConUnique 71
threadIdPrimTyConKey                    = mkPreludeTyConUnique 72
bcoPrimTyConKey                         = mkPreludeTyConUnique 73
ptrTyConKey                             = mkPreludeTyConUnique 74
funPtrTyConKey                          = mkPreludeTyConUnique 75
tVarPrimTyConKey                        = mkPreludeTyConUnique 76

-- ETA-specific tycons
jcharPrimTyConKey, jboolPrimTyConKey, jbytePrimTyConKey, jshortPrimTyConKey,
  jobjectPrimTyConKey, javaTyConKey, jstringTyConKey, extendsFamTyConKey,
  inheritsFamTyConKey, sobjectTyConKey, byteTyConKey, shortTyConKey,
  jcharTyConKey :: Unique
jcharPrimTyConKey   = mkPreludeTyConUnique 77
jboolPrimTyConKey   = mkPreludeTyConUnique 78
jbytePrimTyConKey   = mkPreludeTyConUnique 79
jshortPrimTyConKey  = mkPreludeTyConUnique 80
jobjectPrimTyConKey = mkPreludeTyConUnique 81
-- 83
javaTyConKey        = mkPreludeTyConUnique 90
jstringTyConKey     = mkPreludeTyConUnique 91
extendsFamTyConKey  = mkPreludeTyConUnique 103
inheritsFamTyConKey = mkPreludeTyConUnique 104
sobjectTyConKey     = mkPreludeTyConUnique 105
byteTyConKey        = mkPreludeTyConUnique 106
shortTyConKey       = mkPreludeTyConUnique 107
jcharTyConKey       = mkPreludeTyConUnique 108

-- Parallel array type constructor
parrTyConKey :: Unique
parrTyConKey                            = mkPreludeTyConUnique 82

-- -- dotnet interop
-- objectTyConKey :: Unique
-- objectTyConKey                          = mkPreludeTyConUnique 83

eitherTyConKey :: Unique
eitherTyConKey                          = mkPreludeTyConUnique 84

-- Super Kinds constructors
superKindTyConKey :: Unique
superKindTyConKey                     = mkPreludeTyConUnique 85

-- Kind constructors
liftedTypeKindTyConKey, anyKindTyConKey, openTypeKindTyConKey,
  unliftedTypeKindTyConKey, constraintKindTyConKey :: Unique
anyKindTyConKey                         = mkPreludeTyConUnique 86
liftedTypeKindTyConKey                  = mkPreludeTyConUnique 87
openTypeKindTyConKey                    = mkPreludeTyConUnique 88
unliftedTypeKindTyConKey                = mkPreludeTyConUnique 89
constraintKindTyConKey                  = mkPreludeTyConUnique 92

-- Coercion constructors
symCoercionTyConKey, transCoercionTyConKey, leftCoercionTyConKey,
    rightCoercionTyConKey, instCoercionTyConKey, unsafeCoercionTyConKey,
    csel1CoercionTyConKey, csel2CoercionTyConKey, cselRCoercionTyConKey
    :: Unique
symCoercionTyConKey                     = mkPreludeTyConUnique 93
transCoercionTyConKey                   = mkPreludeTyConUnique 94
leftCoercionTyConKey                    = mkPreludeTyConUnique 95
rightCoercionTyConKey                   = mkPreludeTyConUnique 96
instCoercionTyConKey                    = mkPreludeTyConUnique 97
unsafeCoercionTyConKey                  = mkPreludeTyConUnique 98
csel1CoercionTyConKey                   = mkPreludeTyConUnique 99
csel2CoercionTyConKey                   = mkPreludeTyConUnique 100
cselRCoercionTyConKey                   = mkPreludeTyConUnique 101

pluginTyConKey :: Unique
pluginTyConKey                          = mkPreludeTyConUnique 102

unknownTyConKey, unknown1TyConKey, unknown2TyConKey, unknown3TyConKey,
    opaqueTyConKey :: Unique
unknownTyConKey                         = mkPreludeTyConUnique 129
unknown1TyConKey                        = mkPreludeTyConUnique 130
unknown2TyConKey                        = mkPreludeTyConUnique 131
unknown3TyConKey                        = mkPreludeTyConUnique 132
opaqueTyConKey                          = mkPreludeTyConUnique 133

stringTyConKey :: Unique
stringTyConKey                          = mkPreludeTyConUnique 134

-- Generics (Unique keys)
v1TyConKey, u1TyConKey, par1TyConKey, rec1TyConKey,
  k1TyConKey, m1TyConKey, sumTyConKey, prodTyConKey,
  compTyConKey, rTyConKey, pTyConKey, dTyConKey,
  cTyConKey, sTyConKey, rec0TyConKey, par0TyConKey,
  d1TyConKey, c1TyConKey, s1TyConKey, noSelTyConKey,
  repTyConKey, rep1TyConKey :: Unique

v1TyConKey    = mkPreludeTyConUnique 135
u1TyConKey    = mkPreludeTyConUnique 136
par1TyConKey  = mkPreludeTyConUnique 137
rec1TyConKey  = mkPreludeTyConUnique 138
k1TyConKey    = mkPreludeTyConUnique 139
m1TyConKey    = mkPreludeTyConUnique 140

sumTyConKey   = mkPreludeTyConUnique 141
prodTyConKey  = mkPreludeTyConUnique 142
compTyConKey  = mkPreludeTyConUnique 143

rTyConKey = mkPreludeTyConUnique 144
pTyConKey = mkPreludeTyConUnique 145
dTyConKey = mkPreludeTyConUnique 146
cTyConKey = mkPreludeTyConUnique 147
sTyConKey = mkPreludeTyConUnique 148

rec0TyConKey  = mkPreludeTyConUnique 149
par0TyConKey  = mkPreludeTyConUnique 150
d1TyConKey    = mkPreludeTyConUnique 151
c1TyConKey    = mkPreludeTyConUnique 152
s1TyConKey    = mkPreludeTyConUnique 153
noSelTyConKey = mkPreludeTyConUnique 154

repTyConKey  = mkPreludeTyConUnique 155
rep1TyConKey = mkPreludeTyConUnique 156

-- Type-level naturals
typeNatKindConNameKey, typeSymbolKindConNameKey,
  typeNatAddTyFamNameKey, typeNatMulTyFamNameKey, typeNatExpTyFamNameKey,
  typeNatLeqTyFamNameKey, typeNatSubTyFamNameKey
  , typeSymbolCmpTyFamNameKey, typeNatCmpTyFamNameKey
  :: Unique
typeNatKindConNameKey     = mkPreludeTyConUnique 160
typeSymbolKindConNameKey  = mkPreludeTyConUnique 161
typeNatAddTyFamNameKey    = mkPreludeTyConUnique 162
typeNatMulTyFamNameKey    = mkPreludeTyConUnique 163
typeNatExpTyFamNameKey    = mkPreludeTyConUnique 164
typeNatLeqTyFamNameKey    = mkPreludeTyConUnique 165
typeNatSubTyFamNameKey    = mkPreludeTyConUnique 166
typeSymbolCmpTyFamNameKey = mkPreludeTyConUnique 167
typeNatCmpTyFamNameKey    = mkPreludeTyConUnique 168

-- Custom user type-errors
errorMessageTypeErrorFamKey :: Unique
errorMessageTypeErrorFamKey =  mkPreludeTyConUnique 173

ntTyConKey:: Unique
ntTyConKey = mkPreludeTyConUnique 174
coercibleTyConKey :: Unique
coercibleTyConKey = mkPreludeTyConUnique 175

proxyPrimTyConKey :: Unique
proxyPrimTyConKey = mkPreludeTyConUnique 176

specTyConKey :: Unique
specTyConKey = mkPreludeTyConUnique 177

smallArrayPrimTyConKey        = mkPreludeTyConUnique  178
smallMutableArrayPrimTyConKey = mkPreludeTyConUnique  179

staticPtrTyConKey  :: Unique
staticPtrTyConKey  = mkPreludeTyConUnique 180

staticPtrInfoTyConKey :: Unique
staticPtrInfoTyConKey = mkPreludeTyConUnique 181

typeRepTyConKey :: Unique
typeRepTyConKey = mkPreludeTyConUnique 183

callStackTyConKey :: Unique
callStackTyConKey = mkPreludeTyConUnique 182

hasCallStackTyConKey :: Unique
hasCallStackTyConKey = mkPreludeTyConUnique 184

-- Implicit Parameters
ipTyConKey :: Unique
ipTyConKey = mkPreludeTyConUnique 185

ipCoNameKey :: Unique
ipCoNameKey = mkPreludeTyConUnique 186

typeErrorTextDataConKey,
  typeErrorAppendDataConKey,
  typeErrorVAppendDataConKey,
  typeErrorShowTypeDataConKey
  :: Unique
typeErrorTextDataConKey                 = mkPreludeDataConUnique 50
typeErrorAppendDataConKey               = mkPreludeDataConUnique 51
typeErrorVAppendDataConKey              = mkPreludeDataConUnique 52
typeErrorShowTypeDataConKey             = mkPreludeDataConUnique 53

---------------- Template Haskell -------------------
--      USES TyConUniques 200-299
-----------------------------------------------------

----------------------- SIMD ------------------------
--      USES TyConUniques 300-399
-----------------------------------------------------

-- int8X16PrimTyConKey :: Unique
-- int8X16PrimTyConKey = mkPreludeTyConUnique 300
-- int16X8PrimTyConKey :: Unique
-- int16X8PrimTyConKey = mkPreludeTyConUnique 301
-- int32X4PrimTyConKey :: Unique
-- int32X4PrimTyConKey = mkPreludeTyConUnique 302
-- int64X2PrimTyConKey :: Unique
-- int64X2PrimTyConKey = mkPreludeTyConUnique 303
-- int8X32PrimTyConKey :: Unique
-- int8X32PrimTyConKey = mkPreludeTyConUnique 304
-- int16X16PrimTyConKey :: Unique
-- int16X16PrimTyConKey = mkPreludeTyConUnique 305
-- int32X8PrimTyConKey :: Unique
-- int32X8PrimTyConKey = mkPreludeTyConUnique 306
-- int64X4PrimTyConKey :: Unique
-- int64X4PrimTyConKey = mkPreludeTyConUnique 307
-- int8X64PrimTyConKey :: Unique
-- int8X64PrimTyConKey = mkPreludeTyConUnique 308
-- int16X32PrimTyConKey :: Unique
-- int16X32PrimTyConKey = mkPreludeTyConUnique 309
-- int32X16PrimTyConKey :: Unique
-- int32X16PrimTyConKey = mkPreludeTyConUnique 310
-- int64X8PrimTyConKey :: Unique
-- int64X8PrimTyConKey = mkPreludeTyConUnique 311
-- word8X16PrimTyConKey :: Unique
-- word8X16PrimTyConKey = mkPreludeTyConUnique 312
-- word16X8PrimTyConKey :: Unique
-- word16X8PrimTyConKey = mkPreludeTyConUnique 313
-- word32X4PrimTyConKey :: Unique
-- word32X4PrimTyConKey = mkPreludeTyConUnique 314
-- word64X2PrimTyConKey :: Unique
-- word64X2PrimTyConKey = mkPreludeTyConUnique 315
-- word8X32PrimTyConKey :: Unique
-- word8X32PrimTyConKey = mkPreludeTyConUnique 316
-- word16X16PrimTyConKey :: Unique
-- word16X16PrimTyConKey = mkPreludeTyConUnique 317
-- word32X8PrimTyConKey :: Unique
-- word32X8PrimTyConKey = mkPreludeTyConUnique 318
-- word64X4PrimTyConKey :: Unique
-- word64X4PrimTyConKey = mkPreludeTyConUnique 319
-- word8X64PrimTyConKey :: Unique
-- word8X64PrimTyConKey = mkPreludeTyConUnique 320
-- word16X32PrimTyConKey :: Unique
-- word16X32PrimTyConKey = mkPreludeTyConUnique 321
-- word32X16PrimTyConKey :: Unique
-- word32X16PrimTyConKey = mkPreludeTyConUnique 322
-- word64X8PrimTyConKey :: Unique
-- word64X8PrimTyConKey = mkPreludeTyConUnique 323
-- floatX4PrimTyConKey :: Unique
-- floatX4PrimTyConKey = mkPreludeTyConUnique 324
-- doubleX2PrimTyConKey :: Unique
-- doubleX2PrimTyConKey = mkPreludeTyConUnique 325
-- floatX8PrimTyConKey :: Unique
-- floatX8PrimTyConKey = mkPreludeTyConUnique 326
-- doubleX4PrimTyConKey :: Unique
-- doubleX4PrimTyConKey = mkPreludeTyConUnique 327
-- floatX16PrimTyConKey :: Unique
-- floatX16PrimTyConKey = mkPreludeTyConUnique 328
-- doubleX8PrimTyConKey :: Unique
-- doubleX8PrimTyConKey = mkPreludeTyConUnique 329

unitTyConKey :: Unique
unitTyConKey = mkTupleTyConUnique BoxedTuple 0

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-DataCons]{@Uniques@ for wired-in @DataCons@}
*                                                                      *
************************************************************************
-}

charDataConKey, consDataConKey, doubleDataConKey, falseDataConKey,
    floatDataConKey, intDataConKey, integerSDataConKey, nilDataConKey,
    ratioDataConKey, stableNameDataConKey, trueDataConKey, wordDataConKey,
    word8DataConKey, ioDataConKey, integerDataConKey, eqBoxDataConKey,
    coercibleDataConKey, nothingDataConKey, justDataConKey :: Unique
charDataConKey                          = mkPreludeDataConUnique  1
consDataConKey                          = mkPreludeDataConUnique  2
doubleDataConKey                        = mkPreludeDataConUnique  3
falseDataConKey                         = mkPreludeDataConUnique  4
floatDataConKey                         = mkPreludeDataConUnique  5
intDataConKey                           = mkPreludeDataConUnique  6
integerSDataConKey                      = mkPreludeDataConUnique  7
nothingDataConKey                       = mkPreludeDataConUnique  8
justDataConKey                          = mkPreludeDataConUnique  9
nilDataConKey                           = mkPreludeDataConUnique 11
ratioDataConKey                         = mkPreludeDataConUnique 12
word8DataConKey                         = mkPreludeDataConUnique 13
stableNameDataConKey                    = mkPreludeDataConUnique 14
trueDataConKey                          = mkPreludeDataConUnique 15
wordDataConKey                          = mkPreludeDataConUnique 16
ioDataConKey                            = mkPreludeDataConUnique 17
integerDataConKey                       = mkPreludeDataConUnique 18
eqBoxDataConKey                         = mkPreludeDataConUnique 19

-- Generic data constructors
crossDataConKey, inlDataConKey, inrDataConKey, genUnitDataConKey :: Unique
crossDataConKey                         = mkPreludeDataConUnique 20
inlDataConKey                           = mkPreludeDataConUnique 21
inrDataConKey                           = mkPreludeDataConUnique 22
genUnitDataConKey                       = mkPreludeDataConUnique 23

-- Data constructor for parallel arrays
parrDataConKey :: Unique
parrDataConKey                          = mkPreludeDataConUnique 24

leftDataConKey, rightDataConKey :: Unique
leftDataConKey                          = mkPreludeDataConUnique 25
rightDataConKey                         = mkPreludeDataConUnique 26

ltDataConKey, eqDataConKey, gtDataConKey :: Unique
ltDataConKey                            = mkPreludeDataConUnique 27
eqDataConKey                            = mkPreludeDataConUnique 28
gtDataConKey                            = mkPreludeDataConUnique 29

coercibleDataConKey                     = mkPreludeDataConUnique 32

staticPtrDataConKey :: Unique
staticPtrDataConKey                     = mkPreludeDataConUnique 33

staticPtrInfoDataConKey :: Unique
staticPtrInfoDataConKey                 = mkPreludeDataConUnique 34

fingerprintDataConKey :: Unique
fingerprintDataConKey                   = mkPreludeDataConUnique 35

callStackDataConKey, srcLocDataConKey :: Unique
callStackDataConKey                     = mkPreludeDataConUnique 36
srcLocDataConKey                        = mkPreludeDataConUnique 37

ipDataConKey :: Unique
ipDataConKey                            = mkPreludeDataConUnique 41


javaDataConKey, jstringDataConKey, sobjectDataConKey :: Unique
javaDataConKey    = mkPreludeDataConUnique 38
jstringDataConKey = mkPreludeDataConUnique 39
sobjectDataConKey = mkPreludeDataConUnique 40

{-
************************************************************************
*                                                                      *
\subsubsection[Uniques-prelude-Ids]{@Uniques@ for wired-in @Ids@ (except @DataCons@)}
*                                                                      *
************************************************************************
-}

wildCardKey, absentErrorIdKey, augmentIdKey, appendIdKey,
    buildIdKey, errorIdKey, foldrIdKey, recSelErrorIdKey,
    seqIdKey, irrefutPatErrorIdKey, eqStringIdKey,
    noMethodBindingErrorIdKey, nonExhaustiveGuardsErrorIdKey,
    runtimeErrorIdKey, patErrorIdKey, voidPrimIdKey,
    realWorldPrimIdKey, recConErrorIdKey,
    unpackCStringUtf8IdKey, unpackCStringAppendIdKey,
    unpackCStringFoldrIdKey, unpackCStringIdKey :: Unique
wildCardKey                   = mkPreludeMiscIdUnique  0  -- See Note [WildCard binders]
absentErrorIdKey              = mkPreludeMiscIdUnique  1
augmentIdKey                  = mkPreludeMiscIdUnique  2
appendIdKey                   = mkPreludeMiscIdUnique  3
buildIdKey                    = mkPreludeMiscIdUnique  4
errorIdKey                    = mkPreludeMiscIdUnique  5
foldrIdKey                    = mkPreludeMiscIdUnique  6
recSelErrorIdKey              = mkPreludeMiscIdUnique  7
seqIdKey                      = mkPreludeMiscIdUnique  8
irrefutPatErrorIdKey          = mkPreludeMiscIdUnique  9
eqStringIdKey                 = mkPreludeMiscIdUnique 10
noMethodBindingErrorIdKey     = mkPreludeMiscIdUnique 11
nonExhaustiveGuardsErrorIdKey = mkPreludeMiscIdUnique 12
runtimeErrorIdKey             = mkPreludeMiscIdUnique 13
patErrorIdKey                 = mkPreludeMiscIdUnique 14
realWorldPrimIdKey            = mkPreludeMiscIdUnique 15
recConErrorIdKey              = mkPreludeMiscIdUnique 16
unpackCStringUtf8IdKey        = mkPreludeMiscIdUnique 17
unpackCStringAppendIdKey      = mkPreludeMiscIdUnique 18
unpackCStringFoldrIdKey       = mkPreludeMiscIdUnique 19
unpackCStringIdKey            = mkPreludeMiscIdUnique 20
voidPrimIdKey                 = mkPreludeMiscIdUnique 21

unsafeCoerceIdKey, concatIdKey, filterIdKey, zipIdKey, bindIOIdKey,
    returnIOIdKey, newStablePtrIdKey,
    printIdKey, printRawIdKey, failIOIdKey, nullAddrIdKey, voidArgIdKey,
    fstIdKey, sndIdKey, otherwiseIdKey, assertIdKey, runSTRepIdKey :: Unique
unsafeCoerceIdKey             = mkPreludeMiscIdUnique 30
concatIdKey                   = mkPreludeMiscIdUnique 31
filterIdKey                   = mkPreludeMiscIdUnique 32
zipIdKey                      = mkPreludeMiscIdUnique 33
bindIOIdKey                   = mkPreludeMiscIdUnique 34
returnIOIdKey                 = mkPreludeMiscIdUnique 35
newStablePtrIdKey             = mkPreludeMiscIdUnique 36
printIdKey                    = mkPreludeMiscIdUnique 37
failIOIdKey                   = mkPreludeMiscIdUnique 38
nullAddrIdKey                 = mkPreludeMiscIdUnique 39
voidArgIdKey                  = mkPreludeMiscIdUnique 40
fstIdKey                      = mkPreludeMiscIdUnique 41
sndIdKey                      = mkPreludeMiscIdUnique 42
otherwiseIdKey                = mkPreludeMiscIdUnique 43
assertIdKey                   = mkPreludeMiscIdUnique 44
runSTRepIdKey                 = mkPreludeMiscIdUnique 45
printRawIdKey                 = mkPreludeMiscIdUnique 46

mkIntegerIdKey, smallIntegerIdKey, wordToIntegerIdKey,
    integerToWordIdKey, integerToIntIdKey,
    integerToWord64IdKey, integerToInt64IdKey,
    word64ToIntegerIdKey, int64ToIntegerIdKey,
    plusIntegerIdKey, timesIntegerIdKey, minusIntegerIdKey,
    negateIntegerIdKey,
    eqIntegerPrimIdKey, neqIntegerPrimIdKey, absIntegerIdKey, signumIntegerIdKey,
    leIntegerPrimIdKey, gtIntegerPrimIdKey, ltIntegerPrimIdKey, geIntegerPrimIdKey,
    compareIntegerIdKey, quotRemIntegerIdKey, divModIntegerIdKey,
    quotIntegerIdKey, remIntegerIdKey, divIntegerIdKey, modIntegerIdKey,
    floatFromIntegerIdKey, doubleFromIntegerIdKey,
    encodeFloatIntegerIdKey, encodeDoubleIntegerIdKey,
    decodeDoubleIntegerIdKey,
    gcdIntegerIdKey, lcmIntegerIdKey,
    andIntegerIdKey, orIntegerIdKey, xorIntegerIdKey, complementIntegerIdKey,
    shiftLIntegerIdKey, shiftRIntegerIdKey :: Unique
mkIntegerIdKey                = mkPreludeMiscIdUnique 60
smallIntegerIdKey             = mkPreludeMiscIdUnique 61
integerToWordIdKey            = mkPreludeMiscIdUnique 62
integerToIntIdKey             = mkPreludeMiscIdUnique 63
integerToWord64IdKey          = mkPreludeMiscIdUnique 64
integerToInt64IdKey           = mkPreludeMiscIdUnique 65
plusIntegerIdKey              = mkPreludeMiscIdUnique 66
timesIntegerIdKey             = mkPreludeMiscIdUnique 67
minusIntegerIdKey             = mkPreludeMiscIdUnique 68
negateIntegerIdKey            = mkPreludeMiscIdUnique 69
eqIntegerPrimIdKey            = mkPreludeMiscIdUnique 70
neqIntegerPrimIdKey           = mkPreludeMiscIdUnique 71
absIntegerIdKey               = mkPreludeMiscIdUnique 72
signumIntegerIdKey            = mkPreludeMiscIdUnique 73
leIntegerPrimIdKey            = mkPreludeMiscIdUnique 74
gtIntegerPrimIdKey            = mkPreludeMiscIdUnique 75
ltIntegerPrimIdKey            = mkPreludeMiscIdUnique 76
geIntegerPrimIdKey            = mkPreludeMiscIdUnique 77
compareIntegerIdKey           = mkPreludeMiscIdUnique 78
quotIntegerIdKey              = mkPreludeMiscIdUnique 79
remIntegerIdKey               = mkPreludeMiscIdUnique 80
divIntegerIdKey               = mkPreludeMiscIdUnique 81
modIntegerIdKey               = mkPreludeMiscIdUnique 82
divModIntegerIdKey            = mkPreludeMiscIdUnique 83
quotRemIntegerIdKey           = mkPreludeMiscIdUnique 84
floatFromIntegerIdKey         = mkPreludeMiscIdUnique 85
doubleFromIntegerIdKey        = mkPreludeMiscIdUnique 86
encodeFloatIntegerIdKey       = mkPreludeMiscIdUnique 87
encodeDoubleIntegerIdKey      = mkPreludeMiscIdUnique 88
gcdIntegerIdKey               = mkPreludeMiscIdUnique 89
lcmIntegerIdKey               = mkPreludeMiscIdUnique 90
andIntegerIdKey               = mkPreludeMiscIdUnique 91
orIntegerIdKey                = mkPreludeMiscIdUnique 92
xorIntegerIdKey               = mkPreludeMiscIdUnique 93
complementIntegerIdKey        = mkPreludeMiscIdUnique 94
shiftLIntegerIdKey            = mkPreludeMiscIdUnique 95
shiftRIntegerIdKey            = mkPreludeMiscIdUnique 96
wordToIntegerIdKey            = mkPreludeMiscIdUnique 97
word64ToIntegerIdKey          = mkPreludeMiscIdUnique 98
int64ToIntegerIdKey           = mkPreludeMiscIdUnique 99
decodeDoubleIntegerIdKey      = mkPreludeMiscIdUnique 100

rootMainKey, runMainKey :: Unique
rootMainKey                   = mkPreludeMiscIdUnique 101
runMainKey                    = mkPreludeMiscIdUnique 102

thenIOIdKey, lazyIdKey, assertErrorIdKey, oneShotKey, runRWKey :: Unique
thenIOIdKey                   = mkPreludeMiscIdUnique 103
lazyIdKey                     = mkPreludeMiscIdUnique 104
assertErrorIdKey              = mkPreludeMiscIdUnique 105
oneShotKey                    = mkPreludeMiscIdUnique 106
runRWKey                      = mkPreludeMiscIdUnique 107

breakpointIdKey, breakpointCondIdKey, breakpointAutoIdKey,
    breakpointJumpIdKey, breakpointCondJumpIdKey,
    breakpointAutoJumpIdKey :: Unique
breakpointIdKey               = mkPreludeMiscIdUnique 110
breakpointCondIdKey           = mkPreludeMiscIdUnique 111
breakpointAutoIdKey           = mkPreludeMiscIdUnique 112
breakpointJumpIdKey           = mkPreludeMiscIdUnique 113
breakpointCondJumpIdKey       = mkPreludeMiscIdUnique 114
breakpointAutoJumpIdKey       = mkPreludeMiscIdUnique 115

inlineIdKey, noinlineIdKey :: Unique
inlineIdKey                   = mkPreludeMiscIdUnique 120

mapIdKey, groupWithIdKey, dollarIdKey :: Unique
mapIdKey              = mkPreludeMiscIdUnique 121
groupWithIdKey        = mkPreludeMiscIdUnique 122
dollarIdKey           = mkPreludeMiscIdUnique 123

coercionTokenIdKey :: Unique
coercionTokenIdKey    = mkPreludeMiscIdUnique 124

noinlineIdKey         = mkPreludeMiscIdUnique 125

rationalToFloatIdKey, rationalToDoubleIdKey :: Unique
rationalToFloatIdKey   = mkPreludeMiscIdUnique 130
rationalToDoubleIdKey  = mkPreludeMiscIdUnique 131

-- dotnet interop
unmarshalObjectIdKey, marshalObjectIdKey, marshalStringIdKey,
    unmarshalStringIdKey, checkDotnetResNameIdKey :: Unique
unmarshalObjectIdKey          = mkPreludeMiscIdUnique 150
marshalObjectIdKey            = mkPreludeMiscIdUnique 151
marshalStringIdKey            = mkPreludeMiscIdUnique 152
unmarshalStringIdKey          = mkPreludeMiscIdUnique 153
checkDotnetResNameIdKey       = mkPreludeMiscIdUnique 154

undefinedKey :: Unique
undefinedKey                  = mkPreludeMiscIdUnique 155

magicDictKey :: Unique
magicDictKey                  = mkPreludeMiscIdUnique 156

coerceKey :: Unique
coerceKey                     = mkPreludeMiscIdUnique 157

{-
Certain class operations from Prelude classes.  They get their own
uniques so we can look them up easily when we want to conjure them up
during type checking.
-}

        -- Just a place holder for  unbound variables  produced by the renamer:
unboundKey :: Unique
unboundKey                    = mkPreludeMiscIdUnique 160

fromIntegerClassOpKey, minusClassOpKey, fromRationalClassOpKey,
    enumFromClassOpKey, enumFromThenClassOpKey, enumFromToClassOpKey,
    enumFromThenToClassOpKey, eqClassOpKey, geClassOpKey, negateClassOpKey,
    failMClassOpKey, bindMClassOpKey, thenMClassOpKey, returnMClassOpKey,
    fmapClassOpKey
    :: Unique
fromIntegerClassOpKey         = mkPreludeMiscIdUnique 160
minusClassOpKey               = mkPreludeMiscIdUnique 161
fromRationalClassOpKey        = mkPreludeMiscIdUnique 162
enumFromClassOpKey            = mkPreludeMiscIdUnique 163
enumFromThenClassOpKey        = mkPreludeMiscIdUnique 164
enumFromToClassOpKey          = mkPreludeMiscIdUnique 165
enumFromThenToClassOpKey      = mkPreludeMiscIdUnique 166
eqClassOpKey                  = mkPreludeMiscIdUnique 167
geClassOpKey                  = mkPreludeMiscIdUnique 168
negateClassOpKey              = mkPreludeMiscIdUnique 169
failMClassOpKey               = mkPreludeMiscIdUnique 170
bindMClassOpKey               = mkPreludeMiscIdUnique 171 -- (>>=)
thenMClassOpKey               = mkPreludeMiscIdUnique 172 -- (>>)
fmapClassOpKey                = mkPreludeMiscIdUnique 173
returnMClassOpKey             = mkPreludeMiscIdUnique 174

-- Recursive do notation
mfixIdKey :: Unique
mfixIdKey       = mkPreludeMiscIdUnique 175

-- Arrow notation
arrAIdKey, composeAIdKey, firstAIdKey, appAIdKey, choiceAIdKey,
    loopAIdKey :: Unique
arrAIdKey       = mkPreludeMiscIdUnique 180
composeAIdKey   = mkPreludeMiscIdUnique 181 -- >>>
firstAIdKey     = mkPreludeMiscIdUnique 182
appAIdKey       = mkPreludeMiscIdUnique 183
choiceAIdKey    = mkPreludeMiscIdUnique 184 --  |||
loopAIdKey      = mkPreludeMiscIdUnique 185

fromStringClassOpKey :: Unique
fromStringClassOpKey          = mkPreludeMiscIdUnique 186

-- Annotation type checking
toAnnotationWrapperIdKey :: Unique
toAnnotationWrapperIdKey      = mkPreludeMiscIdUnique 187

-- Conversion functions
fromIntegralIdKey, realToFracIdKey, toIntegerClassOpKey, toRationalClassOpKey :: Unique
fromIntegralIdKey    = mkPreludeMiscIdUnique 190
realToFracIdKey      = mkPreludeMiscIdUnique 191
toIntegerClassOpKey  = mkPreludeMiscIdUnique 192
toRationalClassOpKey = mkPreludeMiscIdUnique 193

-- Monad comprehensions
guardMIdKey, liftMIdKey, mzipIdKey :: Unique
guardMIdKey     = mkPreludeMiscIdUnique 194
liftMIdKey      = mkPreludeMiscIdUnique 195
mzipIdKey       = mkPreludeMiscIdUnique 196

-- GHCi
ghciStepIoMClassOpKey :: Unique
ghciStepIoMClassOpKey = mkPreludeMiscIdUnique 197

-- Overloaded lists
isListClassKey, fromListClassOpKey, fromListNClassOpKey, toListClassOpKey :: Unique
isListClassKey = mkPreludeMiscIdUnique 198
fromListClassOpKey = mkPreludeMiscIdUnique 199
fromListNClassOpKey = mkPreludeMiscIdUnique 500
toListClassOpKey = mkPreludeMiscIdUnique 501

proxyHashKey :: Unique
proxyHashKey = mkPreludeMiscIdUnique 502

---------------- Template Haskell -------------------
--      USES IdUniques 200-499
-----------------------------------------------------

-- Used to make `Typeable` dictionaries
mkTyConKey
  , mkPolyTyConAppKey
  , mkAppTyKey
  , typeLitTypeRepKey
  :: Unique
mkTyConKey        = mkPreludeMiscIdUnique 503
mkPolyTyConAppKey = mkPreludeMiscIdUnique 504
mkAppTyKey        = mkPreludeMiscIdUnique 505
typeLitTypeRepKey = mkPreludeMiscIdUnique 506

-- ETA-specific
superCastClassOpKey, unsafeCastClassOpKey, fmapJavaIdKey, objClassOpKey
  , unobjClassOpKey, classIdentifierClassOpKey, fromJStringIdKey, toJStringIdKey
    :: Unique
superCastClassOpKey  = mkPreludeMiscIdUnique 507
unsafeCastClassOpKey = mkPreludeMiscIdUnique 508
fmapJavaIdKey        = mkPreludeMiscIdUnique 509
objClassOpKey        = mkPreludeMiscIdUnique 510
unobjClassOpKey      = mkPreludeMiscIdUnique 511
fromJStringIdKey     = mkPreludeMiscIdUnique 512
toJStringIdKey       = mkPreludeMiscIdUnique 513
classIdentifierClassOpKey = mkPreludeMiscIdUnique 514

-- Dynamic
toDynIdKey :: Unique
toDynIdKey = mkPreludeMiscIdUnique 550

emptyCallStackKey, pushCallStackKey :: Unique
emptyCallStackKey = mkPreludeMiscIdUnique 551
pushCallStackKey  = mkPreludeMiscIdUnique 552

{-
************************************************************************
*                                                                      *
\subsection[Class-std-groups]{Standard groups of Prelude classes}
*                                                                      *
************************************************************************

NOTE: @Eq@ and @Text@ do need to appear in @standardClasses@
even though every numeric class has these two as a superclass,
because the list of ambiguous dictionaries hasn't been simplified.
-}

numericClassKeys :: [Unique]
numericClassKeys =
        [ numClassKey
        , realClassKey
        , integralClassKey
        ]
        ++ fractionalClassKeys

fractionalClassKeys :: [Unique]
fractionalClassKeys =
        [ fractionalClassKey
        , floatingClassKey
        , realFracClassKey
        , realFloatClassKey
        ]

-- The "standard classes" are used in defaulting (Haskell 98 report 4.3.4),
-- and are: "classes defined in the Prelude or a standard library"
standardClassKeys :: [Unique]
standardClassKeys = derivableClassKeys ++ numericClassKeys
                  ++ [randomClassKey, randomGenClassKey,
                      functorClassKey,
                      monadClassKey, monadPlusClassKey,
                      isStringClassKey,
                      applicativeClassKey, foldableClassKey,
                      traversableClassKey, alternativeClassKey
                     ]

{-
@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).
-}

derivableClassKeys :: [Unique]
derivableClassKeys
  = [ eqClassKey, ordClassKey, enumClassKey, ixClassKey,
      boundedClassKey, showClassKey, readClassKey, classClassKey ]

{-
************************************************************************
*                                                                      *
   Semi-builtin names
*                                                                      *
************************************************************************

The following names should be considered by GHCi to be in scope always.

-}

pretendNameIsInScope :: Name -> Bool
pretendNameIsInScope n
  = any (n `hasKey`)
    [ liftedTypeKindTyConKey ]
