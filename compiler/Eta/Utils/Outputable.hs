{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-1998
-}

-- | This module defines classes and functions for pretty-printing. It also
-- exports a number of helpful debugging and other utilities such as 'trace' and 'panic'.
--
-- The interface to this module is very similar to the standard Hughes-PJ pretty printing
-- module, except that it exports a number of additional functions that are rarely used,
-- and works over the 'SDoc' type.
module Eta.Utils.Outputable (
        -- * Type classes
        Outputable(..), OutputableBndr(..),

        -- * Pretty printing combinators
        SDoc, runSDoc, initSDocContext,
        docToSDoc,
        interppSP, interpp'SP, pprQuotedList, pprWithCommas, quotedListWithOr, quotedListWithNor,
        empty, nest,
        char,
        text, ftext, ptext, ztext,
        int, intWithCommas, integer, float, double, rational,
        parens, cparen, brackets, braces, quotes, quote,
        doubleQuotes, angleBrackets, paBrackets,
        semi, comma, colon, dcolon, space, equals, dot, vbar,
        arrow, larrow, darrow, arrowt, larrowt, arrowtt, larrowtt,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore,
        blankLine, forAllLit,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        hang, punctuate, ppWhen, ppUnless,
        speakNth, speakNTimes, speakN, speakNOf, speakNCaps, speakNOfCaps, plural, isOrAre,

        colored, keyword,

        -- * Converting 'SDoc' into strings and outputting it
        printForC, printForAsm, printForUser, printForUserColored, printForUserPartWay,
        pprCode, mkCodeStyle,
        showSDoc, showSDocUnsafe, showSDocOneLine,
        showSDocForUser, showSDocForUserColored, showSDocDebug, showSDocDump, showSDocDumpOneLine,
        showSDocUnqual, showPpr, showSDocWithColor,
        renderWithStyle,

        pprInfixVar, pprPrefixVar,
        pprHsChar, pprHsString, pprHsBytes,
        pprFastFilePath,

        -- * Controlling the style in which output is printed
        BindingSite(..),

        PprStyle, CodeStyle(..), PrintUnqualified(..),
        QueryQualifyName, QueryQualifyModule, QueryQualifyPackage,
        reallyAlwaysQualify, reallyAlwaysQualifyNames,
        alwaysQualify, alwaysQualifyNames, alwaysQualifyModules,
        neverQualify, neverQualifyNames, neverQualifyModules,
        alwaysQualifyPackages, neverQualifyPackages,
        QualifyName(..), queryQual,
        sdocWithDynFlags, sdocWithPlatform,
        getPprStyle, withPprStyle, withPprStyleDoc, setStyleColored,
        pprDeeper, pprDeeperList, pprSetDepth,
        codeStyle, userStyle, debugStyle, dumpStyle, asmStyle,
        ifPprDebug, qualName, qualModule, qualPackage,
        mkErrStyle, defaultErrStyle, defaultDumpStyle, mkDumpStyle, defaultUserStyle,
        mkUserStyle, cmdlineParserStyle, Depth(..),

        -- * Error handling and debugging utilities
        pprPanic, pprSorry, assertPprPanic, pprPanicFastInt, pprPgmError,
        pprTrace, pprTraceDebug, warnPprTrace,
        trace, pgmError, panic, sorry, panicFastInt, assertPanic,
        pprDebugAndThen,
    ) where

import {-# SOURCE #-}   Eta.Main.DynFlags( DynFlags,
                                  targetPlatform, pprUserLength, pprCols,
                                  useUnicode, useUnicodeSyntax, hasPprDebug,
                                  shouldUseColor,
                                  unsafeGlobalDynFlags, hasPprDebug )
import {-# SOURCE #-}   Eta.BasicTypes.Module( UnitId, Module, ModuleName, moduleName )
import {-# SOURCE #-}   Eta.BasicTypes.OccName( OccName )
import {-# SOURCE #-}   Eta.Main.StaticFlags( opt_NoDebugOutput )

import Eta.REPL.RemoteTypes
import Eta.Utils.FastString
import Eta.Utils.FastTypes
import qualified Eta.Utils.Pretty as Pretty
import Eta.Utils.Util
import Eta.Utils.Platform
import Eta.Utils.Pretty           ( Doc, Mode(..) )
import qualified Eta.Utils.PprColor as Col
import Eta.Utils.Panic
import Eta.Serialized
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Map as M
import Data.Int
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import System.IO        ( Handle )
import System.FilePath
import Text.Printf
import Data.String
import GHC.Fingerprint
import GHC.Show         ( showMultiLineString )
import Control.Exception (finally)

{-
************************************************************************
*                                                                      *
\subsection{The @PprStyle@ data type}
*                                                                      *
************************************************************************
-}

data PprStyle
  = PprUser PrintUnqualified Depth Colored
                -- Pretty-print in a way that will make sense to the
                -- ordinary user; must be very close to Haskell
                -- syntax, etc.
                -- Assumes printing tidied code: non-system names are
                -- printed without uniques.

  | PprDump PrintUnqualified
                -- For -ddump-foo; less verbose than PprDebug, but more than PprUser
                -- Does not assume tidied code: non-external names
                -- are printed with uniques.

  | PprDebug    -- Full debugging output

  | PprCode CodeStyle
                -- Print code; either C or assembler

data CodeStyle = CStyle         -- The format of labels differs for C and assembler
               | AsmStyle

data Depth = AllTheWay
           | PartWay Int        -- 0 => stop

data Colored
  = Uncolored
  | Colored

-- -----------------------------------------------------------------------------
-- Printing original names

-- | When printing code that contains original names, we need to map the
-- original names back to something the user understands.  This is the
-- purpose of the triple of functions that gets passed around
-- when rendering 'SDoc'.
data PrintUnqualified = QueryQualify {
    queryQualifyName    :: QueryQualifyName,
    queryQualifyModule  :: QueryQualifyModule,
    queryQualifyPackage :: QueryQualifyPackage
}

-- | given an /original/ name, this function tells you which module
-- name it should be qualified with when printing for the user, if
-- any.  For example, given @Control.Exception.catch@, which is in scope
-- as @Exception.catch@, this function will return @Just "Exception"@.
-- Note that the return value is a ModuleName, not a Module, because
-- in source code, names are qualified by ModuleNames.
type QueryQualifyName = Module -> OccName -> QualifyName

-- | For a given module, we need to know whether to print it with
-- a package name to disambiguate it.
type QueryQualifyModule = Module -> Bool

-- | For a given package, we need to know whether to print it with
-- the package key to disambiguate it.
type QueryQualifyPackage = UnitId -> Bool

-- See Note [Printing original names] in HscTypes
data QualifyName                        -- given P:M.T
        = NameUnqual                    -- refer to it as "T"
        | NameQual ModuleName           -- refer to it as "X.T" for the supplied X
        | NameNotInScope1
                -- it is not in scope at all, but M.T is not bound in the current
                -- scope, so we can refer to it as "M.T"
        | NameNotInScope2
                -- it is not in scope at all, and M.T is already bound in the
                -- current scope, so we must refer to it as "P:M.T"

reallyAlwaysQualifyNames :: QueryQualifyName
reallyAlwaysQualifyNames _ _ = NameNotInScope2

-- | NB: This won't ever show package IDs
alwaysQualifyNames :: QueryQualifyName
alwaysQualifyNames m _ = NameQual (moduleName m)

neverQualifyNames :: QueryQualifyName
neverQualifyNames _ _ = NameUnqual

alwaysQualifyModules :: QueryQualifyModule
alwaysQualifyModules _ = True

neverQualifyModules :: QueryQualifyModule
neverQualifyModules _ = False

alwaysQualifyPackages :: QueryQualifyPackage
alwaysQualifyPackages _ = True

neverQualifyPackages :: QueryQualifyPackage
neverQualifyPackages _ = False

reallyAlwaysQualify, alwaysQualify, neverQualify :: PrintUnqualified
reallyAlwaysQualify
              = QueryQualify reallyAlwaysQualifyNames
                             alwaysQualifyModules
                             alwaysQualifyPackages
alwaysQualify = QueryQualify alwaysQualifyNames
                             alwaysQualifyModules
                             alwaysQualifyPackages
neverQualify  = QueryQualify neverQualifyNames
                             neverQualifyModules
                             neverQualifyPackages

defaultUserStyle :: DynFlags -> PprStyle
defaultUserStyle dflags = mkUserStyle dflags neverQualify AllTheWay

defaultDumpStyle :: DynFlags -> PprStyle
 -- Print without qualifiers to reduce verbosity, unless -dppr-debug
defaultDumpStyle dflags
   | hasPprDebug dflags = PprDebug
   | otherwise          = PprDump neverQualify

mkDumpStyle :: DynFlags -> PrintUnqualified -> PprStyle
mkDumpStyle dflags print_unqual
  | hasPprDebug dflags = PprDebug
  | otherwise          = PprDump print_unqual

defaultErrStyle :: DynFlags -> PprStyle
-- Default style for error messages, when we don't know PrintUnqualified
-- It's a bit of a hack because it doesn't take into account what's in scope
-- Only used for desugarer warnings, and typechecker errors in interface sigs
-- NB that -dppr-debug will still get into PprDebug style
defaultErrStyle dflags = mkErrStyle dflags neverQualify

-- | Style for printing error messages
mkErrStyle :: DynFlags -> PrintUnqualified -> PprStyle
mkErrStyle dflags qual =
   mkUserStyle dflags qual (PartWay (pprUserLength dflags))

cmdlineParserStyle :: DynFlags -> PprStyle
cmdlineParserStyle dflags = mkUserStyle dflags alwaysQualify AllTheWay

mkUserStyle :: DynFlags -> PrintUnqualified -> Depth -> PprStyle
mkUserStyle dflags unqual depth
   | hasPprDebug dflags = PprDebug
   | otherwise          = PprUser unqual depth Uncolored

setStyleColored :: Bool -> PprStyle -> PprStyle
setStyleColored col style =
  case style of
    PprUser q d _ -> PprUser q d c
    _             -> style
  where
    c | col       = Colored
      | otherwise = Uncolored

{-
Orthogonal to the above printing styles are (possibly) some
command-line flags that affect printing (often carried with the
style).  The most likely ones are variations on how much type info is
shown.

The following test decides whether or not we are actually generating
code (either C or assembly), or generating interface files.

************************************************************************
*                                                                      *
\subsection{The @SDoc@ data type}
*                                                                      *
************************************************************************
-}

newtype SDoc = SDoc { runSDoc :: SDocContext -> Doc }

data SDocContext = SDC
  { sdocStyle      :: !PprStyle
  , sdocLastColor :: !Col.PprColor
    -- ^ The most recently used color.  This allows nesting colors.
  , sdocDynFlags   :: !DynFlags
  }

instance IsString SDoc where
  fromString = text

initSDocContext :: DynFlags -> PprStyle -> SDocContext
initSDocContext dflags sty = SDC
  { sdocStyle = sty
  , sdocLastColor = Col.colReset
  , sdocDynFlags = dflags
  }

withPprStyle :: PprStyle -> SDoc -> SDoc
withPprStyle sty d = SDoc $ \ctxt -> runSDoc d ctxt{sdocStyle=sty}

withPprStyleDoc :: DynFlags -> PprStyle -> SDoc -> Doc
withPprStyleDoc dflags sty d = runSDoc d (initSDocContext dflags sty)

pprDeeper :: SDoc -> SDoc
pprDeeper d = SDoc $ \ctx -> case ctx of
  SDC{sdocStyle=PprUser _ (PartWay 0) _} -> Pretty.text "..."
  SDC{sdocStyle=PprUser q (PartWay n) c} ->
    runSDoc d ctx{sdocStyle = PprUser q (PartWay (n-1)) c}
  _ -> runSDoc d ctx

pprDeeperList :: ([SDoc] -> SDoc) -> [SDoc] -> SDoc
-- Truncate a list that list that is longer than the current depth
pprDeeperList f ds
  | null ds   = f []
  | otherwise = SDoc work
 where
  work ctx@SDC{sdocStyle=PprUser q (PartWay n) c}
   | n==0      = Pretty.text "..."
   | otherwise =
      runSDoc (f (go 0 ds)) ctx{sdocStyle = PprUser q (PartWay (n-1)) c}
   where
     go _ [] = []
     go i (d:ds) | i >= n    = [text "...."]
                 | otherwise = d : go (i+1) ds
  work other_ctx = runSDoc (f ds) other_ctx

pprSetDepth :: Depth -> SDoc -> SDoc
pprSetDepth depth doc = SDoc $ \ctx ->
    case ctx of
        SDC{sdocStyle=PprUser q _ c} ->
            runSDoc doc ctx{sdocStyle = PprUser q depth c}
        _ ->
            runSDoc doc ctx

getPprStyle :: (PprStyle -> SDoc) -> SDoc
getPprStyle df = SDoc $ \ctx -> runSDoc (df (sdocStyle ctx)) ctx

sdocWithDynFlags :: (DynFlags -> SDoc) -> SDoc
sdocWithDynFlags f = SDoc $ \ctx -> runSDoc (f (sdocDynFlags ctx)) ctx

sdocWithPlatform :: (Platform -> SDoc) -> SDoc
sdocWithPlatform f = sdocWithDynFlags (f . targetPlatform)

qualName :: PprStyle -> QueryQualifyName
qualName (PprUser q _ _)  mod occ = queryQualifyName q mod occ
qualName (PprDump q)      mod occ = queryQualifyName q mod occ
qualName _other           mod _   = NameQual (moduleName mod)

qualModule :: PprStyle -> QueryQualifyModule
qualModule (PprUser q _ _)  m = queryQualifyModule q m
qualModule (PprDump q)      m = queryQualifyModule q m
qualModule _other          _m = True

qualPackage :: PprStyle -> QueryQualifyPackage
qualPackage (PprUser q _ _)  m = queryQualifyPackage q m
qualPackage (PprDump q)      m = queryQualifyPackage q m
qualPackage _other          _m = True

queryQual :: PprStyle -> PrintUnqualified
queryQual s = QueryQualify (qualName s)
                           (qualModule s)
                           (qualPackage s)

codeStyle :: PprStyle -> Bool
codeStyle (PprCode _)     = True
codeStyle _               = False

asmStyle :: PprStyle -> Bool
asmStyle (PprCode AsmStyle)  = True
asmStyle _other              = False

dumpStyle :: PprStyle -> Bool
dumpStyle (PprDump {}) = True
dumpStyle _other       = False

debugStyle :: PprStyle -> Bool
debugStyle PprDebug = True
debugStyle _other   = False

userStyle ::  PprStyle -> Bool
userStyle (PprUser {}) = True
userStyle _other       = False

ifPprDebug :: SDoc -> SDoc        -- Empty for non-debug style
ifPprDebug d = SDoc $ \ctx ->
    case ctx of
        SDC{sdocStyle=PprDebug} -> runSDoc d ctx
        _                       -> Pretty.empty

-- | The analog of 'Pretty.printDoc_' for 'SDoc', which tries to make sure the
--   terminal doesn't get screwed up by the ANSI color codes if an exception
--   is thrown during pretty-printing.
printSDoc :: Mode -> DynFlags -> Handle -> PprStyle -> SDoc -> IO ()
printSDoc mode dflags handle sty doc =
  Pretty.printDoc_ mode cols handle (runSDoc doc ctx)
    `finally`
      Pretty.printDoc_ mode cols handle
        (runSDoc (colored Col.colReset empty) ctx)
  where
    cols = pprCols dflags
    ctx = initSDocContext dflags sty

-- | Like 'printSDoc' but appends an extra newline.
printSDocLn :: Mode -> DynFlags -> Handle -> PprStyle -> SDoc -> IO ()
printSDocLn mode dflags handle sty doc =
  printSDoc mode dflags handle sty (doc $$ text "")

-- printForUser :: DynFlags -> Handle -> PrintUnqualified -> SDoc -> IO ()
-- printForUser dflags handle unqual doc
--   = Pretty.printDoc PageMode (pprCols dflags) handle
--       (runSDoc doc (initSDocContext dflags (mkUserStyle unqual AllTheWay)))

printForUser :: DynFlags -> Handle -> PrintUnqualified -> SDoc -> IO ()
printForUser dflags handle unqual doc
  = printSDocLn PageMode dflags handle
               (mkUserStyle dflags unqual AllTheWay) doc

printForUserColored :: DynFlags -> Handle -> PrintUnqualified -> SDoc -> IO ()
printForUserColored dflags handle unqual doc
  = printSDocLn PageMode dflags handle
      (setStyleColored True (mkUserStyle dflags unqual AllTheWay)) doc

printForUserPartWay :: DynFlags -> Handle -> Int -> PrintUnqualified -> SDoc
                    -> IO ()
printForUserPartWay dflags handle d unqual doc
  = printSDocLn PageMode dflags handle
                (mkUserStyle dflags unqual (PartWay d)) doc

-- printForC, printForAsm do what they sound like
printForC :: DynFlags -> Handle -> SDoc -> IO ()
printForC dflags handle doc =
  Pretty.printDoc LeftMode (pprCols dflags) handle
    (runSDoc doc (initSDocContext dflags (PprCode CStyle)))

printForAsm :: DynFlags -> Handle -> SDoc -> IO ()
printForAsm dflags handle doc =
  Pretty.printDoc LeftMode (pprCols dflags) handle
    (runSDoc doc (initSDocContext dflags (PprCode AsmStyle)))

pprCode :: CodeStyle -> SDoc -> SDoc
pprCode cs d = withPprStyle (PprCode cs) d

mkCodeStyle :: CodeStyle -> PprStyle
mkCodeStyle = PprCode

-- Can't make SDoc an instance of Show because SDoc is just a function type
-- However, Doc *is* an instance of Show
-- showSDoc just blasts it out as a string
showSDoc :: DynFlags -> SDoc -> String
showSDoc dflags sdoc = renderWithStyle dflags sdoc (defaultUserStyle dflags)

showSDocWithColor :: DynFlags -> SDoc -> String
showSDocWithColor dflags sdoc = renderWithStyle dflags sdoc (setStyleColored True (defaultUserStyle dflags))

-- showSDocUnsafe is unsafe, because `unsafeGlobalDynFlags` might not be
-- initialised yet.
showSDocUnsafe :: SDoc -> String
showSDocUnsafe sdoc = showSDoc unsafeGlobalDynFlags sdoc

showPpr :: Outputable a => DynFlags -> a -> String
showPpr dflags thing = showSDoc dflags (ppr thing)

showSDocUnqual :: DynFlags -> SDoc -> String
-- Only used by Haddock
showSDocUnqual dflags sdoc = showSDoc dflags sdoc

showSDocForUser :: DynFlags -> PrintUnqualified -> SDoc -> String
-- Allows caller to specify the PrintUnqualified to use
showSDocForUser dflags unqual doc
 = renderWithStyle dflags doc (mkUserStyle dflags unqual AllTheWay)

showSDocForUserColored :: DynFlags -> PrintUnqualified -> SDoc -> String
-- Allows caller to specify the PrintUnqualified to use
showSDocForUserColored dflags unqual doc
 = renderWithStyle dflags doc (setStyleColored True (mkUserStyle dflags unqual AllTheWay))

showSDocDump :: DynFlags -> SDoc -> String
showSDocDump dflags d = renderWithStyle dflags d (defaultDumpStyle dflags)

showSDocDebug :: DynFlags -> SDoc -> String
showSDocDebug dflags d = renderWithStyle dflags d PprDebug

renderWithStyle :: DynFlags -> SDoc -> PprStyle -> String
renderWithStyle dflags sdoc sty
  = Pretty.showDoc PageMode (pprCols dflags) $
    runSDoc sdoc (initSDocContext dflags sty)

-- This shows an SDoc, but on one line only. It's cheaper than a full
-- showSDoc, designed for when we're getting results like "Foo.bar"
-- and "foo{uniq strictness}" so we don't want fancy layout anyway.
showSDocOneLine :: DynFlags -> SDoc -> String
showSDocOneLine dflags d
 = Pretty.showDoc OneLineMode (pprCols dflags) $
   runSDoc d (initSDocContext dflags (defaultUserStyle dflags))

showSDocDumpOneLine :: DynFlags -> SDoc -> String
showSDocDumpOneLine dflags d
 = Pretty.showDoc OneLineMode irrelevantNCols $
   runSDoc d (initSDocContext dflags (defaultDumpStyle dflags))

irrelevantNCols :: Int
-- Used for OneLineMode and LeftMode when number of cols isn't used
irrelevantNCols = 1

docToSDoc :: Doc -> SDoc
docToSDoc d = SDoc (\_ -> d)

empty    :: SDoc
char     :: Char       -> SDoc
text     :: String     -> SDoc
ftext    :: FastString -> SDoc
ptext    :: LitString  -> SDoc
ztext    :: FastZString -> SDoc
int      :: Int        -> SDoc
integer  :: Integer    -> SDoc
float    :: Float      -> SDoc
double   :: Double     -> SDoc
rational :: Rational   -> SDoc

empty       = docToSDoc $ Pretty.empty
char c      = docToSDoc $ Pretty.char c

text s      = docToSDoc $ Pretty.text s
{-# INLINE text #-}   -- Inline so that the RULE Pretty.text will fire

ftext s     = docToSDoc $ Pretty.ftext s
ptext s     = docToSDoc $ Pretty.ptext s
ztext s     = docToSDoc $ Pretty.ztext s
int n       = docToSDoc $ Pretty.int n
integer n   = docToSDoc $ Pretty.integer n
float n     = docToSDoc $ Pretty.float n
double n    = docToSDoc $ Pretty.double n
rational n  = docToSDoc $ Pretty.rational n

parens, braces, brackets, quotes, quote,
        paBrackets, doubleQuotes, angleBrackets :: SDoc -> SDoc

parens d        = SDoc $ Pretty.parens . runSDoc d
braces d        = SDoc $ Pretty.braces . runSDoc d
brackets d      = SDoc $ Pretty.brackets . runSDoc d
quote d         = SDoc $ Pretty.quote . runSDoc d
doubleQuotes d  = SDoc $ Pretty.doubleQuotes . runSDoc d
angleBrackets d = char '<' <> d <> char '>'
paBrackets d    = ptext (sLit "[:") <> d <> ptext (sLit ":]")

cparen :: Bool -> SDoc -> SDoc

cparen b d     = SDoc $ Pretty.cparen b . runSDoc d

-- 'quotes' encloses something in single quotes...
-- but it omits them if the thing begins or ends in a single quote
-- so that we don't get `foo''.  Instead we just have foo'.
quotes d =
      sdocWithDynFlags $ \dflags ->
      if useUnicode dflags
      then char '‘' <> d <> char '’'
      else SDoc $ \sty ->
           let pp_d = runSDoc d sty
               str  = show pp_d
           in case (str, snocView str) of
             (_, Just (_, '\'')) -> pp_d
             ('\'' : _, _)       -> pp_d
             _other              -> Pretty.quotes pp_d

semi, comma, colon, equals, space, dcolon, underscore, dot, vbar :: SDoc
arrow, larrow, darrow, arrowt, larrowt, arrowtt, larrowtt :: SDoc
lparen, rparen, lbrack, rbrack, lbrace, rbrace, blankLine :: SDoc

blankLine  = docToSDoc $ Pretty.ptext (sLit "")
dcolon     = unicodeSyntax (char '∷') (docToSDoc $ Pretty.ptext (sLit "::"))
arrow      = unicodeSyntax (char '→') (docToSDoc $ Pretty.ptext (sLit "->"))
larrow     = unicodeSyntax (char '←') (docToSDoc $ Pretty.ptext (sLit "<-"))
darrow     = unicodeSyntax (char '⇒') (docToSDoc $ Pretty.ptext (sLit "=>"))
arrowt     = unicodeSyntax (char '↣') (docToSDoc $ Pretty.ptext (sLit ">-"))
larrowt    = unicodeSyntax (char '↢') (docToSDoc $ Pretty.ptext (sLit "-<"))
arrowtt    = unicodeSyntax (char '⤜') (docToSDoc $ Pretty.ptext (sLit ">>-"))
larrowtt   = unicodeSyntax (char '⤛') (docToSDoc $ Pretty.ptext (sLit "-<<"))
semi       = docToSDoc $ Pretty.semi
comma      = docToSDoc $ Pretty.comma
colon      = docToSDoc $ Pretty.colon
equals     = docToSDoc $ Pretty.equals
space      = docToSDoc $ Pretty.space
underscore = char '_'
dot        = char '.'
vbar       = char '|'
lparen     = docToSDoc $ Pretty.lparen
rparen     = docToSDoc $ Pretty.rparen
lbrack     = docToSDoc $ Pretty.lbrack
rbrack     = docToSDoc $ Pretty.rbrack
lbrace     = docToSDoc $ Pretty.lbrace
rbrace     = docToSDoc $ Pretty.rbrace

forAllLit :: SDoc
forAllLit = unicodeSyntax (char '∀') (ptext (sLit "forall"))

unicodeSyntax :: SDoc -> SDoc -> SDoc
unicodeSyntax unicode plain = sdocWithDynFlags $ \dflags ->
    if useUnicode dflags && useUnicodeSyntax dflags
    then unicode
    else plain

nest :: Int -> SDoc -> SDoc
-- ^ Indent 'SDoc' some specified amount
(<>) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together horizontally without a gap
(<+>) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together horizontally with a gap between them
($$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically; if there is
-- no vertical overlap it "dovetails" the two onto one line
($+$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically

nest n d    = SDoc $ Pretty.nest n . runSDoc d
(<>) d1 d2  = SDoc $ \sty -> (Pretty.<>)  (runSDoc d1 sty) (runSDoc d2 sty)
(<+>) d1 d2 = SDoc $ \sty -> (Pretty.<+>) (runSDoc d1 sty) (runSDoc d2 sty)
($$) d1 d2  = SDoc $ \sty -> (Pretty.$$)  (runSDoc d1 sty) (runSDoc d2 sty)
($+$) d1 d2 = SDoc $ \sty -> (Pretty.$+$) (runSDoc d1 sty) (runSDoc d2 sty)

hcat :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' horizontally
hsep :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' horizontally with a space between each one
vcat :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' vertically with dovetailing
sep :: [SDoc] -> SDoc
-- ^ Separate: is either like 'hsep' or like 'vcat', depending on what fits
cat :: [SDoc] -> SDoc
-- ^ Catenate: is either like 'hcat' or like 'vcat', depending on what fits
fsep :: [SDoc] -> SDoc
-- ^ A paragraph-fill combinator. It's much like sep, only it
-- keeps fitting things on one line until it can't fit any more.
fcat :: [SDoc] -> SDoc
-- ^ This behaves like 'fsep', but it uses '<>' for horizontal composition rather than '<+>'


hcat ds = SDoc $ \sty -> Pretty.hcat [runSDoc d sty | d <- ds]
hsep ds = SDoc $ \sty -> Pretty.hsep [runSDoc d sty | d <- ds]
vcat ds = SDoc $ \sty -> Pretty.vcat [runSDoc d sty | d <- ds]
sep ds  = SDoc $ \sty -> Pretty.sep  [runSDoc d sty | d <- ds]
cat ds  = SDoc $ \sty -> Pretty.cat  [runSDoc d sty | d <- ds]
fsep ds = SDoc $ \sty -> Pretty.fsep [runSDoc d sty | d <- ds]
fcat ds = SDoc $ \sty -> Pretty.fcat [runSDoc d sty | d <- ds]

hang :: SDoc  -- ^ The header
      -> Int  -- ^ Amount to indent the hung body
      -> SDoc -- ^ The hung body, indented and placed below the header
      -> SDoc
hang d1 n d2   = SDoc $ \sty -> Pretty.hang (runSDoc d1 sty) n (runSDoc d2 sty)

punctuate :: SDoc   -- ^ The punctuation
          -> [SDoc] -- ^ The list that will have punctuation added between every adjacent pair of elements
          -> [SDoc] -- ^ Punctuated list
punctuate _ []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

ppWhen, ppUnless :: Bool -> SDoc -> SDoc
ppWhen True  doc = doc
ppWhen False _   = empty

ppUnless True  _   = empty
ppUnless False doc = doc

-- | Apply the given color\/style for the argument.
--
-- Only takes effect if colors are enabled.
colored :: Col.PprColor -> SDoc -> SDoc
colored col sdoc =
  sdocWithDynFlags $ \dflags ->
    if shouldUseColor dflags
    then SDoc $ \ctx@SDC{ sdocLastColor = lastCol } ->
         case ctx of
           SDC{ sdocStyle = PprUser _ _ Colored } ->
             let ctx' = ctx{ sdocLastColor = lastCol `mappend` col } in
             Pretty.zeroWidthText (Col.renderColor col)
               Pretty.<> runSDoc sdoc ctx'
               Pretty.<> Pretty.zeroWidthText (Col.renderColorAfresh lastCol)
           _ -> runSDoc sdoc ctx
    else sdoc

keyword :: SDoc -> SDoc
keyword = colored Col.colBold

{-
************************************************************************
*                                                                      *
\subsection[Outputable-class]{The @Outputable@ class}
*                                                                      *
************************************************************************
-}

-- | Class designating that some type has an 'SDoc' representation
class Outputable a where
        ppr :: a -> SDoc
        pprPrec :: Rational -> a -> SDoc
                -- 0 binds least tightly
                -- We use Rational because there is always a
                -- Rational between any other two Rationals

        ppr = pprPrec 0
        pprPrec _ = ppr

instance Outputable Char where
    ppr c = text [c]

instance Outputable Bool where
    ppr True  = ptext (sLit "True")
    ppr False = ptext (sLit "False")

instance Outputable Int32 where
   ppr n = integer $ fromIntegral n

instance Outputable Int64 where
   ppr n = integer $ fromIntegral n

instance Outputable Int where
    ppr n = int n

instance Outputable Word16 where
    ppr n = integer $ fromIntegral n

instance Outputable Word32 where
    ppr n = integer $ fromIntegral n

instance Outputable Word where
    ppr n = integer $ fromIntegral n

instance Outputable () where
    ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a) => Outputable (Set a) where
    ppr s = braces (fsep (punctuate comma (map ppr (Set.toList s))))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance Outputable a => Outputable (Maybe a) where
    ppr Nothing = ptext (sLit "Nothing")
    ppr (Just x) = ptext (sLit "Just") <+> ppr x

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    ppr (Left x)  = ptext (sLit "Left")  <+> ppr x
    ppr (Right y) = ptext (sLit "Right") <+> ppr y

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
                   ppr y <> comma,
                   ppr z ])

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
         Outputable (a, b, c, d) where
    ppr (a,b,c,d) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e) =>
         Outputable (a, b, c, d, e) where
    ppr (a,b,c,d,e) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f) =>
         Outputable (a, b, c, d, e, f) where
    ppr (a,b,c,d,e,f) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f, Outputable g) =>
         Outputable (a, b, c, d, e, f, g) where
    ppr (a,b,c,d,e,f,g) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f <> comma,
                   ppr g])

instance Outputable FastString where
    ppr fs = ftext fs           -- Prints an unadorned string,
                                -- no double quotes or anything

instance (Outputable key, Outputable elt) => Outputable (M.Map key elt) where
    ppr m = ppr (M.toList m)
instance (Outputable elt) => Outputable (IM.IntMap elt) where
    ppr m = ppr (IM.toList m)

instance Outputable Fingerprint where
    ppr (Fingerprint w1 w2) = text (printf "%016x%016x" w1 w2)

instance Outputable Serialized where
    ppr (Serialized the_type bytes) = int (length bytes) <+> ptext (sLit "of type")
      <+> text (show the_type)

instance Outputable (ForeignRef a) where
  ppr hv = ppr (foreignRefToInt hv)

{-
************************************************************************
*                                                                      *
\subsection{The @OutputableBndr@ class}
*                                                                      *
************************************************************************
-}

-- | 'BindingSite' is used to tell the thing that prints binder what
-- language construct is binding the identifier.  This can be used
-- to decide how much info to print.
data BindingSite = LambdaBind | CaseBind | LetBind

-- | When we print a binder, we often want to print its type too.
-- The @OutputableBndr@ class encapsulates this idea.
class Outputable a => OutputableBndr a where
   pprBndr :: BindingSite -> a -> SDoc
   pprBndr _b x = ppr x

   pprPrefixOcc, pprInfixOcc :: a -> SDoc
      -- Print an occurrence of the name, suitable either in the
      -- prefix position of an application, thus   (f a b) or  ((+) x)
      -- or infix position,                 thus   (a `f` b) or  (x + y)

{-
************************************************************************
*                                                                      *
\subsection{Random printing helpers}
*                                                                      *
************************************************************************
-}

-- We have 31-bit Chars and will simply use Show instances of Char and String.

-- | Special combinator for showing character literals.
pprHsChar :: Char -> SDoc
pprHsChar c | c > '\x10ffff' = char '\\' <> text (show (fromIntegral (ord c) :: Word32))
            | otherwise      = text (show c)

-- | Special combinator for showing string literals.
pprHsString :: FastString -> SDoc
pprHsString fs = vcat (map text (showMultiLineString (unpackFS fs)))

-- | Special combinator for showing string literals.
pprHsBytes :: ByteString -> SDoc
pprHsBytes bs = let escaped = concatMap escape $ BS.unpack bs
                in vcat (map text (showMultiLineString escaped)) <> char '#'
    where escape :: Word8 -> String
          escape w = let c = chr (fromIntegral w)
                     in if isAscii c
                        then [c]
                        else '\\' : show w

---------------------
-- Put a name in parens if it's an operator
pprPrefixVar :: Bool -> SDoc -> SDoc
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise   = pp_v

-- Put a name in backquotes if it's not an operator
pprInfixVar :: Bool -> SDoc -> SDoc
pprInfixVar is_operator pp_v
  | is_operator = pp_v
  | otherwise   = char '`' <> pp_v <> char '`'

---------------------
pprFastFilePath :: FastString -> SDoc
pprFastFilePath path = text $ normalise $ unpackFS path

{-
************************************************************************
*                                                                      *
\subsection{Other helper functions}
*                                                                      *
************************************************************************
-}

pprWithCommas :: (a -> SDoc) -- ^ The pretty printing function to use
              -> [a]         -- ^ The things to be pretty printed
              -> SDoc        -- ^ 'SDoc' where the things have been pretty printed,
                             -- comma-separated and finally packed into a paragraph.
pprWithCommas pp xs = fsep (punctuate comma (map pp xs))

-- | Returns the separated concatenation of the pretty printed things.
interppSP  :: Outputable a => [a] -> SDoc
interppSP  xs = sep (map ppr xs)

-- | Returns the comma-separated concatenation of the pretty printed things.
interpp'SP :: Outputable a => [a] -> SDoc
interpp'SP xs = sep (punctuate comma (map ppr xs))

-- | Returns the comma-separated concatenation of the quoted pretty printed things.
--
-- > [x,y,z]  ==>  `x', `y', `z'
pprQuotedList :: Outputable a => [a] -> SDoc
pprQuotedList = quotedList . map ppr

quotedList :: [SDoc] -> SDoc
quotedList xs = hsep (punctuate comma (map quotes xs))

quotedListWithOr :: [SDoc] -> SDoc
-- [x,y,z]  ==>  `x', `y' or `z'
quotedListWithOr xs@(_:_:_) = quotedList (init xs) <+> ptext (sLit "or") <+> quotes (last xs)
quotedListWithOr xs = quotedList xs

quotedListWithNor :: [SDoc] -> SDoc
-- [x,y,z]  ==>  `x', `y' nor `z'
quotedListWithNor xs@(_:_:_) = quotedList (init xs) <+> text "nor" <+> quotes (last xs)
quotedListWithNor xs = quotedList xs

{-
************************************************************************
*                                                                      *
\subsection{Printing numbers verbally}
*                                                                      *
************************************************************************
-}

intWithCommas :: Integral a => a -> SDoc
-- Prints a big integer with commas, eg 345,821
intWithCommas n
  | n < 0     = char '-' <> intWithCommas (-n)
  | q == 0    = int (fromIntegral r)
  | otherwise = intWithCommas q <> comma <> zeroes <> int (fromIntegral r)
  where
    (q,r) = n `quotRem` 1000
    zeroes | r >= 100  = empty
           | r >= 10   = char '0'
           | otherwise = ptext (sLit "00")

-- | Converts an integer to a verbal index:
--
-- > speakNth 1 = text "first"
-- > speakNth 5 = text "fifth"
-- > speakNth 21 = text "21st"
speakNth :: Int -> SDoc
speakNth 1 = ptext (sLit "first")
speakNth 2 = ptext (sLit "second")
speakNth 3 = ptext (sLit "third")
speakNth 4 = ptext (sLit "fourth")
speakNth 5 = ptext (sLit "fifth")
speakNth 6 = ptext (sLit "sixth")
speakNth n = hcat [ int n, text suffix ]
  where
    suffix | n <= 20       = "th"       -- 11,12,13 are non-std
           | last_dig == 1 = "st"
           | last_dig == 2 = "nd"
           | last_dig == 3 = "rd"
           | otherwise     = "th"

    last_dig = n `rem` 10

-- | Converts an integer to a verbal multiplicity:
--
-- > speakN 0 = text "none"
-- > speakN 5 = text "five"
-- > speakN 10 = text "10"
speakN :: Int -> SDoc
speakN 0 = ptext (sLit "none")  -- E.g.  "he has none"
speakN 1 = ptext (sLit "one")   -- E.g.  "he has one"
speakN 2 = ptext (sLit "two")
speakN 3 = ptext (sLit "three")
speakN 4 = ptext (sLit "four")
speakN 5 = ptext (sLit "five")
speakN 6 = ptext (sLit "six")
speakN n = int n

-- | Converts an integer and object description to a statement about the
-- multiplicity of those objects:
--
-- > speakNOf 0 (text "melon") = text "no melons"
-- > speakNOf 1 (text "melon") = text "one melon"
-- > speakNOf 3 (text "melon") = text "three melons"
speakNOf :: Int -> SDoc -> SDoc
speakNOf 0 d = ptext (sLit "no") <+> d <> char 's'
speakNOf 1 d = ptext (sLit "one") <+> d                 -- E.g. "one argument"
speakNOf n d = speakN n <+> d <> char 's'               -- E.g. "three arguments"

speakNCaps :: Int -> SDoc
speakNCaps 0 = ptext (sLit "None")  -- E.g.  "he has none"
speakNCaps 1 = ptext (sLit "One")   -- E.g.  "he has one"
speakNCaps 2 = ptext (sLit "Two")
speakNCaps 3 = ptext (sLit "Three")
speakNCaps 4 = ptext (sLit "Four")
speakNCaps 5 = ptext (sLit "Five")
speakNCaps 6 = ptext (sLit "Six")
speakNCaps n = int n

speakNOfCaps :: Int -> SDoc -> SDoc
speakNOfCaps 0 d = ptext (sLit "No") <+> d <> char 's'
speakNOfCaps 1 d = ptext (sLit "One") <+> d                 -- E.g. "one argument"
speakNOfCaps n d = speakNCaps n <+> d <> char 's'               -- E.g. "three arguments"

-- | Converts a strictly positive integer into a number of times:
--
-- > speakNTimes 1 = text "once"
-- > speakNTimes 2 = text "twice"
-- > speakNTimes 4 = text "4 times"
speakNTimes :: Int {- >=1 -} -> SDoc
speakNTimes t | t == 1     = ptext (sLit "once")
              | t == 2     = ptext (sLit "twice")
              | otherwise  = speakN t <+> ptext (sLit "times")

-- | Determines the pluralisation suffix appropriate for the length of a list:
--
-- > plural [] = char 's'
-- > plural ["Hello"] = empty
-- > plural ["Hello", "World"] = char 's'
plural :: [a] -> SDoc
plural [_] = empty  -- a bit frightening, but there you are
plural _   = char 's'

-- | Determines the form of to be appropriate for the length of a list:
--
-- > isOrAre [] = ptext (sLit "are")
-- > isOrAre ["Hello"] = ptext (sLit "is")
-- > isOrAre ["Hello", "World"] = ptext (sLit "are")
isOrAre :: [a] -> SDoc
isOrAre [_] = ptext (sLit "is")
isOrAre _   = ptext (sLit "are")

{-
************************************************************************
*                                                                      *
\subsection{Error handling}
*                                                                      *
************************************************************************
-}

pprPanic :: String -> SDoc -> a
-- ^ Throw an exception saying "bug in GHC"
pprPanic    = panicDoc

pprSorry :: String -> SDoc -> a
-- ^ Throw an exception saying "this isn't finished yet"
pprSorry    = sorryDoc


pprPgmError :: String -> SDoc -> a
-- ^ Throw an exception saying "bug in pgm being compiled" (used for unusual program errors)
pprPgmError = pgmErrorDoc

pprTraceDebug :: String -> SDoc -> a -> a
pprTraceDebug str doc x
   | debugIsOn && hasPprDebug unsafeGlobalDynFlags = pprTrace str doc x
   | otherwise                                     = x

pprTrace :: String -> SDoc -> a -> a
-- ^ If debug output is on, show some 'SDoc' on the screen
pprTrace str doc x
   | opt_NoDebugOutput = x
   | otherwise         = pprDebugAndThen unsafeGlobalDynFlags trace (text str) doc x

pprPanicFastInt :: String -> SDoc -> FastInt
-- ^ Specialization of pprPanic that can be safely used with 'FastInt'
pprPanicFastInt heading pretty_msg = panicDocFastInt heading pretty_msg

warnPprTrace :: Bool -> String -> Int -> SDoc -> a -> a
-- ^ Just warn about an assertion failure, recording the given file and line number.
-- Should typically be accessed with the WARN macros
warnPprTrace _     _     _     _    x | not debugIsOn     = x
warnPprTrace _     _file _line _msg x | opt_NoDebugOutput = x
warnPprTrace False _file _line _msg x = x
warnPprTrace True   file  line  msg x
  = pprDebugAndThen unsafeGlobalDynFlags trace heading msg x
  where
    heading = hsep [text "WARNING: file", text file <> comma, text "line", int line]

assertPprPanic :: String -> Int -> SDoc -> a
-- ^ Panic with an assertion failure, recording the given file and line number.
-- Should typically be accessed with the ASSERT family of macros
assertPprPanic file line msg
  = pprPanic "ASSERT failed!" doc
  where
    doc = sep [ hsep [ text "file", text file
                     , text "line", int line ]
              , msg ]

pprDebugAndThen :: DynFlags -> (String -> a) -> SDoc -> SDoc -> a
pprDebugAndThen dflags cont heading pretty_msg
 = cont (showSDocDump dflags doc)
 where
     doc = sep [heading, nest 2 pretty_msg]
