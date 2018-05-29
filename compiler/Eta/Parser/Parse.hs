module Eta.Parser.Parse where

import Eta.Main.HscStats
import Eta.Main.HscTypes
import Eta.Main.DynFlags
import Eta.Main.ErrUtils
import Eta.Utils.MonadUtils
import Eta.BasicTypes.SrcLoc
import Eta.Utils.FastString
import Eta.Utils.StringBuffer
import Eta.Utils.Outputable
import Eta.BasicTypes.Module
import Eta.Utils.Bag

import Eta.Parser.Parser
import Eta.Parser.Lexer

import Data.List
import Control.Monad
import Control.Exception (throwIO)
import System.Directory
import qualified Data.Map as Map
import System.FilePath as FilePath

hscParse' :: ModSummary -> Hsc HsParsedModule
hscParse' mod_summary = do
    dflags <- getDynFlags
    let src_filename  = ms_hspp_file mod_summary
        maybe_src_buf = ms_hspp_buf  mod_summary

    --------------------------  Parser  ----------------
    liftIO $ showPass dflags "Parser"
    {-# SCC "Parser" #-} do

      -- sometimes we already have the buffer in memory, perhaps
      -- because we needed to parse the imports out of it, or get the
      -- module name.
      buf <- case maybe_src_buf of
                Just b  -> return b
                Nothing -> liftIO $ hGetStringBuffer src_filename

      let loc = mkRealSrcLoc (mkFastString src_filename) 1 1

      case unP parseModule (mkPState dflags buf loc) of
          PFailed span err ->
              liftIO $ throwOneError (mkPlainErrMsg dflags span err)

          POk pst rdr_module -> do
              logWarningsReportErrors (getMessages pst)
              liftIO $ dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" $
                                    ppr rdr_module
              liftIO $ dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics" $
                                    ppSourceStats False rdr_module

              -- To get the list of extra source files, we take the list
              -- that the parser gave us,
              --   - eliminate files beginning with '<'.  gcc likes to use
              --     pseudo-filenames like "<built-in>" and "<command-line>"
              --   - normalise them (elimiante differences between ./f and f)
              --   - filter out the preprocessed source file
              --   - filter out anything beginning with tmpdir
              --   - remove duplicates
              --   - filter out the .hs/.lhs source filename if we have one
              --
              let n_hspp  = FilePath.normalise src_filename
                  srcs0 = nub $ filter (not . (tmpDir dflags `isPrefixOf`))
                              $ filter (not . (== n_hspp))
                              $ map FilePath.normalise
                              $ filter (not . (isPrefixOf "<"))
                              $ map unpackFS
                              $ srcfiles pst
                  srcs1 = case ml_hs_file (ms_location mod_summary) of
                            Just f  -> filter (/= FilePath.normalise f) srcs0
                            Nothing -> srcs0

              -- sometimes we see source files from earlier
              -- preprocessing stages that cannot be found, so just
              -- filter them out:
              srcs2 <- liftIO $ filterM doesFileExist srcs1

              return HsParsedModule {
                        hpm_module    = rdr_module,
                        hpm_src_files = srcs2,
                        hpm_annotations
                                = (Map.fromListWith (++) $ annotations pst,
                                  Map.fromList $ ((noSrcSpan,comment_q pst)
                                                  :(annotations_comments pst)))
                    }

-- | log warning in the monad, and if there are errors then
-- throw a SourceError exception.
logWarningsReportErrors :: Messages -> Hsc ()
logWarningsReportErrors (warns,errs) = do
    logWarnings warns
    unless (isEmptyBag errs) $ throwErrors errs

-- | Throw some errors.
throwErrors :: ErrorMessages -> Hsc a
throwErrors = liftIO . throwIO . mkSrcErr

logWarnings :: WarningMessages -> Hsc ()
logWarnings w = Hsc $ \_ w0 -> return ((), w0 `unionBags` w)
