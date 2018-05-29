{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[Constants]{Info about this compilation}
-}
{-# LANGUAGE TemplateHaskell #-}
module Eta.Main.Constants where

import Paths_eta (version)
import Data.Version (versionBranch)
import Development.GitRev

hiVersion :: Integer
hiVersion = read cProjectVersionInt :: Integer

cProjectName, cProjectVersion, cProjectVersionNumbers, cProjectVersionInt, cProjectPatchLevel, cProjectPatchLevel1, cProjectPatchLevel2, cProjectHomeURL, cProjectIssueReportURL, ghcProjectVersion, ghcProjectVersionInt, ghcprojectPatchLevel, ghcProjectPatchLevel1, ghcProjectPatchLevel2, cProjectGitCommitId
  :: String
cProjectName = "The Eta Programming Language Compiler"
cProjectVersion = show maj ++ "." ++ show minor ++ "." ++ show patch ++ "b"
               ++ cProjectPatchLevel
  where [maj,minor,patch,_] = versionBranch version
cProjectVersionNumbers = show maj ++ "." ++ show minor ++ "." ++ show patch ++ "."
                     ++ cProjectPatchLevel
  where [maj,minor,patch,_] = versionBranch version
cProjectVersionInt = show (maj * 100 + minor * 10 + patch)
  where [maj,minor,patch,_] = versionBranch version

cProjectPatchLevel = show (last (versionBranch version))
cProjectPatchLevel1 = cProjectPatchLevel
cProjectPatchLevel2 = ""
cProjectHomeURL = "http://github.com/typelead/eta"
cProjectIssueReportURL = cProjectHomeURL ++ "/issues"
cProjectGitCommitId = $(gitHash)

ghcProjectVersion = "7.10.3"
ghcProjectVersionInt = "710"
ghcprojectPatchLevel = "3"
ghcProjectPatchLevel1 = "3"
ghcProjectPatchLevel2 = ""

-- All pretty arbitrary:

mAX_TUPLE_SIZE :: Int
mAX_TUPLE_SIZE = 62 -- Should really match the number
                    -- of decls in Data.Tuple

mAX_CONTEXT_REDUCTION_DEPTH :: Int
mAX_CONTEXT_REDUCTION_DEPTH = 100
  -- Trac #5395 reports at least one library that needs depth 37 here

mAX_TYPE_FUNCTION_REDUCTION_DEPTH :: Int
mAX_TYPE_FUNCTION_REDUCTION_DEPTH = 200
  -- Needs to be much higher than mAX_CONTEXT_REDUCTION_DEPTH; see Trac #5395

wORD64_SIZE :: Int
wORD64_SIZE = 8

tARGET_MAX_CHAR :: Int
tARGET_MAX_CHAR = 0x10ffff

mAX_INTLIKE, mIN_INTLIKE, mAX_CHARLIKE, mIN_CHARLIKE, mAX_SPEC_AP_SIZE :: Int
mIN_INTLIKE = -16
mAX_INTLIKE = 16
mIN_CHARLIKE = 0
mAX_CHARLIKE = 255
mAX_SPEC_AP_SIZE = 7
