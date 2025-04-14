-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Adam C. Foltzer
-- License     :  BSD3
-- Maintainer  :  acfoltzer@galois.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some handy Template Haskell splices for including the current git
-- hash and branch in the code of your project. Useful for including
-- in panic messages, @--version@ output, or diagnostic info for more
-- informative bug reports.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Development.GitRev
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", $(gitBranch), "@", $(gitHash)
-- >                  , " (", $(gitCommitDate), ")"
-- >                  , " (", $(gitCommitCount), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | $(gitDirty) = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >
-- > main = panic "oh no!"
--
-- > % cabal exec runhaskell Example.hs
-- > Example.hs: [panic master@2ae047ba5e4a6f0f3e705a43615363ac006099c1 (Mon Jan 11 11:50:59 2016 -0800) (14 commits in HEAD) (uncommitted files present)] oh no!
module Development.GitRev.Typed
  ( -- * Branch
    gitBranch,
    gitBranchEnv,
    gitBranchMay,
    gitBranchMayEnv,

    -- * Commit count
    gitCommitCount,
    gitCommitCountEnv,
    gitCommitCountMay,
    gitCommitCountMayEnv,

    -- * Date
    gitCommitDate,
    gitCommitDateEnv,
    gitCommitDateMay,
    gitCommitDateMayEnv,

    -- * Describe
    gitDescribe,
    gitDescribeEnv,
    gitDescribeMay,
    gitDescribeMayEnv,

    -- * Dirty
    gitDirty,

    -- * Dirty Tracked
    gitDirtyTracked,

    -- * Hash
    gitHash,
    gitHashEnv,
    gitHashMay,
    gitHashMayEnv,
  )
where

import Development.GitRev.Internal qualified as Internal
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (lift), TExp (TExp))

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
gitHash :: Code Q String
gitHash = qToCode $ Internal.unknownFallback Internal.gitHashMay

-- | 'gitHash' that attempts to read the parameter environment variable,
-- when 'gitHash' fails.
--
-- @since 1.4.0
gitHashEnv :: String -> Code Q String
gitHashEnv var = qToCode $ Internal.envUnknownFallback var Internal.gitHashMay

-- | 'gitHash' that returns that returns 'Nothing' instead of @UNKNOWN@.
--
-- @since 1.4.0
gitHashMay :: Code Q (Maybe String)
gitHashMay = qToCode Internal.gitHashMay

-- | 'gitHashMay' that attempts to read the parameter environment variable,
-- when 'gitHashMay' fails.
--
-- @since 1.4.0
gitHashMayEnv :: String -> Code Q (Maybe String)
gitHashMayEnv var = qToCode $ Internal.envFallback var Internal.gitHashMay

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD".
--
-- @since 1.4.0
gitBranch :: Code Q String
gitBranch = qToCode $ Internal.unknownFallback Internal.gitBranchMay

-- | 'gitBranch' that attempts to read the parameter environment variable,
-- when 'gitBranch' fails.
--
-- @since 1.4.0
gitBranchEnv :: String -> Code Q String
gitBranchEnv var = qToCode $ Internal.envUnknownFallback var Internal.gitBranchMay

-- | 'gitBranch' that returns that returns 'Nothing' instead of @UNKNOWN@.
--
-- @since 1.4.0
gitBranchMay :: Code Q (Maybe String)
gitBranchMay = qToCode Internal.gitBranchMay

-- | 'gitBranchMay' that attempts to read the parameter environment variable,
-- when 'gitBranchMay' fails.
--
-- @since 1.4.0
gitBranchMayEnv :: String -> Code Q (Maybe String)
gitBranchMayEnv var = qToCode $ Internal.envFallback var Internal.gitBranchMay

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
gitDescribe :: Code Q String
gitDescribe = qToCode $ Internal.unknownFallback Internal.gitDescribeMay

-- | 'gitDescribe' that attempts to read the parameter environment variable,
-- when 'gitDescribe' fails.
--
-- @since 1.4.0
gitDescribeEnv :: String -> Code Q String
gitDescribeEnv var = qToCode $ Internal.envUnknownFallback var Internal.gitDescribeMay

-- | 'gitDescribe' that returns that returns 'Nothing' instead of @UNKNOWN@.
--
-- @since 1.4.0
gitDescribeMay :: Code Q (Maybe String)
gitDescribeMay = qToCode Internal.gitDescribeMay

-- | 'gitDescribeMay' that attempts to read the parameter environment variable,
-- when 'gitDescribeMay' fails.
--
-- @since 1.4.0
gitDescribeMayEnv :: String -> Code Q (Maybe String)
gitDescribeMayEnv var = qToCode $ Internal.envFallback var Internal.gitDescribeMay

-- | Return @True@ if there are non-committed files present in the
-- repository.
gitDirty :: Code Q Bool
gitDirty = qToCode Internal.gitDirty

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository.
gitDirtyTracked :: Code Q Bool
gitDirtyTracked = qToCode Internal.gitDirtyTracked

-- | Return the number of commits in the current head.
gitCommitCount :: Code Q String
gitCommitCount = qToCode $ Internal.unknownFallback Internal.gitCommitCountMay

-- | 'gitCommitCount' that attempts to read the parameter environment variable,
-- when 'gitCommitCount' fails.
--
-- @since 1.4.0
gitCommitCountEnv :: String -> Code Q String
gitCommitCountEnv var = qToCode $ Internal.envUnknownFallback var Internal.gitCommitCountMay

-- | 'gitCommitCount' that returns that returns 'Nothing' instead of @UNKNOWN@.
--
-- @since 1.4.0
gitCommitCountMay :: Code Q (Maybe String)
gitCommitCountMay = qToCode Internal.gitCommitCountMay

-- | 'gitCommitCountMay' that attempts to read the parameter environment variable,
-- when 'gitCommitCountMay' fails.
--
-- @since 1.4.0
gitCommitCountMayEnv :: String -> Code Q (Maybe String)
gitCommitCountMayEnv var = qToCode $ Internal.envFallback var Internal.gitCommitCountMay

-- | Return the commit date of the current head.
gitCommitDate :: Code Q String
gitCommitDate = qToCode $ Internal.unknownFallback Internal.gitCommitDateMay

-- | 'gitCommitDate' that attempts to read the parameter environment variable,
-- when 'gitCommitDate' fails.
--
-- @since 1.4.0
gitCommitDateEnv :: String -> Code Q String
gitCommitDateEnv var = qToCode $ Internal.envUnknownFallback var Internal.gitCommitDateMay

-- | 'gitCommitDate' that returns that returns 'Nothing' instead of @UNKNOWN@.
--
-- @since 1.4.0
gitCommitDateMay :: Code Q (Maybe String)
gitCommitDateMay = qToCode Internal.gitCommitDateMay

-- | 'gitCommitDateMay' that attempts to read the parameter environment variable,
-- when 'gitCommitDateMay' fails.
--
-- @since 1.4.0
gitCommitDateMayEnv :: String -> Code Q (Maybe String)
gitCommitDateMayEnv var = qToCode $ Internal.envFallback var Internal.gitCommitDateMay

qToCode :: (Lift a) => Q a -> Code Q a
qToCode = TH.liftCode . fmap TExp . (>>= lift)
