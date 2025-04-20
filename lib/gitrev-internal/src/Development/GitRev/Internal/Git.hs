{-# LANGUAGE QuasiQuotes #-}

-- | Provides utilities for querying git.
--
-- @since 0.1
module Development.GitRev.Internal.Git
  ( GitError (..),
    gitBranchQ,
    gitCommitCountQ,
    gitCommitDateQ,
    gitDescribeQ,
    gitDiffQ,
    gitDirtyQ,
    gitDirtyTrackedQ,
    gitHashQ,
    gitShortHashQ,
    gitTreeQ,
  )
where

import Control.Exception
  ( Exception (displayException),
  )
import Data.Bifunctor (Bifunctor (first))
import Development.GitRev.Internal.Git.Common
  ( GitProcessArgs
      ( MkGitProcessArgs,
        emptyPath,
        gitExeName,
        runProcessFn,
        strToP
      ),
    IndexUsed (IdxNotUsed, IdxUsed),
  )
import Development.GitRev.Internal.Git.Common qualified as Common
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)
import System.Process qualified as Process

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed (qToCode)

-- | Returns the latest git hash.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitHashQ)
-- Right ...
--
-- @since 0.1
gitHashQ :: Q (Either GitError String)
gitHashQ = runGit ["rev-parse", "HEAD"] IdxNotUsed

-- | Returns the latest git short hash.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitShortHashQ)
-- Right ...
--
-- @since 0.1
gitShortHashQ :: Q (Either GitError String)
gitShortHashQ = runGit ["rev-parse", "--short", "HEAD"] IdxNotUsed

-- | Returns the current git branch.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitBranchQ)
-- Right ...
--
-- @since 0.1
gitBranchQ :: Q (Either GitError String)
gitBranchQ = runGit ["rev-parse", "--abbrev-ref", "HEAD"] IdxNotUsed

-- | Returns the git description.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDescribeQ)
-- Right ...
--
-- @since 0.1
gitDescribeQ :: Q (Either GitError String)
gitDescribeQ = runGit ["describe", "--long", "--always"] IdxNotUsed

-- | Returns the git dirty status.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDirtyQ)
-- Right ...
--
-- @since 0.1
gitDirtyQ :: Q (Either GitError Bool)
gitDirtyQ = fmap Common.nonEmpty <$> runGit ["status", "--porcelain"] IdxUsed

-- | Returns the git dirty status, ignoring untracked files.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDirtyTrackedQ)
-- Right ...
--
-- @since 0.1
gitDirtyTrackedQ :: Q (Either GitError Bool)
gitDirtyTrackedQ =
  fmap Common.nonEmpty
    <$> runGit ["status", "--porcelain", "--untracked-files=no"] IdxUsed

-- | Returns the git commit count.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitCommitCountQ)
-- Right ...
--
-- @since 0.1
gitCommitCountQ :: Q (Either GitError String)
gitCommitCountQ = runGit ["rev-list", "HEAD", "--count"] IdxNotUsed

-- | Returns the latest git commit date.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitCommitDateQ)
-- Right ...
--
-- @since 0.1
gitCommitDateQ :: Q (Either GitError String)
gitCommitDateQ = runGit ["log", "HEAD", "-1", "--format=%cd"] IdxNotUsed

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDiffQ)
-- Right ...
--
-- @since 0.1
gitDiffQ :: Q (Either GitError String)
gitDiffQ =
  first mapGitError
    <$> Common.runGitPostprocess gitProcessArgs id ["diff", "HEAD"] IdxNotUsed

-- | Returns the hash of the current tree.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitTreeQ)
-- Right ...
--
-- @since 0.1
gitTreeQ :: Q (Either GitError String)
gitTreeQ = runGit ["show", "HEAD", "--format=%T", "--no-patch"] IdxNotUsed

-- | Errors that can be encountered with git.
--
-- @since 0.1
data GitError
  = -- | @since 0.1
    GitNotFound
  | -- | @since 0.1
    GitRunError String
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception GitError where
  displayException GitNotFound = "Git executable not found"
  displayException (GitRunError s) = "Git error: " ++ s

runGit :: [String] -> IndexUsed -> Q (Either GitError String)
runGit args idxUsed =
  first mapGitError
    <$> Common.runGitPostprocess
      gitProcessArgs
      tillNewLineStr
      args
      idxUsed

mapGitError :: Common.GitError String -> GitError
mapGitError Common.GitNotFound = GitNotFound
mapGitError (Common.GitRunError s) = GitRunError s

gitProcessArgs :: GitProcessArgs String
gitProcessArgs =
  MkGitProcessArgs
    { emptyPath = "",
      gitExeName = "git",
      runProcessFn = Process.readProcessWithExitCode,
      strToP = id
    }

tillNewLineStr :: String -> String
tillNewLineStr = takeWhile (\c -> c /= '\n' && c /= '\r')
