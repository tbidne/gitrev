{-# LANGUAGE QuasiQuotes #-}

-- | Provides utilities for querying git.
--
-- @since 2.0
module Development.GitRev.Utils.Git
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

import Control.Exception (Exception (displayException))
import Data.Bifunctor (Bifunctor (first))
import Development.GitRev.Utils.Git.Internal
  ( GitProcessArgs
      ( MkGitProcessArgs,
        emptyPath,
        gitExeName,
        runProcessFn,
        strToP
      ),
    IndexUsed (IdxNotUsed, IdxUsed),
  )
import Development.GitRev.Utils.Git.Internal qualified as Internal
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
-- @since 2.0
gitHashQ :: Q (Either GitError String)
gitHashQ = runGit ["rev-parse", "HEAD"] IdxNotUsed

-- | Returns the latest git short hash.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitShortHashQ)
-- Right ...
--
-- @since 2.0
gitShortHashQ :: Q (Either GitError String)
gitShortHashQ = runGit ["rev-parse", "--short", "HEAD"] IdxNotUsed

-- | Returns the current git branch.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitBranchQ)
-- Right ...
--
-- @since 2.0
gitBranchQ :: Q (Either GitError String)
gitBranchQ = runGit ["rev-parse", "--abbrev-ref", "HEAD"] IdxNotUsed

-- | Returns the git description.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDescribeQ)
-- Right ...
--
-- @since 2.0
gitDescribeQ :: Q (Either GitError String)
gitDescribeQ = runGit ["describe", "--long", "--always"] IdxNotUsed

-- | Returns the git dirty status.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDirtyQ)
-- Right ...
--
-- @since 2.0
gitDirtyQ :: Q (Either GitError Bool)
gitDirtyQ = fmap nonEmpty <$> runGit ["status", "--porcelain"] IdxUsed

-- | Returns the git dirty status, ignoring untracked files.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDirtyTrackedQ)
-- Right ...
--
-- @since 2.0
gitDirtyTrackedQ :: Q (Either GitError Bool)
gitDirtyTrackedQ =
  fmap nonEmpty
    <$> runGit ["status", "--porcelain", "--untracked-files=no"] IdxUsed

-- | Returns the git commit count.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitCommitCountQ)
-- Right ...
--
-- @since 2.0
gitCommitCountQ :: Q (Either GitError String)
gitCommitCountQ = runGit ["rev-list", "HEAD", "--count"] IdxNotUsed

-- | Returns the latest git commit date.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitCommitDateQ)
-- Right ...
--
-- @since 2.0
gitCommitDateQ :: Q (Either GitError String)
gitCommitDateQ = runGit ["log", "HEAD", "-1", "--format=%cd"] IdxNotUsed

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDiffQ)
-- Right ...
--
-- @since 2.0
gitDiffQ :: Q (Either GitError String)
gitDiffQ =
  (first mapError)
    <$> Internal.runGitPostprocess gitProcessArgs id ["diff", "HEAD"] IdxNotUsed

-- | Returns the hash of the current tree.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitTreeQ)
-- Right ...
--
-- @since 2.0
gitTreeQ :: Q (Either GitError String)
gitTreeQ = runGit ["show", "HEAD", "--format=%T", "--no-patch"] IdxNotUsed

nonEmpty :: String -> Bool
nonEmpty "" = False
nonEmpty _ = True

-- | Errors that can be encountered with git.
--
-- @since 2.0
data GitError
  = -- | @since 2.0
    GitNotFound
  | -- | @since 2.0
    GitRunError String
  deriving stock
    ( -- | @since 2.0
      Eq,
      -- | @since 2.0
      Lift,
      -- | @since 2.0
      Show
    )

-- | @since 2.0
instance Exception GitError where
  displayException GitNotFound = "Git executable not found"
  displayException (GitRunError s) = "Git error: " ++ s

runGit :: [String] -> IndexUsed -> Q (Either GitError String)
runGit args =
  fmap (first mapError)
    . Internal.runGitPostprocess gitProcessArgs tillNewLineStr args

gitProcessArgs :: GitProcessArgs String
gitProcessArgs =
  MkGitProcessArgs
    { emptyPath = "",
      gitExeName = "git",
      runProcessFn = Process.readProcessWithExitCode,
      strToP = id
    }

mapError :: Internal.GitError String -> GitError
mapError Internal.GitNotFound = GitNotFound
mapError (Internal.GitRunError s) = GitRunError s

tillNewLineStr :: String -> String
tillNewLineStr = takeWhile (\c -> c /= '\n' && c /= '\r')
