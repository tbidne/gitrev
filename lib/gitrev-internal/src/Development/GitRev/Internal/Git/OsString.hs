{-# LANGUAGE QuasiQuotes #-}

-- | "Development.GitRev.Internal.Git" for 'OsString'.
--
-- @since 0.1
module Development.GitRev.Internal.Git.OsString
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
import Development.GitRev.Internal.Git.Common
  ( GitProcessArgs
      ( MkGitProcessArgs,
        gitRootArgs,
        pToOsPath,
        runProcessGit,
        strToP
      ),
    IndexUsed (IdxNotUsed, IdxUsed),
  )
import Development.GitRev.Internal.Git.Common qualified as Common
import Development.GitRev.Internal.OsString qualified as OsStringI
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)
import System.OsString (OsString, osstr)
import System.Process qualified as Process

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed.OsString (qToCode)

-- | Returns the latest git hash.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitHashQ)
-- Right ...
--
-- @since 0.1
gitHashQ :: Q (Either GitError OsString)
gitHashQ = runGit [[osstr|rev-parse|], [osstr|HEAD|]] IdxNotUsed

-- | Returns the latest git short hash.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitShortHashQ)
-- Right ...
--
-- @since 0.1
gitShortHashQ :: Q (Either GitError OsString)
gitShortHashQ =
  runGit [[osstr|rev-parse|], [osstr|--short|], [osstr|HEAD|]] IdxNotUsed

-- | Returns the current git branch.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitBranchQ)
-- Right ...
--
-- @since 0.1
gitBranchQ :: Q (Either GitError OsString)
gitBranchQ =
  runGit [[osstr|rev-parse|], [osstr|--abbrev-ref|], [osstr|HEAD|]] IdxNotUsed

-- | Returns the git description.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDescribeQ)
-- Right ...
--
-- @since 0.1
gitDescribeQ :: Q (Either GitError OsString)
gitDescribeQ =
  runGit [[osstr|describe|], [osstr|--long|], [osstr|--always|]] IdxNotUsed

-- | Returns the git dirty status.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDirtyQ)
-- Right ...
--
-- @since 0.1
gitDirtyQ :: Q (Either GitError Bool)
gitDirtyQ =
  fmap Common.nonEmpty
    <$> runGit [[osstr|status|], [osstr|--porcelain|]] IdxUsed

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
    <$> runGit
      [[osstr|status|], [osstr|--porcelain|], [osstr|--untracked-files=no|]]
      IdxUsed

-- | Returns the git commit count.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitCommitCountQ)
-- Right ...
--
-- @since 0.1
gitCommitCountQ :: Q (Either GitError OsString)
gitCommitCountQ =
  runGit [[osstr|rev-list|], [osstr|HEAD|], [osstr|--count|]] IdxNotUsed

-- | Returns the latest git commit date.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitCommitDateQ)
-- Right ...
--
-- @since 0.1
gitCommitDateQ :: Q (Either GitError OsString)
gitCommitDateQ =
  runGit
    [ [osstr|log|],
      [osstr|HEAD|],
      [osstr|-1|],
      [osstr|--format=%cd|]
    ]
    IdxNotUsed

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDiffQ)
-- Right ...
--
-- @since 0.1
gitDiffQ :: Q (Either GitError OsString)
gitDiffQ =
  first mapGitError
    <$> Common.runGitPostprocess
      gitProcessArgs
      id
      [[osstr|diff|], [osstr|HEAD|]]
      IdxNotUsed

-- | Returns the hash of the current tree.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitTreeQ)
-- Right ...
--
-- @since 0.1
gitTreeQ :: Q (Either GitError OsString)
gitTreeQ =
  runGit
    [ [osstr|show|],
      [osstr|HEAD|],
      [osstr|--format=%T|],
      [osstr|--no-patch|]
    ]
    IdxNotUsed

-- | Errors that can be encountered with git.
--
-- @since 0.1
data GitError
  = -- | @since 0.1
    GitNotFound
  | -- | @since 0.1
    GitRunError OsString
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
  displayException (GitRunError s) = "Git error: " ++ OsStringI.decodeLenient s

runGit :: [OsString] -> IndexUsed -> Q (Either GitError OsString)
runGit args idxUsed =
  first mapGitError
    <$> Common.runGitPostprocess
      gitProcessArgs
      Common.tillNewLineOsPath
      args
      idxUsed

mapGitError :: Common.GitError OsString -> GitError
mapGitError Common.GitNotFound = GitNotFound
mapGitError (Common.GitRunError s) = GitRunError s

gitProcessArgs :: GitProcessArgs OsString
gitProcessArgs =
  MkGitProcessArgs
    { gitRootArgs = [[osstr|rev-parse|], [osstr|--show-toplevel|]],
      pToOsPath = pure . id,
      -- TODO: Once process gets OsString support, replace
      -- readProcessWithExitCode below. Should make all of this encoding
      -- unnecessary.
      runProcessGit = \args -> do
        args' <- traverse OsStringI.decodeThrowM args
        (ec, out, err) <- Process.readProcessWithExitCode "git" args' ""
        (ec,,OsStringI.encodeLenient err) <$> OsStringI.encodeThrowM out,
      strToP = OsStringI.encodeLenient
    }
