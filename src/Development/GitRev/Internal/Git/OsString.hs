{-# LANGUAGE QuasiQuotes #-}

-- | "Development.GitRev.Internal.Git" for 'OsString'.
--
-- @since 0.1
module Development.GitRev.Internal.Git.OsString
  ( -- * Built-in
    GitError (..),
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

    -- * Git primitives
    runGitQ,
    runGitPostProcessQ,
    IndexUsed (..),
  )
where

import Control.Exception (Exception (displayException))
import Data.Bifunctor (Bifunctor (first))
import Development.GitRev.Internal.Git.Common
  ( GitProcessArgs
      ( MkGitProcessArgs,
        fromStringTotal,
        gitRootArgs,
        runProcessGit,
        toOsPath,
        toStringTotal
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
-- >>> :set -XQuasiQuotes
-- >>> import Development.GitRev.Typed.OsString (qToCode)

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
  runGitQ [[osstr|rev-parse|], [osstr|--abbrev-ref|], [osstr|HEAD|]] IdxNotUsed

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
  runGitQ [[osstr|rev-list|], [osstr|HEAD|], [osstr|--count|]] IdxNotUsed

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
  runGitQ
    [ [osstr|log|],
      [osstr|HEAD|],
      [osstr|-1|],
      [osstr|--format=%cd|]
    ]
    IdxNotUsed

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
  runGitQ [[osstr|describe|], [osstr|--long|], [osstr|--always|]] IdxNotUsed

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDiffQ)
-- Right ...
--
-- @since 0.1
gitDiffQ :: Q (Either GitError OsString)
gitDiffQ = runGitPostProcessQ id [[osstr|diff|], [osstr|HEAD|]] IdxNotUsed

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
    <$> runGitQ [[osstr|status|], [osstr|--porcelain|]] IdxUsed

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
    <$> runGitQ
      [[osstr|status|], [osstr|--porcelain|], [osstr|--untracked-files=no|]]
      IdxUsed

-- | Returns the latest git hash.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitHashQ)
-- Right ...
--
-- @since 0.1
gitHashQ :: Q (Either GitError OsString)
gitHashQ = runGitQ [[osstr|rev-parse|], [osstr|HEAD|]] IdxNotUsed

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
  runGitQ [[osstr|rev-parse|], [osstr|--short|], [osstr|HEAD|]] IdxNotUsed

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
  runGitQ
    [ [osstr|show|],
      [osstr|HEAD|],
      [osstr|--format=%T|],
      [osstr|--no-patch|]
    ]
    IdxNotUsed

-- | Errors that can be encountered with git.
--
-- @since 0.1
newtype GitError = MkGitError
  { -- | @since 0.1
    reason :: OsString
  }
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
  displayException (MkGitError s) = "Git error: " ++ OsStringI.decodeLenient s

-- | Runs git with the arguments. If 'IdxUsed' is passed, it is tracked for
-- recompilation purposes.
--
-- ==== __Examples__
--
-- >>> :{
--   -- Returns 'YYYY-MM-DD' rather than e.g. gitCommitDateQ's
--   -- 'Fri May 2 13:29:59 2025 +1200'.
--   gitCommitDateShortQ :: Q (Either GitError OsString)
--   gitCommitDateShortQ =
--     runGitQ
--       [[osstr|log|], [osstr|HEAD|], [osstr|-1|], [osstr|--format=%cs|]]
--       IdxNotUsed
-- :}
--
-- @since 0.1
runGitQ ::
  -- | Arguments to git.
  [OsString] ->
  -- | Whether the index is used.
  IndexUsed ->
  Q (Either GitError OsString)
runGitQ = runGitPostProcessQ Common.tillNewLineOsPath

-- | Like 'runGitQ', except it applies the given function to the result.
-- Normal 'runGitQ' takes everything up until the first new line or carriage
-- return.
--
-- ==== __Examples__
--
-- >>> :{
--   runGitNoProcessQ :: [OsString] -> IndexUsed -> Q (Either GitError OsString)
--   runGitNoProcessQ = runGitPostProcessQ id
-- :}
--
-- @since 0.1
runGitPostProcessQ ::
  -- | Function to run on the result.
  (OsString -> OsString) ->
  -- | Arguments to git.
  [OsString] ->
  -- | Whether the index is used.
  IndexUsed ->
  Q (Either GitError OsString)
runGitPostProcessQ postProcess args idxUsed =
  first mapGitError
    <$> Common.runGitPostprocess
      gitProcessArgs
      postProcess
      args
      idxUsed

mapGitError :: Common.GitError OsString -> GitError
mapGitError (Common.MkGitError s) = MkGitError s

gitProcessArgs :: GitProcessArgs OsString
gitProcessArgs =
  MkGitProcessArgs
    { fromStringTotal = OsStringI.encodeLenient,
      gitRootArgs = [[osstr|rev-parse|], [osstr|--show-toplevel|]],
      -- TODO: Once process gets OsString support, replace
      -- readProcessWithExitCode below. Should make all of this encoding
      -- unnecessary.
      runProcessGit = \args -> do
        args' <- traverse OsStringI.decodeThrowM args
        (ec, out, err) <- Process.readProcessWithExitCode "git" args' ""
        (ec,,OsStringI.encodeLenient err) <$> OsStringI.encodeThrowM out,
      toOsPath = pure,
      toStringTotal = OsStringI.decodeLenient
    }
