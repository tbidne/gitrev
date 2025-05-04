-- | 'Q' primitives for git actions.
--
-- @since 0.1
module Development.GitRev.Internal.Git
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

import Control.Exception
  ( Exception (displayException),
  )
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
import System.Process qualified as Process

-- $setup
-- >>> import Development.GitRev.Typed (qToCode)

-- | Returns the current git branch.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitBranchQ)
-- Right ...
--
-- @since 0.1
gitBranchQ :: Q (Either GitError String)
gitBranchQ = runGitQ ["rev-parse", "--abbrev-ref", "HEAD"] IdxNotUsed

-- | Returns the git commit count.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitCommitCountQ)
-- Right ...
--
-- @since 0.1
gitCommitCountQ :: Q (Either GitError String)
gitCommitCountQ = runGitQ ["rev-list", "HEAD", "--count"] IdxNotUsed

-- | Returns the latest git commit date.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitCommitDateQ)
-- Right ...
--
-- @since 0.1
gitCommitDateQ :: Q (Either GitError String)
gitCommitDateQ = runGitQ ["log", "HEAD", "-1", "--format=%cd"] IdxNotUsed

-- | Returns the git description.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDescribeQ)
-- Right ...
--
-- @since 0.1
gitDescribeQ :: Q (Either GitError String)
gitDescribeQ = runGitQ ["describe", "--long", "--always"] IdxNotUsed

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDiffQ)
-- Right ...
--
-- @since 0.1
gitDiffQ :: Q (Either GitError String)
gitDiffQ = runGitPostProcessQ id ["diff", "HEAD"] IdxNotUsed

-- | Returns the git dirty status.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitDirtyQ)
-- Right ...
--
-- @since 0.1
gitDirtyQ :: Q (Either GitError Bool)
gitDirtyQ = fmap Common.nonEmpty <$> runGitQ ["status", "--porcelain"] IdxUsed

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
    <$> runGitQ ["status", "--porcelain", "--untracked-files=no"] IdxUsed

-- | Returns the latest git hash.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitHashQ)
-- Right ...
--
-- @since 0.1
gitHashQ :: Q (Either GitError String)
gitHashQ = runGitQ ["rev-parse", "HEAD"] IdxNotUsed

-- | Returns the latest git short hash.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitShortHashQ)
-- Right ...
--
-- @since 0.1
gitShortHashQ :: Q (Either GitError String)
gitShortHashQ = runGitQ ["rev-parse", "--short", "HEAD"] IdxNotUsed

-- | Returns the hash of the current tree.
--
-- ==== __Examples__
--
-- >>> $$(qToCode gitTreeQ)
-- Right ...
--
-- @since 0.1
gitTreeQ :: Q (Either GitError String)
gitTreeQ = runGitQ ["show", "HEAD", "--format=%T", "--no-patch"] IdxNotUsed

-- | Errors that can be encountered with git.
--
-- @since 0.1
newtype GitError = MkGitError
  { -- | @since 0.1
    reason :: String
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
  displayException (MkGitError s) = "Git error: " ++ s

-- | Runs git with the arguments. If 'IdxUsed' is passed, it is tracked for
-- recompilation purposes.
--
-- ==== __Examples__
--
-- >>> :{
--   -- Returns 'YYYY-MM-DD' rather than e.g. gitCommitDateQ's
--   -- 'Fri May 2 13:29:59 2025 +1200'.
--   gitCommitDateShortQ :: Q (Either GitError String)
--   gitCommitDateShortQ = runGitQ ["log", "HEAD", "-1", "--format=%cs"] IdxNotUsed
-- :}
--
-- @since 0.1
runGitQ ::
  -- | Arguments to git.
  [String] ->
  -- | Whether the index is used.
  IndexUsed ->
  Q (Either GitError String)
runGitQ = runGitPostProcessQ tillNewLineStr

-- | Like 'runGitQ', except it applies the given function to the result.
-- Normal 'runGitQ' takes everything up until the first new line or carriage
-- return.
--
-- ==== __Examples__
--
-- >>> :{
--   runGitNoProcessQ :: [String] -> IndexUsed -> Q (Either GitError String)
--   runGitNoProcessQ = runGitPostProcessQ id
-- :}
--
-- @since 0.1
runGitPostProcessQ ::
  -- | Function to run on the result.
  (String -> String) ->
  -- | Arguments to git.
  [String] ->
  -- | Whether the index is used.
  IndexUsed ->
  Q (Either GitError String)
runGitPostProcessQ postProcess args idxUsed =
  first mapGitError
    <$> Common.runGitPostprocess
      gitProcessArgs
      postProcess
      args
      idxUsed

mapGitError :: Common.GitError String -> GitError
mapGitError (Common.MkGitError s) = MkGitError s

gitProcessArgs :: GitProcessArgs String
gitProcessArgs =
  MkGitProcessArgs
    { fromStringTotal = id,
      gitRootArgs = ["rev-parse", "--show-toplevel"],
      runProcessGit = \args -> Process.readProcessWithExitCode "git" args "",
      toOsPath = OsStringI.encodeThrowM,
      toStringTotal = id
    }

tillNewLineStr :: String -> String
tillNewLineStr = takeWhile (\c -> c /= '\n' && c /= '\r')
