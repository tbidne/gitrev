-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Adam C. Foltzer, 2025 Thomas Bidne
-- License     :  BSD3
-- Maintainer  :  tbidne@protonmail.com
--
-- Untyped Template Haskell splices for including git information in your
-- project.
--
-- @since 0.1
module Development.GitRev
  ( gitBranch,
    gitCommitCount,
    gitCommitDate,
    gitDescribe,
    gitDiff,
    gitDirty,
    gitDirtyTracked,
    gitHash,
    gitShortHash,
    gitTree,
  )
where

import Development.GitRev.Internal.Git qualified as Git
import Development.GitRev.Internal.Utils qualified as Utils
import Language.Haskell.TH (ExpQ, Q)
import Language.Haskell.TH.Syntax (Lift (lift))

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD".
--
-- ==== __Examples__
--
-- > λ. $gitBranch
-- > "main"
--
-- @since 0.1
gitBranch :: ExpQ
gitBranch = qToExp $ Utils.projectStringUnknown Git.gitBranchQ

-- | Return the number of commits in the current head.
--
-- ==== __Examples__
--
-- > λ. $gitCommitCount
-- > "47"
--
-- @since 0.1
gitCommitCount :: ExpQ
gitCommitCount = qToExp $ Utils.projectStringUnknown Git.gitCommitCountQ

-- | Return the commit date of the current head.
--
-- ==== __Examples__
--
-- > λ. $gitCommitDate
-- > "Mon Apr 14 22:14:44 2025 +1200"
--
-- @since 0.1
gitCommitDate :: ExpQ
gitCommitDate = qToExp $ Utils.projectStringUnknown Git.gitCommitDateQ

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
--
-- ==== __Examples__
--
-- > λ. $gitDescribe
-- > "1.2.0-14-g40b5d7b"
--
-- @since 0.1
gitDescribe :: ExpQ
gitDescribe = qToExp $ Utils.projectStringUnknown Git.gitDescribeQ

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- > λ. $gitDiff
-- > "diff ..."
--
-- @since 0.1
gitDiff :: ExpQ
gitDiff = qToExp $ Utils.projectStringUnknown Git.gitDiffQ

-- | Return @True@ if there are non-committed files present in the
-- repository.
--
-- ==== __Examples__
--
-- > λ. $gitDirty
-- > False
--
-- @since 0.1
gitDirty :: ExpQ
gitDirty = qToExp $ Utils.projectFalse Git.gitDirtyQ

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository.
--
-- ==== __Examples__
--
-- > λ. $gitDirtyTracked
-- > False
--
-- @since 0.1
gitDirtyTracked :: ExpQ
gitDirtyTracked = qToExp $ Utils.projectFalse Git.gitDirtyTrackedQ

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $gitHash
-- > "e67e943dd03744d3f93c21f84e127744e6a04543"
--
-- @since 0.1
gitHash :: ExpQ
gitHash = qToExp $ Utils.projectStringUnknown Git.gitHashQ

-- | Return the short hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $gitShortHash
-- > "e67e943"
--
-- @since 0.1
gitShortHash :: ExpQ
gitShortHash = qToExp $ Utils.projectStringUnknown Git.gitShortHashQ

-- | Return the hash of the current tree.
--
-- ==== __Examples__
--
-- > λ. $gitTreeQ
-- > "b718a493773568bbf920a4710b5b83bd1762dbb9"
--
-- @since 0.1
gitTree :: ExpQ
gitTree = qToExp $ Utils.projectStringUnknown Git.gitTreeQ

qToExp :: forall a. (Lift a) => Q a -> ExpQ
qToExp = (>>= lift)
