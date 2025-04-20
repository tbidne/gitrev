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

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $(gitHash)
-- > "e67e943dd03744d3f93c21f84e127744e6a04543"
--
-- >>> $(gitHash)
-- ...
gitHash :: ExpQ
gitHash = qToExp $ Utils.projectStringUnknown Git.gitHashQ

-- | Return the short hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $(gitShortHash)
-- > "e67e943"
--
-- >>> $(gitShortHash)
-- ...
gitShortHash :: ExpQ
gitShortHash = qToExp $ Utils.projectStringUnknown Git.gitShortHashQ

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD".
--
-- ==== __Examples__
--
-- > λ. $(gitBranch)
-- > "main"
--
-- >>> $(gitBranch)
-- ...
gitBranch :: ExpQ
gitBranch = qToExp $ Utils.projectStringUnknown Git.gitBranchQ

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
--
-- ==== __Examples__
--
-- > λ. $(gitDescribe)
-- > "e67e943"
--
-- >>> $(gitDescribe)
-- ...
gitDescribe :: ExpQ
gitDescribe = qToExp $ Utils.projectStringUnknown Git.gitDescribeQ

-- | Return @True@ if there are non-committed files present in the
-- repository.
--
-- ==== __Examples__
--
-- > λ. $(gitDirty)
-- > False
--
-- >>> $(gitDirty)
-- ...
gitDirty :: ExpQ
gitDirty = qToExp $ Utils.projectFalse Git.gitDirtyQ

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository.
--
-- ==== __Examples__
--
-- > λ. $(gitDirtyTracked)
-- > False
--
-- >>> $(gitDirtyTracked)
-- ...
gitDirtyTracked :: ExpQ
gitDirtyTracked = qToExp $ Utils.projectFalse Git.gitDirtyTrackedQ

-- | Return the number of commits in the current head.
--
-- ==== __Examples__
--
-- > λ. $(gitCommitCount)
-- > "47"
--
-- >>> $(gitCommitCount)
-- ...
gitCommitCount :: ExpQ
gitCommitCount = qToExp $ Utils.projectStringUnknown Git.gitCommitCountQ

-- | Return the commit date of the current head.
--
-- ==== __Examples__
--
-- > λ. $(gitCommitDate)
-- > "Mon Apr 14 22:14:44 2025 +1200"
--
-- >>> $(gitCommitDate)
-- ...
gitCommitDate :: ExpQ
gitCommitDate = qToExp $ Utils.projectStringUnknown Git.gitCommitDateQ

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- > λ. $(gitDiff)
-- > diff ...
--
-- >>> $(gitDiff)
-- ...
gitDiff :: ExpQ
gitDiff = qToExp $ Utils.projectStringUnknown Git.gitDiffQ

-- | Return the hash of the current tree.
--
-- ==== __Examples__
--
-- > λ. $(gitTreeQ)
-- > "Mon Apr 14 22:14:44 2025 +1200"
--
-- >>> $(gitTreeQ)
-- ...
gitTree :: ExpQ
gitTree = qToExp $ Utils.projectStringUnknown Git.gitTreeQ

qToExp :: forall a. (Lift a) => Q a -> ExpQ
qToExp = (>>= lift)
