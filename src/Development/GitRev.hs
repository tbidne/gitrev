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
    gitDirty,
    gitDirtyTracked,
    gitHash,
    gitShortHash,
  )
where

import Development.GitRev.Internal qualified as Internal
import Language.Haskell.TH (ExpQ, Q)
import Language.Haskell.TH.Syntax (Lift (lift))

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $(gitHash)
-- > "e67e943dd03744d3f93c21f84e127744e6a04543"
gitHash :: ExpQ
gitHash = qToExp $ Internal.liftDefString Internal.gitHashQ

-- | Return the short hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $(gitShortHash)
-- > "e67e943"
gitShortHash :: ExpQ
gitShortHash = qToExp $ Internal.liftDefString Internal.gitShortHashQ

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD".
--
-- ==== __Examples__
--
-- > λ. $(gitBranch)
-- > "main"
gitBranch :: ExpQ
gitBranch = qToExp $ Internal.liftDefString Internal.gitBranchQ

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
--
-- ==== __Examples__
--
-- > λ. $(gitDescribe)
-- > "e67e943"
gitDescribe :: ExpQ
gitDescribe = qToExp $ Internal.liftDefString Internal.gitDescribeQ

-- | Return @True@ if there are non-committed files present in the
-- repository.
--
-- ==== __Examples__
--
-- > λ. $(gitDirty)
-- > False
gitDirty :: ExpQ
gitDirty = qToExp $ Internal.liftFalse Internal.gitDirtyQ

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository.
--
-- ==== __Examples__
--
-- > λ. $(gitDirtyTracked)
-- > False
gitDirtyTracked :: ExpQ
gitDirtyTracked = qToExp $ Internal.liftFalse Internal.gitDirtyTrackedQ

-- | Return the number of commits in the current head.
--
-- ==== __Examples__
--
-- > λ. $(gitBranch)
-- > "47"
gitCommitCount :: ExpQ
gitCommitCount = qToExp $ Internal.liftDefString Internal.gitCommitCountQ

-- | Return the commit date of the current head.
--
-- ==== __Examples__
--
-- > λ. $(gitCommitDate)
-- > "Mon Apr 14 22:14:44 2025 +1200"
gitCommitDate :: ExpQ
gitCommitDate = qToExp $ Internal.liftDefString Internal.gitCommitDateQ

qToExp :: (Lift a) => Q a -> ExpQ
qToExp = (>>= lift)
