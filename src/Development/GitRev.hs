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
-- a git repository
gitHash :: ExpQ
gitHash = qToExp $ Internal.liftDefString Internal.gitHashQ

-- | Return the short hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
gitShortHash :: ExpQ
gitShortHash = qToExp $ Internal.liftDefString Internal.gitShortHashQ

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD"
gitBranch :: ExpQ
gitBranch = qToExp $ Internal.liftDefString Internal.gitBranchQ

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
gitDescribe :: ExpQ
gitDescribe = qToExp $ Internal.liftDefString Internal.gitDescribeQ

-- | Return @True@ if there are non-committed files present in the
-- repository
gitDirty :: ExpQ
gitDirty = qToExp $ Internal.liftFalse Internal.gitDirtyQ

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository
gitDirtyTracked :: ExpQ
gitDirtyTracked = qToExp $ Internal.liftFalse Internal.gitDirtyTrackedQ

-- | Return the number of commits in the current head
gitCommitCount :: ExpQ
gitCommitCount = qToExp $ Internal.liftDefString Internal.gitCommitCountQ

-- | Return the commit date of the current head
gitCommitDate :: ExpQ
gitCommitDate = qToExp $ Internal.liftDefString Internal.gitCommitDateQ

qToExp :: (Lift a) => Q a -> ExpQ
qToExp = (>>= lift)
