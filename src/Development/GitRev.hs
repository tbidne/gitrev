{-# LANGUAGE QuasiQuotes #-}

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
  )
where

import Development.GitRev.Internal (IndexUsed (IdxNotUsed, IdxUsed))
import Development.GitRev.Internal qualified as Internal
import Language.Haskell.TH (ExpQ, conE, stringE)
import Language.Haskell.TH.Syntax (falseName, trueName)

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository
gitHash :: ExpQ
gitHash =
  stringE =<< Internal.runGit ["rev-parse", "HEAD"] "UNKNOWN" IdxNotUsed

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD"
gitBranch :: ExpQ
gitBranch =
  stringE =<< Internal.runGit ["rev-parse", "--abbrev-ref", "HEAD"] "UNKNOWN" IdxNotUsed

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
gitDescribe :: ExpQ
gitDescribe =
  stringE =<< Internal.runGit ["describe", "--long", "--always"] "UNKNOWN" IdxNotUsed

-- | Return @True@ if there are non-committed files present in the
-- repository
gitDirty :: ExpQ
gitDirty = do
  output <- Internal.runGit ["status", "--porcelain"] "" IdxUsed
  case output of
    "" -> conE falseName
    _ -> conE trueName

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository
gitDirtyTracked :: ExpQ
gitDirtyTracked = do
  output <- Internal.runGit ["status", "--porcelain", "--untracked-files=no"] "" IdxUsed
  case output of
    "" -> conE falseName
    _ -> conE trueName

-- | Return the number of commits in the current head
gitCommitCount :: ExpQ
gitCommitCount =
  stringE =<< Internal.runGit ["rev-list", "HEAD", "--count"] "UNKNOWN" IdxNotUsed

-- | Return the commit date of the current head
gitCommitDate :: ExpQ
gitCommitDate =
  stringE =<< Internal.runGit ["log", "HEAD", "-1", "--format=%cd"] "UNKNOWN" IdxNotUsed
