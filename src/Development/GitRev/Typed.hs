-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Adam C. Foltzer
-- License     :  BSD3
-- Maintainer  :  acfoltzer@galois.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Typed version of "Development.GitRev".
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Development.GitRev.Typed
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", $$(gitBranch), "@", $$(gitHash)
-- >                  , " (", $$(gitCommitDate), ")"
-- >                  , " (", $$(gitCommitCount), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | $$(gitDirty) = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >
-- > main = panic "oh no!"
--
-- > % cabal exec runhaskell Example.hs
-- > Example.hs: [panic master@2ae047ba5e4a6f0f3e705a43615363ac006099c1 (Mon Jan 11 11:50:59 2016 -0800) (14 commits in HEAD) (uncommitted files present)] oh no!
module Development.GitRev.Typed
  ( -- * Basic functions
    -- $basic
    gitBranch,
    gitCommitCount,
    gitCommitDate,
    gitDescribe,
    gitDirty,
    gitDirtyTracked,
    gitHash,
    gitShortHash,

    -- * Custom functions
    -- $custom

    -- ** Q Values
    Internal.gitBranchQ,
    Internal.gitCommitCountQ,
    Internal.gitCommitDateQ,
    Internal.gitDescribeQ,
    Internal.gitDirtyQ,
    Internal.gitDirtyTrackedQ,
    Internal.gitHashQ,
    Internal.gitShortHashQ,

    -- ** Q to Code
    qToCode,

    -- ** Modifiers
    Internal.liftFalse,
    Internal.liftDefString,
    Internal.liftError,
    Internal.envFallback,

    -- * Errors
    GitError (..),
  )
where

import Development.GitRev.Internal (GitError (GitNotFound, GitRunError))
import Development.GitRev.Internal qualified as Internal
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (lift), TExp (TExp))

-- $basic
--
-- These functions are simple, merely a typed version of "Development.GitRev"'s
-- API.
--
-- __NOTE:__ These functions do /not/ error if git fails to run, opting
-- instead to return some default value (e.g. string @UNKNOWN@, boolean @False@).

-- $custom
--
-- These functions allow us to define custom behavior. For instance, we can
-- define a variant of 'gitHash' that instead fails to compile if there are
-- any problems with git:
--
-- @
--   -- gitHashQ :: Q (Either GitError String)
--   -- liftError :: Q (Either GitError String) -> Q String
--   gitHashOrDie :: 'Code' 'Q' 'String'
--   gitHashOrDie = 'qToCode' $ 'Internal.liftError' 'Internal.gitHashQ'
-- @
--
-- We can also define a function that falls back to an environment variable,
-- in case the git command fails. This can be useful for "out-of-tree" builds
-- where the git directory is unavailable at build time, but we are able to
-- inject the value via an environment variable (e.g. nix).
--
-- @
--   -- envFallback :: String -> Q (Either GitError String) -> Q (Either GitError String)
--   gitHashEnv :: 'String' -> 'Code' 'Q' ('Either' 'GitError' 'String')
--   gitHashEnv var = 'qToCode' $ 'Internal.envFallback' var 'Internal.gitHashQ'
-- @
--
-- Finally, these can be combined:
--
-- @
--   gitHashEnvOrDie :: 'String' -> 'Code' 'Q' 'String'
--   gitHashEnvOrDie var = 'qToCode' $ 'Internal.liftError' $ 'Internal.envFallback' var 'Internal.gitHashQ'
-- @

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- @since 1.40.0
gitHash :: Code Q String
gitHash = qToCode $ Internal.liftDefString Internal.gitHashQ

-- | Return the short hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- @since 1.40.0
gitShortHash :: Code Q String
gitShortHash = qToCode $ Internal.liftDefString Internal.gitShortHashQ

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD".
--
-- @since 1.4.0
gitBranch :: Code Q String
gitBranch = qToCode $ Internal.liftDefString Internal.gitBranchQ

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
--
-- @since 1.40.0
gitDescribe :: Code Q String
gitDescribe = qToCode $ Internal.liftDefString Internal.gitDescribeQ

-- | Return @True@ if there are non-committed files present in the
-- repository.
--
-- @since 1.40.0
gitDirty :: Code Q Bool
gitDirty = qToCode $ Internal.liftFalse Internal.gitDirtyQ

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository.
--
-- @since 1.40.0
gitDirtyTracked :: Code Q Bool
gitDirtyTracked = qToCode $ Internal.liftFalse Internal.gitDirtyTrackedQ

-- | Return the number of commits in the current head.
--
-- @since 1.40.0
gitCommitCount :: Code Q String
gitCommitCount = qToCode $ Internal.liftDefString Internal.gitCommitCountQ

-- | Return the commit date of the current head.
--
-- @since 1.40.0
gitCommitDate :: Code Q String
gitCommitDate = qToCode $ Internal.liftDefString Internal.gitCommitDateQ

-- | Lifts a 'Q' computation to 'Code', for usage with typed TH.
--
-- @since 1.40.0
qToCode :: (Lift a) => Q a -> Code Q a
qToCode = TH.liftCode . fmap TExp . (>>= lift)
