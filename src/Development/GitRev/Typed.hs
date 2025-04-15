-- | Typed version of "Development.GitRev".
--
-- @since 1.40.0
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

    -- ** Q Primitives
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

    -- ** Q Modifiers
    Internal.liftDefString,
    Internal.liftError,
    Internal.liftFalse,
    Internal.envValFallback,
    Internal.envSrcFallback,

    -- * Errors
    GitError (..),

    -- * "Out-of-tree" builds
    -- $out-of-tree
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
-- These functions allow us to define custom behavior. For instance, using
-- the primitive 'Internal.getHashQ', we can define a variant of 'gitHash'
-- that instead fails to compile if there are any problems with git:
--
-- @
--   -- gitHashQ :: Q (Either GitError String)
--   -- liftError :: Q (Either GitError String) -> Q String
--   gitHashOrDie :: 'Code' 'Q' 'String'
--   gitHashOrDie = 'qToCode' $ 'Internal.liftError' 'Internal.gitHashQ'
-- @
--
-- We can also define a function that falls back to an environment variable,
-- in case the git command fails.
--
-- @
--   -- envValFallback :: String -> Q (Either GitError String) -> Q (Either GitError String)
--   gitHashEnv :: 'String' -> 'Code' 'Q' ('Either' 'GitError' 'String')
--   gitHashEnv var = 'qToCode' $ 'Internal.envValFallback' var 'Internal.gitHashQ'
-- @
--
-- Naturally, these can be combined:
--
-- @
--   gitHashEnvOrDie :: 'String' -> 'Code' 'Q' 'String'
--   gitHashEnvOrDie var = 'qToCode' $ 'Internal.liftError' $ 'Internal.envValFallback' var 'Internal.gitHashQ'
-- @

-- $out-of-tree
--
-- So-called \"out-of-tree\" builds present a problem, as we normally rely on
-- building in the project directory where the .git directory is easy to
-- locate. For example, while 'gitHash' will work for @cabal build@, it will
-- not work for nix or @cabal install@. Fortunately, there are workarounds,
-- both relying on passing the right data via environment variables.
--
-- 1. Cabal install
--
--     With cabal, we can pass the current directory during installation e.g.
--
--     > $ export EXAMPLE_HOME=$(pwd); cabal install example
--
--     Then we can define
--
--     @
--       gitHashSrcDir :: 'Code' 'Q' 'String'
--       gitHashSrcDir =
--         'qToCode'
--           . 'Internal.liftDefString'
--           . 'Internal.envSrcFallback' \"EXAMPLE_HOME\"
--           $ 'Internal.gitHashQ'
--     @
--
-- 2. Nix
--
--     Nix requires a different approach. Thankfully, nix flakes provide
--     a variety of revisions via its @self@ interface. For example:
--
--     @
--       # Injecting the git hash via EXAMPLE_HASH where drv is the normal
--       # derivation.
--       drv.overrideAttrs (oldAttrs: {
--         # Also: self.shortRev, self.dirtyShortRev
--         EXAMPLE_HASH = \"${self.rev or self.dirtyRev}\";
--       });
--     @
--
--     Then we can define
--
--     @
--       gitHashVal :: 'Code' 'Q' 'String'
--       gitHashVal =
--         'qToCode'
--           . 'Internal.liftDefString'
--           . 'Internal.envValFallback' \"EXAMPLE_HASH\"
--           $ 'Internal.gitHashQ'
--     @
--
-- We can compose these together to make a function that works for all three:
--
-- @
--   gitHashValSrc :: 'Code' 'Q' 'String'
--   gitHashValSrc =
--     'qToCode'
--       . 'Internal.liftDefString'
--       . 'Internal.envSrcFallback' \"EXAMPLE_HOME\"
--       . 'Internal.envValFallback' \"EXAMPLE_HASH\"
--       $ 'Internal.gitHashQ'
-- @

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitHash)
-- > "e67e943dd03744d3f93c21f84e127744e6a04543"
--
-- @since 1.40.0
gitHash :: Code Q String
gitHash = qToCode $ Internal.liftDefString Internal.gitHashQ

-- | Return the short hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitHash)
-- > "e67e943"
--
-- @since 1.40.0
gitShortHash :: Code Q String
gitShortHash = qToCode $ Internal.liftDefString Internal.gitShortHashQ

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD".
--
-- ==== __Examples__
--
-- > λ. $$(gitBranch)
-- > "main"
--
-- @since 1.4.0
gitBranch :: Code Q String
gitBranch = qToCode $ Internal.liftDefString Internal.gitBranchQ

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitDescribe)
-- > "e67e943"
--
-- @since 1.40.0
gitDescribe :: Code Q String
gitDescribe = qToCode $ Internal.liftDefString Internal.gitDescribeQ

-- | Return @True@ if there are non-committed files present in the
-- repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitDirty)
-- > False
--
-- @since 1.40.0
gitDirty :: Code Q Bool
gitDirty = qToCode $ Internal.liftFalse Internal.gitDirtyQ

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitHash)
-- > False
--
-- @since 1.40.0
gitDirtyTracked :: Code Q Bool
gitDirtyTracked = qToCode $ Internal.liftFalse Internal.gitDirtyTrackedQ

-- | Return the number of commits in the current head.
--
-- ==== __Examples__
--
-- > λ. $$(gitHash)
-- > "47"
--
-- @since 1.40.0
gitCommitCount :: Code Q String
gitCommitCount = qToCode $ Internal.liftDefString Internal.gitCommitCountQ

-- | Return the commit date of the current head.
--
-- ==== __Examples__
--
-- > λ. $$(gitCommitDate)
-- > "Mon Apr 14 22:14:44 2025 +1200"
--
-- @since 1.40.0
gitCommitDate :: Code Q String
gitCommitDate = qToCode $ Internal.liftDefString Internal.gitCommitDateQ

-- | Lifts a 'Q' computation to 'Code', for usage with typed TH.
--
-- @since 1.40.0
qToCode :: (Lift a) => Q a -> Code Q a
qToCode = TH.liftCode . fmap TExp . (>>= lift)
