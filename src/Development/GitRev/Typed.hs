-- | Typed version of "Development.GitRev".
--
-- @since 2.0
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

    -- * Custom behavior
    -- $custom

    -- ** "Out-of-tree" builds
    -- $out-of-tree

    -- ** Git Primitives
    Git.gitBranchQ,
    Git.gitCommitCountQ,
    Git.gitCommitDateQ,
    Git.gitDescribeQ,
    Git.gitDirtyQ,
    Git.gitDirtyTrackedQ,
    Git.gitHashQ,
    Git.gitShortHashQ,

    -- ** Environment lookup
    Utils.envValQ,
    Utils.runGitInEnvDirQ,

    -- ** Q to Code
    qToCode,

    -- ** Q Combinators

    -- *** First success
    QFirst (..),
    Utils.firstRight,

    -- *** Lifting Either
    Utils.liftDefString,
    Utils.liftError,
    Utils.liftFalse,

    -- ** Errors
    GitOrLookupEnvError (..),
    GitError (..),
    LookupEnvError (..),

    -- *** Utilities
    Utils.liftGitError,
    Utils.liftLookupEnvError,
    Utils.joinLookupEnvGitErrors,
    Utils.joinGitLookupEnvErrors,
  )
where

import Development.GitRev.Utils
  ( GitOrLookupEnvError
      ( GitOrLookupEnvGit,
        GitOrLookupEnvLookupEnv
      ),
    QFirst (MkQFirst),
  )
import Development.GitRev.Utils qualified as Utils
import Development.GitRev.Utils.Git (GitError (GitNotFound, GitRunError))
import Development.GitRev.Utils.Git qualified as Git
import Development.GitRev.Utils.LookupEnv (LookupEnvError (MkLookupEnvError))
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (lift), TExp (TExp))

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Data.Functor (($>))
-- >>> import Language.Haskell.TH (Code, Q, runIO)

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
-- the primitive 'Git.getHashQ', we can define a variant of 'gitHash'
-- that instead fails to compile if there are any problems with git:
--
-- >>> :{
--   let gitHashOrDie :: Code Q String
--       gitHashOrDie = qToCode $ liftError gitHashQ
-- :}
--
-- We can also define a function that falls back to an environment variable,
-- in case the git command fails. 'Utils.firstRight' takes the first action
-- that returns 'Right'.
--
-- >>> :{
--   let gitHashEnv :: String -> Code Q (Either GitOrLookupEnvError String)
--       gitHashEnv var =
--         qToCode $
--           firstRight
--             (liftGitError gitHashQ)
--             [envValQ var]
-- :}
--
-- Naturally, these can be combined:
--
-- >>> :{
--   let gitHashEnvOrDie :: String -> Code Q String
--       gitHashEnvOrDie var =
--         qToCode
--           . liftError
--           $ firstRight
--             (liftGitError gitHashQ)
--             [envValQ var]
-- :}

-- $out-of-tree
--
-- An example where custom definitions are paramount is \"out-of-tree\"
-- builds, where the build takes place outside of the normal git tree.
--
-- These builds present a problem, as we normally rely on building in the
-- project directory where the .git directory is easy to locate. For example,
-- while 'gitHash' will work for @cabal build@, it will not work for
-- @nix build@ or @cabal install@. Fortunately, there are workarounds, both
-- relying on passing the right data via environment variables.
--
-- 1. Passing the git directory.
--
--     For situations where we can pass the current directory during
--     installation e.g.
--
--     > $ export EXAMPLE_HOME=$(pwd); cabal install example
--
--     We can define
--
--     >>> :{
--       let gitHashSrcDir :: Code Q String
--           gitHashSrcDir =
--             qToCode
--             . liftDefString
--             $ firstRight
--               -- 1. We first try normal gitHashQ.
--               (liftGitError gitHashQ)
--               -- 2. If that fails, we try again in the directory pointed
--               --    to by "EXAMPLE_HOME"
--               [runGitInEnvDirQ "EXAMPLE_HOME" gitHashQ]
--     :}
--
--     If the initial call to 'Utils.gitHashQ' fails, then we will try again,
--     running the command from the directory pointed to by @EXAMPLE_HOME@.
--     'Utils.firstRight' ensures we do not run the second action unless the
--     first fails.
--
-- 2. Passing the value itself.
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
--     >>> :{
--       let gitHashVal :: Code Q String
--           gitHashVal =
--             qToCode
--             . liftDefString
--             $ firstRight
--               -- 1. We first try normal gitHashQ.
--               (liftGitError gitHashQ)
--               -- 2. If that fails, get the value directly from
--               --    "EXAMPLE_HASH".
--               [envValQ "EXAMPLE_HASH"]
--     :}
--
--     Once again, if the first attempt fails, we will run the second action,
--     looking for the value of @EXAMPLE_HASH@.
--
-- Finally, we can compose these together to make a function that works for all
-- three cases:
--
-- >>> :{
--   let gitHashValSrc :: Code Q String
--       gitHashValSrc =
--         qToCode
--           . liftDefString
--           $ firstRight
--             (liftGitError gitHashQ)
--             [ envValQ "EXAMPLE_HASH",
--               runGitInEnvDirQ "EXAMPLE_HOME" gitHashQ
--             ]
-- :}
--
-- A final note on laziness: As alluded to above, Q's default semigroup
-- instance is not lazy enough:
--
-- >>> :{
--   $$( qToCode $
--       (pure (Right "q1") :: Q (Either () String))
--         <> (runIO (putStrLn "in q2") $> Left ())
--     )
-- :}
-- in q2
-- Right "q1"
--
-- For this reason, we introduce the 'QFirst' newtype:
--
-- >>> :{
--   $$( qToCode $ unQFirst $
--       (MkQFirst $ pure (Right "q1") :: QFirst () String)
--         <> (MkQFirst $ runIO (putStrLn "in q2") $> Left ())
--     )
-- :}
-- Right "q1"
--
-- We provide a convenience function
--
-- @
--   'firstRight' :: Q (Either e a) -> [Q (Either e a)] -> Q (Either e a)
-- @
--
-- for sequencing a series of Q actions, stopping after the first success.

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitHash)
-- > "e67e943dd03744d3f93c21f84e127744e6a04543"
--
-- >>> $$(gitHash)
-- ...
--
-- @since 2.0
gitHash :: Code Q String
gitHash = qToCode $ Utils.liftDefString Git.gitHashQ

-- | Return the short hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitShortHash)
-- > "e67e943"
--
-- >>> $$(gitShortHash)
-- ...
--
-- @since 2.0
gitShortHash :: Code Q String
gitShortHash = qToCode $ Utils.liftDefString Git.gitShortHashQ

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD".
--
-- ==== __Examples__
--
-- > λ. $$(gitBranch)
-- > "main"
--
-- >>> $$(gitBranch)
-- ...
--
-- @since 2.0
gitBranch :: Code Q String
gitBranch = qToCode $ Utils.liftDefString Git.gitBranchQ

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitDescribe)
-- > "e67e943"
--
-- >>> $$(gitDescribe)
-- ...
--
-- @since 2.0
gitDescribe :: Code Q String
gitDescribe = qToCode $ Utils.liftDefString Git.gitDescribeQ

-- | Return @True@ if there are non-committed files present in the
-- repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitDirty)
-- > False
--
-- >>> $$(gitDirty)
-- ...
--
-- @since 2.0
gitDirty :: Code Q Bool
gitDirty = qToCode $ Utils.liftFalse Git.gitDirtyQ

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository.
--
-- ==== __Examples__
--
-- > λ. $$(gitDirtyTracked)
-- > False
--
-- >>> $$(gitDirtyTracked)
-- ...
--
-- @since 2.0
gitDirtyTracked :: Code Q Bool
gitDirtyTracked = qToCode $ Utils.liftFalse Git.gitDirtyTrackedQ

-- | Return the number of commits in the current head.
--
-- ==== __Examples__
--
-- > λ. $$(gitCommitCount)
-- > "47"
--
-- >>> $$(gitCommitCount)
-- ...
--
-- @since 2.0
gitCommitCount :: Code Q String
gitCommitCount = qToCode $ Utils.liftDefString Git.gitCommitCountQ

-- | Return the commit date of the current head.
--
-- ==== __Examples__
--
-- > λ. $$(gitCommitDate)
-- > "Mon Apr 14 22:14:44 2025 +1200"
--
-- >>> $$(gitCommitDate)
-- ...
--
-- @since 2.0
gitCommitDate :: Code Q String
gitCommitDate = qToCode $ Utils.liftDefString Git.gitCommitDateQ

-- | Lifts a 'Q' computation to 'Code', for usage with typed TH.
--
-- @since 2.0
qToCode :: (Lift a) => Q a -> Code Q a
qToCode = TH.liftCode . fmap TExp . (>>= lift)
