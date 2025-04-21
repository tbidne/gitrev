-- | "Development.GitRev.Typed" for 'OsString'.
--
-- @since 2.0
module Development.GitRev.Typed.OsString
  ( -- * Basic functions
    -- $basic
    gitBranch,
    gitCommitCount,
    gitCommitDate,
    gitDescribe,
    gitDiff,
    gitDirty,
    gitDirtyTracked,
    gitHash,
    gitShortHash,
    gitTree,

    -- * Custom behavior
    -- $custom

    -- ** "Out-of-tree" builds
    -- $out-of-tree

    -- ** Git Primitives
    Git.gitBranchQ,
    Git.gitCommitCountQ,
    Git.gitCommitDateQ,
    Git.gitDescribeQ,
    Git.gitDiffQ,
    Git.gitDirtyQ,
    Git.gitDirtyTrackedQ,
    Git.gitHashQ,
    Git.gitShortHashQ,
    Git.gitTreeQ,

    -- *** Running your own git actions
    Git.runGit,
    IndexUsed (..),

    -- ** Environment lookup
    Env.envValQ,
    Utils.runGitInEnvDirQ,

    -- ** Q to Code
    qToCode,

    -- ** Q Combinators

    -- *** First success
    QFirst (..),
    QFirst.mkQFirst,
    QFirst.firstSuccessQ,
    Exceptions (MkExceptions),
    QFirst.mkExceptions,

    -- *** Eliminating Either
    Utils.projectStringUnknown,
    Utils.projectString,
    Utils.projectFalse,
    Utils.projectError,
    Utils.projectErrorMap,

    -- ** Errors
    GitOrEnvLookupError (..),
    GitError (..),
    EnvLookupError (..),

    -- *** Utilities
    Utils.embedGitError,
    Utils.embedEnvLookupError,
    Utils.joinEnvLookupGitErrors,
    Utils.joinGitEnvLookupErrors,
  )
where

import Development.GitRev.Internal.Environment.OsString
  ( EnvLookupError (MkEnvLookupError),
  )
import Development.GitRev.Internal.Environment.OsString qualified as Env
import Development.GitRev.Internal.Git.OsString
  ( GitError (GitNotFound, GitRunError),
    IndexUsed (IdxNotUsed, IdxUsed),
  )
import Development.GitRev.Internal.Git.OsString qualified as Git
import Development.GitRev.Internal.QFirst
  ( Exceptions (MkExceptions),
    QFirst (MkQFirst),
  )
import Development.GitRev.Internal.QFirst qualified as QFirst
import Development.GitRev.Internal.Utils.OsString
  ( GitOrEnvLookupError
      ( GitOrEnvLookupEnvLookup,
        GitOrEnvLookupGit
      ),
  )
import Development.GitRev.Internal.Utils.OsString qualified as Utils
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (lift), TExp (TExp))
import System.OsString (OsString)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XTemplateHaskell
-- >>> import Data.Functor (($>))
-- >>> import Language.Haskell.TH (Code, Q, runIO)
-- >>> import System.OsString (OsString, osstr)

-- $basic
--
-- These functions are simple, merely a typed version of "Development.GitRev"'s
-- API.
--
-- __NOTE:__ These functions do /not/ error if git fails to run, opting
-- instead to return some default value (e.g. string @UNKNOWN@, boolean @False@).

-- $custom
--
-- These functions allow defining custom behavior. For instance, using
-- the primitive 'Git.getHashQ' and combinator 'Utils.projectError', we can
-- define a variant of 'gitHash' that instead fails to compile if there are
-- any problems with git:
--
-- >>> :{
--   let gitHashOrDie :: Code Q OsString
--       gitHashOrDie = qToCode $ projectError gitHashQ
-- :}
--
-- We can also define a function that falls back to an environment variable,
-- in case the git command fails. 'Utils.firstSuccessQ' takes the first action
-- that returns 'Right'.
--
-- >>> :{
--   let gitHashEnv :: OsString -> Code Q (Either (Exceptions GitOrEnvLookupError) OsString)
--       gitHashEnv var =
--         qToCode $
--           firstSuccessQ
--             (embedGitError gitHashQ)
--             [embedEnvLookupError $ envValQ var]
-- :}
--
-- Naturally, these can be combined:
--
-- >>> :{
--   let gitHashEnvOrDie :: OsString -> Code Q OsString
--       gitHashEnvOrDie var =
--         qToCode
--           . projectError
--           $ firstSuccessQ
--             (embedGitError gitHashQ)
--             [embedEnvLookupError $ envValQ var]
-- :}

-- $out-of-tree
--
-- Custom definitions are particularly useful for \"out-of-tree\" builds,
-- where the build takes place outside of the normal git tree.
--
-- These builds present a problem, as we normally rely on building in the
-- project directory where the @.git@ directory is easy to locate. For
-- example, while 'gitHash' will work for @\'cabal build\'@, it will not work
-- for @\'nix build\'@ or @\'cabal install\'@. Fortunately, there are
-- workarounds, both relying on passing the right data via environment
-- variables.
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
--       let gitHashSrcDir :: Code Q OsString
--           gitHashSrcDir =
--             qToCode
--             . projectStringUnknown
--             $ firstSuccessQ
--               -- 1. We first try normal gitHashQ.
--               (embedGitError gitHashQ)
--               -- 2. If that fails, we try again in the directory pointed
--               --    to by "EXAMPLE_HOME".
--               [runGitInEnvDirQ [osstr|EXAMPLE_HOME|] gitHashQ]
--     :}
--
--     If the initial call to 'Utils.gitHashQ' fails, then we will try again,
--     running the command from the directory pointed to by @EXAMPLE_HOME@.
--     'Utils.firstSuccessQ' ensures we do not run the second action unless the
--     first fails.
--
-- 2. Passing the value itself.
--
--     This approach can work well with nix, as nix flakes provides
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
--       let gitHashVal :: Code Q OsString
--           gitHashVal =
--             qToCode
--             . projectStringUnknown
--             $ firstSuccessQ
--               -- 1. We first try normal gitHashQ.
--               (embedGitError gitHashQ)
--               -- 2. If that fails, get the value directly from
--               --    "EXAMPLE_HASH".
--               [embedEnvLookupError $ envValQ [osstr|EXAMPLE_HASH|]]
--     :}
--
--     Once again, if the first attempt fails, we will run the second action,
--     looking for the value of @EXAMPLE_HASH@.
--
-- Finally, we can compose these together to make a function that works for all
-- three cases:
--
-- >>> :{
--   let gitHashValSrc :: Code Q OsString
--       gitHashValSrc =
--         qToCode
--           . projectStringUnknown
--           $ firstSuccessQ
--             (embedGitError gitHashQ)
--             [ embedEnvLookupError $ envValQ [osstr|EXAMPLE_HASH|],
--               runGitInEnvDirQ [osstr|EXAMPLE_HOME|] gitHashQ
--             ]
-- :}
--
-- A final note on laziness: As alluded to above, Q's default semigroup
-- instance is not lazy enough:
--
-- >>> :{
--   $$( qToCode $
--       (runIO (putStrLn "in q1") $> (Right "q1") :: Q (Either () String))
--         <> (runIO (putStrLn "in q2") $> Left ())
--     )
-- :}
-- in q1
-- in q2
-- Right "q1"
--
-- For this reason, we introduce the 'QFirst' newtype:
--
-- >>> :{
--   $$( qToCode $ unQFirst $
--       (mkQFirst $ runIO (putStrLn "in q1") $> (Right "q1") :: QFirst () String)
--         <> (mkQFirst $ runIO (putStrLn "in q2") $> Left ())
--     )
-- :}
-- in q1
-- Right "q1"
--
-- The convenience function
--
-- @
--   firstSuccessQ :: Q (Either e a) -> [Q (Either e a)] -> Q (Either (Exceptions e) a)
-- @
--
-- utilizes 'QFirst' for sequencing a series of Q actions, stopping after the
-- first success.

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
gitHash :: Code Q OsString
gitHash = qToCode $ Utils.projectStringUnknown Git.gitHashQ

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
gitShortHash :: Code Q OsString
gitShortHash = qToCode $ Utils.projectStringUnknown Git.gitShortHashQ

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
gitBranch :: Code Q OsString
gitBranch = qToCode $ Utils.projectStringUnknown Git.gitBranchQ

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
gitDescribe :: Code Q OsString
gitDescribe = qToCode $ Utils.projectStringUnknown Git.gitDescribeQ

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
gitDirty = qToCode $ Utils.projectFalse Git.gitDirtyQ

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
gitDirtyTracked = qToCode $ Utils.projectFalse Git.gitDirtyTrackedQ

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
gitCommitCount :: Code Q OsString
gitCommitCount = qToCode $ Utils.projectStringUnknown Git.gitCommitCountQ

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
gitCommitDate :: Code Q OsString
gitCommitDate = qToCode $ Utils.projectStringUnknown Git.gitCommitDateQ

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- > λ. $$(gitDiff)
-- > "Mon Apr 14 22:14:44 2025 +1200"
--
-- >>> $$(gitDiff)
-- ...
gitDiff :: Code Q OsString
gitDiff = qToCode $ Utils.projectStringUnknown Git.gitDiffQ

-- | Return the hash of the current tree.
--
-- ==== __Examples__
--
-- > λ. $$(gitTree)
-- > "Mon Apr 14 22:14:44 2025 +1200"
--
-- >>> $$(gitTree)
-- ...
gitTree :: Code Q OsString
gitTree = qToCode $ Utils.projectStringUnknown Git.gitTreeQ

-- | Lifts a 'Q' computation to 'Code', for usage with typed TH.
--
-- @since 2.0
qToCode :: forall a. (Lift a) => Q a -> Code Q a
qToCode = TH.liftCode . fmap TExp . (>>= lift)
